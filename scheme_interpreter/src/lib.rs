#![feature(let_chains)]

use std::{cell::RefCell, collections::HashMap, error::Error, path::PathBuf};

use alloc::{InterpreterHeapAlloc, InterpreterStackAlloc};
use deref::InterpreterDeref;
use frame::Frame;
use object::{HeapObject, ObjectPointer, ObjectRef, StackObject, UnallocatedObject};
use scheme_core::{
    error::{ErrorWriter, FormattedError},
    file::SchemeFile,
    parser::ast::AST,
    token::span::{Span, TotalSpan},
};

use crate::func::Func;

pub mod alloc;
pub mod deref;
pub mod frame;
pub mod func;
pub mod object;
pub mod std_lib;

pub type InterpreterResult<T> = Result<T, InterpreterError>;

pub struct InterpreterContext {
    pub error_writer: ErrorWriter,

    pub frame_stack: Vec<Frame>,
    pub data_stack: Vec<StackObject>,

    pub ident_mapping: HashMap<String, ObjectPointer>,
    pub heap: Vec<Option<HeapObject>>,
}

impl InterpreterContext {
    pub fn new(error_writer: ErrorWriter) -> Self {
        let mut s = Self {
            error_writer,
            frame_stack: Vec::new(),
            data_stack: Vec::new(),
            ident_mapping: HashMap::new(),
            heap: Vec::new(),
        };
        s.with_std();
        s
    }

    pub fn start(&mut self, ast: Vec<AST>) {
        for node in ast {
            if let Err(err) = self.interpret(&node) {
                let _ = self.error_writer.report_errors(vec![err]);
                // self.stack_trace();
                // self.heap_dump();
                break;
            }
        }
    }

    pub fn with_std(&mut self) {
        fn alloc_func(int: &mut InterpreterContext, f: Func) {
            let name = match &f {
                Func::TokenNative(s, _)
                | Func::Macro(s, _)
                | Func::Defined(Some(s), _, _)
                | Func::Native(s, _) => s,
                _ => panic!(),
            }
            .clone();
            HeapObject::Func(f).heap_alloc_named(&name, int).unwrap();
        }

        alloc_func(self, Func::TokenNative("import".into(), std_lib::import));

        alloc_func(
            self,
            Func::Native("stack-trace".into(), std_lib::stack_trace),
        );
        alloc_func(self, Func::Native("heap-dump".into(), std_lib::heap_dump));

        alloc_func(self, Func::TokenNative("define".into(), std_lib::define));
        alloc_func(self, Func::TokenNative("lambda".into(), std_lib::lambda));

        alloc_func(self, Func::Macro("if".into(), std_lib::if_macro));

        alloc_func(self, Func::Native("car".into(), std_lib::car));
        alloc_func(self, Func::Native("cdr".into(), std_lib::cdr));

        alloc_func(self, Func::Native("write".into(), std_lib::write));

        alloc_func(self, Func::Native("+".into(), std_lib::add));
        alloc_func(self, Func::Native("-".into(), std_lib::sub));
        alloc_func(self, Func::Native("*".into(), std_lib::mul));
        alloc_func(self, Func::Native("/".into(), std_lib::div));

        alloc_func(self, Func::Native("eq".into(), std_lib::eq));
        alloc_func(self, Func::Native("<".into(), std_lib::lt));
        alloc_func(self, Func::Native("<=".into(), std_lib::lteq));
        alloc_func(self, Func::Native(">".into(), std_lib::gt));
        alloc_func(self, Func::Native(">=".into(), std_lib::gteq));
    }

    pub fn stack_trace(&self) {
        println!("Stack Trace:");
        for s in self.frame_stack.iter() {
            println!("{s}")
        }
    }

    pub fn heap_dump(&self) {
        println!("Heap Dump:");
        for (i, o) in self
            .heap
            .iter()
            .enumerate()
            .filter_map(|(i, o)| o.as_ref().map(|o| (i, o)))
        {
            if let Ok(item) = o.deref(self) {
                println!("[{i}] {}", item)
            } else {
                println!("[{i}] Deref Failed {o:?}")
            }
        }
        println!()
    }

    pub fn interpret(&mut self, ast: &AST) -> InterpreterResult<()> {
        match ast {
            AST::Operation(op, params, _) => {
                return self.interpret_operation(op, params.iter().collect())
            }
            AST::Identifier(ident, span) => {
                let p = self.resolve_identifier(&ident, *span)?;
                self.push_data(StackObject::Ref(p));
            }
            AST::Literal(lit, _) => self.push_data(StackObject::Value(*lit)),
            AST::EmptyList(_) => self.push_data(StackObject::Ref(ObjectPointer::Null)),
            AST::StringLiteral(s, _) => {
                let p = UnallocatedObject::String(s.clone()).stack_alloc(self)?;
                self.push_data(p)
            }
            AST::List(head, tail, _) => {
                self.interpret(head)?;
                let head = self.pop_data()?.heap_alloc(self)?;
                self.interpret(tail)?;
                let tail = self.pop_data()?.heap_alloc(self)?;
                let pointer = HeapObject::List(head, tail).stack_alloc(self)?;
                self.push_data(pointer)
            }
        }
        Ok(())
    }

    pub fn interpret_operation(&mut self, op: &AST, mut body: Vec<&AST>) -> InterpreterResult<()> {
        let (pointer, span) = if let AST::Identifier(ident, span) = op {
            (self.resolve_identifier(&ident, *span)?, *span)
        } else if let AST::Operation(inner_op, inner_body, span) = op {
            self.interpret_operation(inner_op, inner_body.iter().collect())?;
            match self.pop_data()? {
                StackObject::Ref(p) => (p, *span),
                StackObject::Value(v) => return Err(InterpreterError::spanned(
                    InterpreterErrorKind::CannotCall(v.to_string()),
                    *span,
                )),
            }
        } else {
            return Err(InterpreterError::new(
                InterpreterErrorKind::InvalidOperator(op.clone()),
            ));
        };

        let deref_pointer = pointer.deref(self)?;
        let ObjectRef::Func(func) = deref_pointer else {
            return Err(InterpreterError::spanned(
                InterpreterErrorKind::CannotCall(deref_pointer.to_string()),
                span,
            ));
        };

        let func_name = func.to_string();
        let func: *const Func = func;
        let frame = Frame::new(self.frame_stack.len(), func_name);
        self.frame_stack.push(frame);

        let param_count = body.len();
        match unsafe { func.as_ref().unwrap() } {
            Func::Native(_, native_func) => {
                for param in body.drain(..) {
                    self.interpret(&param)?
                }
                native_func(self, param_count)?
            }
            Func::Defined(_, param_names, ast) => {
                if body.len() != param_names.len() {
                    return Err(InterpreterError::spanned(
                        InterpreterErrorKind::OperationExpectedNParams {
                            expected: param_names.len(),
                            received: body.len(),
                        },
                        op.span(),
                    ));
                }

                for param in body.drain(..) {
                    self.interpret(&param)?
                }

                let mut params = Vec::new();
                for _ in 0..param_count {
                    params.push(self.pop_data()?);
                }
                params.reverse();

                let frame = self.top_frame()?;
                param_names.iter().zip(params).for_each(|(name, obj)| {
                    frame.insert_local(&name, obj);
                });

                self.interpret(ast)?
            }
            Func::TokenNative(_, native_special_func) => {
                let params = body.drain(..).collect::<Vec<_>>();
                native_special_func(self, params)?;
            }
            Func::Macro(_, macro_func) => {
                let params = body.drain(..).collect::<Vec<_>>();
                let ast = macro_func(self, params)?;
                self.interpret(unsafe { ast.as_ref().unwrap() })?;
            }
        }
        self.pop_frame()?;

        Ok(())
    }

    pub fn pop_frame(&mut self) -> InterpreterResult<()> {
        if self.frame_stack.pop().is_none() {
            Err(InterpreterError::new(InterpreterErrorKind::EmptyStack))
        } else {
            Ok(())
        }
    }

    pub fn top_frame(&mut self) -> InterpreterResult<&mut Frame> {
        self.frame_stack
            .last_mut()
            .ok_or(InterpreterError::new(InterpreterErrorKind::EmptyStack))
    }

    pub fn push_data(&mut self, obj: StackObject) {
        self.data_stack.push(obj)
    }

    pub fn pop_data(&mut self) -> InterpreterResult<StackObject> {
        self.data_stack
            .pop()
            .ok_or(InterpreterError::new(InterpreterErrorKind::EmptyDataStack))
    }

    pub fn resolve_identifier(&self, ident: &str, span: Span) -> InterpreterResult<ObjectPointer> {
        if let Some(ptr) = self
            .frame_stack
            .iter()
            .rev()
            .find_map(|frame| frame.get_local_ptr(ident))
        {
            return Ok(ptr);
        }

        if let Some(ptr) = self.ident_mapping.get(ident) {
            return Ok(*ptr);
        }

        return Err(InterpreterError::spanned(
            InterpreterErrorKind::InvalidIdentifier(ident.to_string()),
            span,
        ));
    }
}

#[derive(Debug)]
pub struct InterpreterError {
    span: Option<Span>,
    kind: InterpreterErrorKind,
}

impl std::fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {}", self.span, self.kind)
    }
}

impl Error for InterpreterError {}

impl InterpreterError {
    pub fn new(kind: InterpreterErrorKind) -> Self {
        Self { span: None, kind }
    }

    pub fn optional_span(kind: InterpreterErrorKind, span: Option<Span>) -> Self {
        Self { span, kind }
    }

    pub fn spanned(kind: InterpreterErrorKind, span: Span) -> Self {
        Self {
            span: Some(span),
            kind,
        }
    }

    pub fn add_if_not_spanned(mut self, span: Span) -> Self {
        self.span.get_or_insert(span);
        self
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InterpreterErrorKind {
    // Identifier Errors
    InvalidIdentifier(String),
    InvalidOperator(AST),
    CannotCall(String),
    OperationExpectedNParams { expected: usize, received: usize },

    NullDeref,
    EmptyStack,
    EmptyDataStack,
    ExpectedResult,
    StackIndexOutOfRange,
    PointerDoesNotExist,
    FailedOperation,
    CannotAllocateNull,
    ExpectedList,
    InvalidFuncParamNames,

    // Import Errors
    EmptyImport,
    InvalidInImport,
    ImportNotFound(String),
    ErrorInParsingImport,
}

impl FormattedError for InterpreterError {
    fn fmt_err(&self, ew: &ErrorWriter) -> std::fmt::Result {
        if let Some(total_span) = self.span {
            if let Some(file_link) = ew.link_file(total_span) {
                println!("Error: {} at {}", self.kind, file_link);
            } else {
                println!("Error: {}", self.kind)
            }
            for span in ew.span_to_lines(total_span).unwrap() {
                println!("{}", ew.get_line(span.file_id, span.start.line).unwrap());
                println!("{}", ErrorWriter::underline_span(span));
            }
        } else {
        }
        Ok(())
    }
}

impl std::fmt::Display for InterpreterErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let temp;
        let s = match self {
            InterpreterErrorKind::NullDeref => "Null Pointer Dereferenced",
            InterpreterErrorKind::CannotCall(s) => {
                temp = format!("Cannot call '{s}', it is not a function");
                &temp
            }
            InterpreterErrorKind::InvalidIdentifier(s) => {
                temp = format!("{s} is not a known identifier");
                &temp
            }
            InterpreterErrorKind::EmptyStack => "Stack is empty, cannot pop Stack frame",
            InterpreterErrorKind::EmptyDataStack => "Data Stack is empty, cannot pop Data Stack",
            InterpreterErrorKind::InvalidOperator(op) => {
                temp = format!("{op} is not an operator");
                &temp
            }
            InterpreterErrorKind::ExpectedResult => "Expected Result?", //TODO:
            InterpreterErrorKind::StackIndexOutOfRange => "Pointer exceeds limit of stack",
            InterpreterErrorKind::PointerDoesNotExist => "Pointer does not.", //TODO:
            InterpreterErrorKind::FailedOperation => "Operation arguments not valid",
            InterpreterErrorKind::CannotAllocateNull => {
                "Cannot allocate whatever the fuck this is to the heap" //TODO:
            }
            InterpreterErrorKind::ExpectedList => "Operation expected a List",
            InterpreterErrorKind::InvalidFuncParamNames => "Invalid Param names",
            InterpreterErrorKind::OperationExpectedNParams { expected, received } => {
                temp = format!("Operation expected {expected} parameters received {received}");
                &temp
            }
            InterpreterErrorKind::EmptyImport => "Import is empty",
            InterpreterErrorKind::InvalidInImport => "Invalid in import",
            InterpreterErrorKind::ImportNotFound(s) => {
                temp = format!("Import '{s}' cannot be found");
                &temp
            }
            InterpreterErrorKind::ErrorInParsingImport => "Parse error in import",
        };
        write!(f, "{s}")
    }
}

impl Error for InterpreterErrorKind {}
