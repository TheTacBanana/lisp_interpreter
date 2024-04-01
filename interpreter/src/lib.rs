#![feature(let_chains)]
#![feature(mapped_lock_guards)]

use std::{
    collections::HashMap,
    env::var,
    error::Error,
    ops::Deref,
    sync::{Arc, Mutex, RwLock},
    thread::JoinHandle,
};

use alloc::{InterpreterHeapAlloc, InterpreterStackAlloc};
use core::{
    error::{ErrorWriter, FormattedError},
    parser::ast::AST,
    token::span::Span,
};
use deref::InterpreterDeref;
use error::{InterpreterError, InterpreterErrorKind};
use frame::Frame;
use heap::InterpreterHeap;
use object::{HeapObject, ObjectPointer, ObjectRef, StackObject};

use crate::func::{Func, NativeFunc};

pub mod alloc;
pub mod deref;
pub mod error;
pub mod frame;
pub mod func;
pub mod heap;
pub mod object;
pub mod std_lib;

pub type InterpreterResult<T> = Result<T, InterpreterError>;

pub struct InterpreterContext {
    pub error_writer: ErrorWriter,

    pub frame_stack: Vec<Frame>,
    pub data_stack: Vec<StackObject>,

    pub ident_mapping: HashMap<String, ObjectPointer>,
    pub heap: Arc<InterpreterHeap>,
    pub gc_thread: JoinHandle<()>,
}

impl InterpreterContext {
    pub fn new(error_writer: ErrorWriter) -> Self {
        let (heap, gc) = InterpreterHeap::new();

        let mut s = Self {
            error_writer,
            frame_stack: Vec::new(),
            data_stack: Vec::new(),
            ident_mapping: HashMap::new(),
            heap,
            gc_thread: gc.spawn_thread(),
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

        // alloc_func(self, Func::Native("+".into(), std_lib::add));
        // alloc_func(self, Func::Native("-".into(), std_lib::sub));
        // alloc_func(self, Func::Native("*".into(), std_lib::mul));
        // alloc_func(self, Func::Native("/".into(), std_lib::div));

        // alloc_func(self, Func::Native("eq".into(), std_lib::eq));
        // alloc_func(self, Func::Native("<".into(), std_lib::lt));
        // alloc_func(self, Func::Native("<=".into(), std_lib::lteq));
        // alloc_func(self, Func::Native(">".into(), std_lib::gt));
        // alloc_func(self, Func::Native(">=".into(), std_lib::gteq));
    }

    pub fn stack_trace(&self) {
        println!("Stack Trace:");
        for s in self.frame_stack.iter() {
            println!("{s}")
        }
    }

    pub fn interpret(&mut self, ast: &AST) -> InterpreterResult<()> {
        match ast {
            AST::Operation(op, params, _) => {
                return self.interpret_operation(op, params.iter().collect())
            }
            AST::Identifier(ident, span) => {
                let p = self.resolve_identifier(ident, *span)?;
                self.push_data(StackObject::Ref(p));
            }
            AST::Literal(lit, _) => self.push_data(StackObject::Value(*lit)),
            AST::EmptyList(_) => self.push_data(StackObject::Ref(ObjectPointer::Null)),
            AST::StringLiteral(s, _) => {
                let p = HeapObject::String(s.clone()).stack_alloc(self)?;
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
        let (pointer, span) = match op {
            AST::Identifier(ident, span) => (self.resolve_identifier(ident, *span)?, *span),
            AST::Operation(inner_op, inner_body, span) => {
                self.interpret_operation(inner_op, inner_body.iter().collect())?;
                match self.pop_data()? {
                    StackObject::Ref(p) => (p, *span),
                    StackObject::Value(v) => {
                        return Err(InterpreterError::spanned(
                            InterpreterErrorKind::CannotCall(v.to_string()),
                            *span,
                        ))
                    }
                }
            }
            v => {
                return Err(InterpreterError::spanned(
                    InterpreterErrorKind::CannotCall(v.to_string()),
                    v.span(),
                ))
            }
        };

        let func = {
            let ObjectRef::Object(lock) = pointer.deref(self)? else {
                return Err(InterpreterError::spanned(
                    InterpreterErrorKind::CannotCall(pointer.deref(self).unwrap().to_string()),
                    span,
                ));
            };
            let HeapObject::Func(f) = lock.deref() else {
                return Err(InterpreterError::spanned(
                    InterpreterErrorKind::CannotCall(lock.deref().to_string()),
                    span,
                ));
            };
            f as *const Func
        };

        println!("{func:?}");

        let func_name = unsafe { func.as_ref().unwrap().to_string() };
        let frame = Frame::new(self.frame_stack.len(), func_name);
        self.frame_stack.push(frame);

        let param_count = body.len();
        let mut call_func = || -> InterpreterResult<()> {
            match unsafe { func.as_ref().unwrap() } {
                Func::Native(_, native_func) => {
                    for param in body.drain(..) {
                        self.interpret(param)?
                    }
                    native_func(self, param_count)
                }
                Func::Defined(_, param_names, ast) => {
                    if body.len() != param_names.len() {
                        return Err(InterpreterError::spanned(
                            InterpreterErrorKind::ExpectedNParams {
                                expected: param_names.len(),
                                received: body.len(),
                            },
                            op.span(),
                        ));
                    }

                    for param in body.drain(..) {
                        self.interpret(param)?
                    }

                    let mut params = Vec::new();
                    for _ in 0..param_count {
                        params.push(self.pop_data()?);
                    }
                    params.reverse();

                    let frame = self.top_frame()?;
                    param_names.iter().zip(params).for_each(|(name, obj)| {
                        frame.insert_local(name, obj);
                    });

                    self.interpret(&ast)
                }
                Func::TokenNative(_, native_special_func) => {
                    let params = std::mem::take(&mut body);
                    native_special_func(self, params)
                }
                Func::Macro(_, macro_func) => {
                    let params = std::mem::take(&mut body);
                    let ast = macro_func(self, params)?;
                    self.interpret(unsafe { ast.as_ref().unwrap() })
                }
            }
        };

        println!(":3");
        call_func().map_err(|e| e.add_if_not_spanned(op.span()))?;
        println!(":3");

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
            return Ok(ptr.clone());
        }

        Err(InterpreterError::spanned(
            InterpreterErrorKind::CantResolveIdentifier(ident.to_string()),
            span,
        ))
    }
}
