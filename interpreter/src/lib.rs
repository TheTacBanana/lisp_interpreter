#![feature(let_chains)]
#![feature(mapped_lock_guards)]

use std::{
    collections::HashMap,
    ops::Deref,
    sync::{Arc, RwLock},
    thread::JoinHandle,
};

use alloc::{InterpreterHeapAlloc, InterpreterStackAlloc};
use core::{error::{AddIfNotSpannedExt, ErrorWriter}, parser::ast::AST, token::span::Span};
use deref::InterpreterDeref;
use error::{InterpreterError, InterpreterErrorKind};
use frame::Frame;
use func::Func;
use heap::InterpreterHeap;
use object::{HeapObject, ObjectPointer, ObjectRef, StackObject};
use stack::InterpreterStack;

pub mod alloc;
pub mod comparison;
pub mod deref;
pub mod error;
pub mod frame;
pub mod func;
pub mod heap;
pub mod object;
pub mod print;
pub mod stack;
pub mod std_lib;

pub type InterpreterResult<T> = Result<T, InterpreterError>;

pub struct InterpreterContext {
    pub error_writer: RwLock<ErrorWriter>,

    pub ident_mapping: RwLock<HashMap<String, ObjectPointer>>,

    pub stack: Arc<InterpreterStack>,
    pub heap: Arc<InterpreterHeap>,

    pub gc_thread: JoinHandle<()>,
}

impl InterpreterContext {
    pub fn new(error_writer: ErrorWriter) -> Self {
        let (heap, gc) = InterpreterHeap::new();

        let mut s = Self {
            error_writer: RwLock::new(error_writer),
            ident_mapping: RwLock::new(HashMap::new()),
            heap,
            stack: Arc::new(InterpreterStack::new()),
            gc_thread: gc.spawn_thread(),
        };
        s.with_std();
        s
    }

    pub fn with_std(&mut self) {
        fn alloc_func(int: &InterpreterContext, f: Func) {
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
        alloc_func(self, Func::TokenNative("let".into(), std_lib::let_));

        alloc_func(self, Func::Macro("if".into(), std_lib::if_macro));

        alloc_func(self, Func::Native("car".into(), std_lib::car));
        alloc_func(self, Func::Native("cdr".into(), std_lib::cdr));
        alloc_func(self, Func::Native("cons".into(), std_lib::cons));
        alloc_func(self, Func::Native("empty?".into(), std_lib::empty));

        alloc_func(self, Func::Native("write".into(), std_lib::write));

        alloc_func(self, Func::Native("+".into(), std_lib::add));
        alloc_func(self, Func::Native("-".into(), std_lib::sub));
        alloc_func(self, Func::Native("*".into(), std_lib::mul));
        alloc_func(self, Func::Native("/".into(), std_lib::div));

        alloc_func(self, Func::Native("eq?".into(), std_lib::eq));
        alloc_func(self, Func::Native("==".into(), std_lib::eq));
        alloc_func(self, Func::Native("<".into(), std_lib::lt));
        alloc_func(self, Func::Native("<=".into(), std_lib::lteq));
        alloc_func(self, Func::Native(">".into(), std_lib::gt));
        alloc_func(self, Func::Native(">=".into(), std_lib::gteq));
    }

    pub fn start(&self, ast: Vec<AST>) {
        for node in ast {
            if let Err(err) = self.interpret(&node) {
                let _ = self.error_writer.read().unwrap().report_errors(vec![err]);
                // self.stack_trace();
                // self.heap.dump(self);
                break;
            }
        }
    }

    pub fn interpret(&self, ast: &AST) -> InterpreterResult<()> {
        #[derive(Debug)]
        enum QueueOp<'a> {
            Eval(&'a AST),
            BuildList,

            EvalLiteral(AST),
            PopRefStack,

            PopFuncOp(Span, Vec<&'a AST>),
            CountedParams(usize),
            NamedParams(Vec<String>),
            MacroParams(Vec<&'a AST>),

            PushFrame(Frame),
            PopFrame,
            ApplyFunc(u64, Span),
        }

        let head = ast;
        let mut op_stack = vec![QueueOp::Eval(head)];
        let mut ref_stack = Vec::new();
        let mut func_cache = HashMap::<u64, Func>::new();
        while !op_stack.is_empty() {
            let next = op_stack.pop().unwrap();
            match next {
                QueueOp::EvalLiteral(ast) => {
                    ref_stack.push(ast);
                    op_stack.push(QueueOp::PopRefStack);
                    op_stack.push(QueueOp::Eval(unsafe {
                        {
                            (&*ref_stack.last().unwrap() as *const AST)
                                .as_ref()
                                .unwrap()
                        }
                    }))
                }
                QueueOp::PopRefStack => {
                    ref_stack.pop();
                }
                QueueOp::Eval(ast) => match ast {
                    AST::Identifier(ident, span) => {
                        let p = self.resolve_identifier(ident, *span)?;
                        self.stack.push_data(StackObject::Ref(p));
                    }
                    AST::Literal(lit, _) => self.stack.push_data(StackObject::Value(*lit)),
                    AST::EmptyList(_) => {
                        self.stack.push_data(StackObject::Ref(ObjectPointer::Null))
                    }
                    AST::StringLiteral(s, _) => {
                        let p = HeapObject::String(s.clone()).stack_alloc(self)?;
                        self.stack.push_data(p)
                    }
                    AST::List(head, tail, _) => {
                        op_stack.extend([
                            QueueOp::BuildList,
                            QueueOp::Eval(&head),
                            QueueOp::Eval(&tail),
                        ]);
                    }
                    AST::Operation(op, params, _) => {
                        op_stack.extend([
                            QueueOp::PopFuncOp(op.span(), params.iter().collect()),
                            QueueOp::Eval(op),
                        ]);
                    }
                },
                QueueOp::BuildList => {
                    let head = self.stack.pop_data()?.heap_alloc(self)?;
                    let tail = self.stack.pop_data()?.heap_alloc(self)?;
                    let pointer = HeapObject::List(head, tail).stack_alloc(self)?;
                    self.stack.push_data(pointer);
                }

                QueueOp::PopFuncOp(span, mut params) => {
                    let pointer = match self.stack.pop_data()? {
                        StackObject::Ref(r) => r,
                        StackObject::Value(v) => {
                            return Err(InterpreterError::spanned(
                                InterpreterErrorKind::CannotCall(v.to_string()),
                                span,
                            ))
                        }
                    };

                    let ObjectRef::Object(lock) = pointer.deref(self)? else {
                        return Err(InterpreterError::spanned(
                            InterpreterErrorKind::CannotCall(
                                pointer.deref(self).unwrap().to_string(),
                            ),
                            span,
                        ));
                    };
                    let HeapObject::Func(func) = lock.deref() else {
                        return Err(InterpreterError::spanned(
                            InterpreterErrorKind::CannotCall(lock.deref().to_string()),
                            span,
                        ));
                    };

                    let func_hash = func.calc_hash();
                    func_cache.entry(func_hash).or_insert_with(|| func.clone());
                    let mut new_frame = !self
                        .stack
                        .top_frame()
                        .is_ok_and(|f| f.func_hash == func.calc_hash());

                    let mut new_ops = Vec::new();
                    let param_len = params.len();
                    match func {
                        Func::Defined(_, p, _) => {
                            if p.len() != param_len {
                                return Err(InterpreterError::spanned(
                                    InterpreterErrorKind::ExpectedNParams(p.len(), param_len),
                                    span,
                                ));
                            }
                            new_ops.push(QueueOp::NamedParams(p.clone()));
                            new_ops.push(QueueOp::ApplyFunc(func_hash, span));
                            new_ops.extend(params.drain(..).map(|p| QueueOp::Eval(p)).rev());
                        }
                        Func::Native(_, _) => {
                            new_ops.push(QueueOp::CountedParams(param_len));
                            new_ops.push(QueueOp::ApplyFunc(func_hash, span));
                            new_ops.extend(params.drain(..).map(|p| QueueOp::Eval(p)).rev());
                        }
                        Func::TokenNative(_, _) => {
                            new_ops.push(QueueOp::MacroParams(params));
                            new_ops.push(QueueOp::ApplyFunc(func_hash, span));
                        }
                        Func::Macro(_, _) => {
                            new_ops.push(QueueOp::MacroParams(params));
                            new_ops.push(QueueOp::ApplyFunc(func_hash, span));
                            new_frame = false;
                        }
                    }

                    if new_frame {
                        op_stack.push(QueueOp::PopFrame);
                    }
                    op_stack.extend(new_ops);
                    if new_frame {
                        op_stack.push(QueueOp::PushFrame(Frame::new(
                            self.stack.frame.read().unwrap().len(),
                            func,
                        )));
                    }
                }

                QueueOp::PushFrame(frame) => {
                    self.stack.push_frame(frame);
                }
                QueueOp::PopFrame => {
                    self.stack.pop_frame()?;
                }

                QueueOp::ApplyFunc(func_hash, span) => {
                    let func = func_cache.get(&func_hash).unwrap();

                    let params = op_stack.last().unwrap();
                    match (params, func) {
                        (QueueOp::NamedParams(param_names), Func::Defined(_, _, ast)) => {
                            let mut params = Vec::new();
                            for _ in 0..param_names.len() {
                                params.push(self.stack.pop_data()?.heap_alloc(self)?);
                            }
                            params.reverse();

                            let mut top_frame = self.stack.top_frame()?;
                            param_names.iter().zip(params).for_each(|(name, obj)| {
                                top_frame.insert_local(name, obj);
                            });

                            op_stack.push(QueueOp::EvalLiteral(ast.clone()))
                        }
                        (QueueOp::CountedParams(n), Func::Native(_, native_func)) => {
                            native_func(self, *n).map_not_spanned(span)?
                        }
                        (QueueOp::MacroParams(params), Func::TokenNative(_, token_native)) => {
                            token_native(self, params.to_vec()).map_not_spanned(span)?;
                        }
                        (QueueOp::MacroParams(params), Func::Macro(_, macro_f)) => {
                            let out = macro_f(self, params.to_vec()).map_not_spanned(span)?;
                            op_stack.push(QueueOp::Eval(params[out]));
                        }
                        e => panic!("{e:?}"),
                    };
                }
                QueueOp::CountedParams(_) | QueueOp::MacroParams(_) | QueueOp::NamedParams(_) => (),
            }
        }
        Ok(())
    }

    pub fn resolve_identifier(&self, ident: &str, span: Span) -> InterpreterResult<ObjectPointer> {
        if let Some(ptr) = self
            .stack
            .frame
            .read()
            .unwrap()
            .iter()
            .rev()
            .find_map(|frame| frame.get_local(ident))
        {
            return Ok(ptr);
        }

        if let Some(ptr) = self.ident_mapping.read().unwrap().get(ident) {
            return Ok(ptr.clone());
        }

        Err(InterpreterError::spanned(
            InterpreterErrorKind::CantResolveIdentifier(ident.to_string()),
            span,
        ))
    }
}
