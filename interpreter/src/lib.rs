#![feature(let_chains)]
#![feature(mapped_lock_guards)]

use std::{
    collections::HashMap,
    ops::Deref,
    sync::{Arc, RwLock},
    thread::JoinHandle,
};

use alloc::{InterpreterHeapAlloc, InterpreterStackAlloc};
use core::{error::ErrorWriter, parser::ast::AST, token::span::Span};
use deref::InterpreterDeref;
use error::{InterpreterError, InterpreterErrorKind};
use frame::Frame;
use heap::InterpreterHeap;
use object::{HeapObject, ObjectPointer, ObjectRef, StackObject};
use stack::InterpreterStack;

use crate::{func::Func, stack::FrameRef};

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

    pub fn start(&mut self, ast: Vec<AST>) {
        for node in ast {
            if let Err(err) = self.interpret(&node) {
                let _ = self.error_writer.read().unwrap().report_errors(vec![err]);
                // self.stack_trace();
                // self.heap.dump(self);
                break;
            }
        }
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

    pub fn interpret(&self, ast: &AST) -> InterpreterResult<()> {
        match ast {
            AST::Operation(op, params, _) => {
                return self.interpret_operation(op, params.iter().collect())
            }
            AST::Identifier(ident, span) => {
                let p = self.resolve_identifier(ident, *span)?;
                self.stack.push_data(StackObject::Ref(p));
            }
            AST::Literal(lit, _) => self.stack.push_data(StackObject::Value(*lit)),
            AST::EmptyList(_) => self.stack.push_data(StackObject::Ref(ObjectPointer::Null)),
            AST::StringLiteral(s, _) => {
                let p = HeapObject::String(s.clone()).stack_alloc(self)?;
                self.stack.push_data(p)
            }
            AST::List(head, tail, _) => {
                self.interpret(head)?;
                let head = self.stack.pop_data()?.heap_alloc(self)?;
                self.interpret(tail)?;
                let tail = self.stack.pop_data()?.heap_alloc(self)?;
                let pointer = HeapObject::List(head, tail).stack_alloc(self)?;
                self.stack.push_data(pointer)
            }
        }
        Ok(())
    }

    pub fn interpret_operation(&self, op: &AST, mut body: Vec<&AST>) -> InterpreterResult<()> {
        let (pointer, span) = match op {
            AST::Identifier(ident, span) => (self.resolve_identifier(ident, *span)?, *span),
            AST::Operation(inner_op, inner_body, span) => {
                self.interpret_operation(inner_op, inner_body.iter().collect())?;
                match self.stack.pop_data()? {
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
            let HeapObject::Func(func) = lock.deref() else {
                return Err(InterpreterError::spanned(
                    InterpreterErrorKind::CannotCall(lock.deref().to_string()),
                    span,
                ));
            };
            func.clone()
        };

        let param_count = body.len();
        let mut call_func = || -> InterpreterResult<()> {
            match &func {
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
                        params.push(self.stack.pop_data()?);
                    }
                    params.reverse();

                    let mut tail_call = false;
                    {
                        let mut top_frame = if let Ok(top_frame) = self.stack.top_frame() && top_frame.func == func {
                            tail_call = true;
                            top_frame
                        } else {
                            let frame = Frame::new(self.stack.frame.read().unwrap().len(), func.clone());
                            self.stack.push_frame(frame);
                            self.stack.top_frame()?
                        };

                        param_names.iter().zip(params).for_each(|(name, obj)| {
                            top_frame.insert_local(name, obj);
                        });
<<<<<<< HEAD

                        println!("{}", top_frame.deref())
=======
>>>>>>> bc44dce (Tail call optimization!!!)
                    }

                    let out = self.interpret(&ast);

                    if !tail_call {
                        self.stack.pop_frame()?;
                    }

                    out

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

        call_func().map_err(|e| e.add_if_not_spanned(op.span()))?;

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
            .find_map(|frame| frame.get_local_ptr(ident))
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
