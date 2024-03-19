#![feature(let_chains)]

use std::{collections::HashMap, error::Error};

use alloc::InterpreterHeapAlloc;
use deref::InterpreterDeref;
use frame::Frame;
use object::{HeapObject, ObjectPointer, ObjectRef, StackObject};
use scheme_core::parser::ast::AST;

use crate::func::Func;

pub mod alloc;
pub mod deref;
pub mod frame;
pub mod func;
pub mod object;
pub mod std_lib;

pub type InterpreterResult<T> = Result<T, InterpreterError>;

pub struct InterpreterContext {
    frame_stack: Vec<Frame>,
    data_stack: Vec<StackObject>,

    ident_mapping: HashMap<String, ObjectPointer>,
    heap: Vec<Option<HeapObject>>,
}

impl InterpreterContext {
    pub fn new() -> Self {
        Self {
            data_stack: Vec::new(),
            frame_stack: Vec::new(),

            ident_mapping: HashMap::default(),
            heap: Vec::new(),
        }
    }

    pub fn with_std(&mut self) {
        HeapObject::Func(Func::Native("+".into(), std_lib::add)).heap_alloc_named("+", self);
    }

    pub fn interpret(&mut self, ast: &AST) -> InterpreterResult<()> {
        match ast {
            AST::Operation(op, params) => {
                return self.interpret_operation(op, params.iter().collect())
            }
            AST::Identifier(ident) => {
                let p = self.resolve_identifier(&ident)?;
                self.push_data(StackObject::Ref(p));
            }
            AST::Literal(lit) => self.push_data(StackObject::Value(*lit)),
            AST::EmptyList => self.push_data(StackObject::Ref(ObjectPointer::Null)),
            _ => todo!(),
        }
        Ok(())
    }

    pub fn interpret_operation(&mut self, op: &AST, mut body: Vec<&AST>) -> InterpreterResult<()> {
        match op {
            AST::Identifier(i) if i == "define" => {
                let mut body = body.drain(..);
                match body.next().unwrap() {
                    // Define a value
                    AST::Identifier(ident) => {
                        self.interpret(&body.next().unwrap())?;
                        let p = self.pop_data()?;
                        p.heap_alloc_named(ident, self)?;
                    }
                    // Define a function
                    AST::Operation(op_name, op_params) => {
                        let AST::Identifier(op_name) = &**op_name else {
                            panic!("Expected Identifier {op_name}")
                        };

                        let mut param_names = Vec::new();
                        for p in op_params.iter() {
                            match p {
                                AST::Identifier(ident) => param_names.push(ident.clone()),
                                e => panic!("Expected Identifier received {e}"),
                            }
                        }

                        HeapObject::Func(Func::Defined(
                            Some(op_name.clone()),
                            param_names,
                            body.next().unwrap().clone(),
                        ))
                        .heap_alloc_named(&op_name, self)?;
                    }
                    e => return Err(InterpreterError::InvalidOperator((*e).clone())),
                };
                assert!(body.len() == 0);
            }
            AST::Identifier(ident) => {
                let pointer = self.resolve_identifier(&ident)?;

                let ObjectRef::Func(func) = pointer.deref(self)? else {
                    println!("{pointer:?}");
                    return Err(InterpreterError::PointerIsNotFn);
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
                    Func::Macro(_, macro_func) => {
                        let params = body.drain(..).collect::<Vec<_>>();
                        macro_func(self, params)?
                    }
                }
                self.pop_frame()?;
            }
            ast => return Err(InterpreterError::InvalidOperator(ast.clone())),
        }

        Ok(())
    }

    pub fn pop_frame(&mut self) -> InterpreterResult<()> {
        if self.frame_stack.pop().is_none() {
            Err(InterpreterError::EmptyStack)
        } else {
            Ok(())
        }
    }

    pub fn top_frame(&mut self) -> InterpreterResult<&mut Frame> {
        self.frame_stack
            .last_mut()
            .ok_or(InterpreterError::EmptyStack)
    }

    pub fn push_data(&mut self, obj: StackObject) {
        self.data_stack.push(obj)
    }

    pub fn pop_data(&mut self) -> InterpreterResult<StackObject> {
        self.data_stack
            .pop()
            .ok_or(InterpreterError::EmptyDataStack)
    }

    pub fn resolve_identifier(&self, ident: &str) -> Result<ObjectPointer, InterpreterError> {
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

        return Err(InterpreterError::InvalidIdentifier);
    }

    // pub fn deref(&mut self, obj: &dyn InterpreterDeref) -> InterpreterResult<ObjectRef<'_>> {
    //     obj.deref(self)
    // }

    // pub fn alloc_named(
    //     &mut self,
    //     ident: &str,
    //     obj: impl InterpreterHeapAlloc,
    // ) -> InterpreterResult<ObjectPointer> {
    //     obj.heap_alloc_named(ident, self)
    // }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InterpreterError {
    NullDeref,
    PointerIsNotFn,
    InvalidIdentifier,
    EmptyStack,
    EmptyDataStack,
    InvalidOperator(AST),
    ExpectedResult,
    StackIndexOutOfRange,
    PointerDoesNotExist,
    FailedOperation,
}

impl std::fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Error for InterpreterError {}
