#![feature(let_chains)]

use std::{
    collections::HashMap,
    error::Error,
    sync::{Arc, Mutex},
};

use frame::Frame;
use object::{Func, Object, ObjectPointer};
use scheme_core::parser::ast::AST;

pub mod frame;
pub mod object;

pub type InterpreterResult<T> = Result<T, InterpreterError>;

pub struct InterpreterContext {
    frame_stack: Vec<Frame>,
    data_stack: Vec<ObjectPointer>,
    global_data: HashMap<String, Object>,
}

impl InterpreterContext {
    pub fn new() -> Self {
        Self {
            data_stack: Vec::new(),
            frame_stack: Vec::new(),
            global_data: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, ast: AST) -> InterpreterResult<()> {
        match ast {
            AST::Operation(op, params) => return self.interpret_operation(*op, params),
            AST::Identifier(ident) => {
                self.push_data(self.resolve_identifier(&ident)?);
            }
            AST::Literal(lit) => {
                self.push_data(ObjectPointer::Object(Object::Value(lit)));
            }
            AST::List(head, tail) => {
                self.interpret(*head);
                let ObjectPointer::Object(head) = self.pop_data()? else {
                    panic!()
                };

                if *tail == AST::EmptyList {
                    self.push_data(ObjectPointer::Object(Object::List(
                        Box::new(head),
                        Box::new(Object::Bottom),
                    )))
                } else {
                    self.interpret(*tail)?;
                    let ObjectPointer::Object(tail) = self.pop_data()? else {
                        panic!()
                    };
                    self.push_data(ObjectPointer::Object(Object::List(
                        Box::new(head),
                        Box::new(tail),
                    )));
                }
            }
            AST::EmptyList => self.push_data(ObjectPointer::Object(Object::Bottom)),
        }
        Ok(())
    }

    pub fn interpret_operation(&mut self, op: AST, mut body: Vec<AST>) -> InterpreterResult<()> {
        match op {
            AST::Identifier(i) if &i == "define" => {
                let mut body = body.drain(..);
                let (ident, object) = match body.next().unwrap() {
                    // Define a value
                    AST::Identifier(ident) => {
                        self.interpret(body.next().unwrap())?;
                        let ObjectPointer::Object(object) = self.pop_data()? else {
                            return Err(InterpreterError::ExpectedResult);
                        };
                        (ident, object)
                    }
                    // Define a function
                    AST::Operation(op_name, mut op_params) => {
                        let AST::Identifier(op_name) = *op_name else {
                            panic!("Expected Identifier {op_name}")
                        };

                        let mut param_names = Vec::new();
                        for p in op_params.drain(..) {
                            match p {
                                AST::Identifier(ident) => param_names.push(ident),
                                e => panic!("Expected Identifier received {e}"),
                            }
                        }

                        (
                            op_name,
                            Object::Func(Func::Defined(
                                Some(op_name),
                                param_names,
                                body.next().unwrap(),
                            )),
                        )
                    }

                    e => return Err(InterpreterError::InvalidOperator(e)),
                };
                assert!(body.len() == 0);

                self.global_data.insert(ident, object);
            }
            AST::Identifier(ident) => {
                let pointer = self.resolve_identifier(&ident)?;
                let Object::Func(func) = self.deref_pointer(&pointer)? else {
                    return Err(InterpreterError::PointerIsNotFn);
                };

                match func {
                    Func::Macro(_, _) => todo!(),
                    Func::Native(_, _) => todo!(),
                    Func::Defined(_, _, _) => todo!(),
                }

                let param_count = body.len();
                for param in body.drain(..) {
                    self.interpret(param)?
                }

                let frame = Frame::new(self.frame_stack.len(), func.to_string());
                let mut params = Vec::new();
                for _ in 0..param_count {
                    params.push(self.pop_data()?);
                }
                params.reverse();

                self.frame_stack.push(value);
                match func {
                    Func::Native(_, f) => f(self, param_count),
                    Func::Defined(_, _, _) => {}
                }

                self.pop_frame()?;
            }
            ast => return Err(InterpreterError::InvalidOperator(ast)),
        }

        Ok(())
    }

    // pub fn create_frame(&mut self, func: &Func, params: usize) -> InterpreterResult<()> {
    // let frame = ;
    // match func {
    //     Func::Native(name, _) => {

    //     },
    //     Func::Defined(_, _, _) => {

    //     },
    // }
    // }

    pub fn pop_frame(&mut self) -> InterpreterResult<()> {
        if self.frame_stack.pop().is_none() {
            Err(InterpreterError::EmptyStack)
        } else {
            Ok(())
        }
    }

    pub fn push_data(&mut self, pointer: ObjectPointer) {
        self.data_stack.push(pointer)
    }

    pub fn pop_data(&mut self) -> InterpreterResult<ObjectPointer> {
        self.data_stack
            .pop()
            .ok_or(InterpreterError::EmptyDataStack)
    }

    pub fn resolve_identifier(&self, ident: &str) -> Result<ObjectPointer, InterpreterError> {
        if let Some(index) = self
            .frame_stack
            .iter()
            .enumerate()
            .rev()
            .find_map(|(i, frame)| frame.get_local(ident).map(|_| i))
        {
            return Ok(ObjectPointer::Stack {
                frame_index: index,
                name: ident.to_string(),
            });
        }

        if self.global_data.get(ident).is_some() {
            return Ok(ObjectPointer::Heap {
                name: ident.to_string(),
            });
        }

        return Err(InterpreterError::InvalidIdentifier);
    }

    pub fn deref_pointer(&self, pointer: &ObjectPointer) -> Result<&Object, InterpreterError> {
        match pointer {
            ObjectPointer::Stack { frame_index, name } => self
                .frame_stack
                .get(*frame_index)
                .and_then(|frame| frame.get_local(&name))
                .ok_or(InterpreterError::NullDeref),
            ObjectPointer::Heap { name } => self
                .global_data
                .get(name)
                .ok_or(InterpreterError::NullDeref),
            ObjectPointer::Object(object) => Ok(object),
        }
    }

    pub fn deref_pointer_mut(
        &mut self,
        pointer: &mut ObjectPointer,
    ) -> Result<&mut Object, InterpreterError> {
        match pointer {
            ObjectPointer::Stack { frame_index, name } => self
                .frame_stack
                .get_mut(*frame_index)
                .and_then(|frame| frame.get_local_mut(&name))
                .ok_or(InterpreterError::NullDeref),
            ObjectPointer::Heap { name } => self
                .global_data
                .get_mut(name)
                .ok_or(InterpreterError::NullDeref),
            ObjectPointer::Object(object) => Ok(object),
        }
    }
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
}
