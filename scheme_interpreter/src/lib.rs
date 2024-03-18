#![feature(let_chains)]

use std::{collections::HashMap, error::Error};

use frame::Frame;
use object::{Func, Object, ObjectPointer};
use scheme_core::parser::ast::AST;

pub mod frame;
pub mod object;
pub mod std_lib;

pub type InterpreterResult<T> = Result<T, InterpreterError>;

pub struct InterpreterContext {
    frame_stack: Vec<Frame>,
    data_stack: Vec<ObjectPointer>,
    heap: HashMap<String, Object>,
    next_alloc_id: usize,
}

impl InterpreterContext {
    pub fn new() -> Self {
        Self {
            data_stack: Vec::new(),
            frame_stack: Vec::new(),
            heap: HashMap::new(),
            next_alloc_id: 0,
        }
    }

    pub fn with_std(&mut self) {
        self.heap.insert(
            "+".into(),
            Object::Func(Func::Native("+".into(), std_lib::add)),
        );
    }

    pub fn interpret(&mut self, ast: &AST) -> InterpreterResult<()> {
        match ast {
            AST::Operation(op, params) => {
                return self.interpret_operation(op, params.iter().collect())
            }
            AST::Identifier(ident) => {
                self.push_data(self.resolve_identifier(&ident)?);
            }
            AST::Literal(lit) => {
                let p = self.allocate_object(None, Object::Value(lit.clone()));
                self.push_data(p);
            }
            AST::List(head, tail) => {
                todo!()
                // self.interpret(head)?;
                // let ObjectPointer::Object(head) = self.pop_data()? else {
                //     panic!()
                // };

                // if **tail == AST::EmptyList {
                //     self.push_data(ObjectPointer::Object(Object::List(
                //         Box::new(head),
                //         Box::new(Object::Bottom),
                //     )))
                // } else {
                //     self.interpret(tail)?;
                //     let ObjectPointer::Object(tail) = self.pop_data()? else {
                //         panic!()
                //     };
                //     self.push_data(ObjectPointer::Object(Object::List(
                //         Box::new(head),
                //         Box::new(tail),
                //     )));
                // }
            }
            AST::EmptyList => self.push_data(ObjectPointer::Null),
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
                        self.allocate_object(Some(&ident), self.deref_pointer(p)?.clone());
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

                        self.allocate_object(
                            Some(&op_name),
                            Object::Func(Func::Defined(
                                Some(op_name.clone()),
                                param_names,
                                body.next().unwrap().clone(),
                            )),
                        );
                    }
                    e => return Err(InterpreterError::InvalidOperator((*e).clone())),
                };
                assert!(body.len() == 0);
            }
            AST::Identifier(ident) => {
                let pointer = self.resolve_identifier(&ident)?;

                let Object::Func(func) = self.deref_pointer(pointer)? else {
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
                        param_names
                            .iter()
                            .zip(params)
                            .for_each(|(name, obj)| frame.insert_local(&name, obj));

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

        if self.heap.get(ident).is_some() {
            return Ok(ObjectPointer::Heap {
                name: ident.to_string(),
            });
        }

        return Err(InterpreterError::InvalidIdentifier);
    }
    pub fn allocate_object(&mut self, ident: Option<&str>, obj: Object) -> ObjectPointer {
        let id = ident.map(|s| s.to_string()).unwrap_or_else(|| {
            let t = self.next_alloc_id.to_string();
            self.next_alloc_id += 1;
            t
        });
        self.heap.insert(id.clone(), obj);
        ObjectPointer::Heap { name: id }
    }

    pub fn deref_pointer(&self, pointer: ObjectPointer) -> Result<&Object, InterpreterError> {
        match pointer {
            ObjectPointer::Stack { frame_index, name } => self
                .frame_stack
                .get(frame_index)
                .and_then(|frame| frame.get_local(&name))
                .map(|p| self.deref_pointer(p.clone()).unwrap()) // TODO: make less bad
                .ok_or(InterpreterError::NullDeref),
            ObjectPointer::Heap { name } => self.heap.get(&name).ok_or(InterpreterError::NullDeref),
            ObjectPointer::Null => Err(InterpreterError::NullDeref),
        }
    }

    pub fn deref_pointer_mut(
        &mut self,
        pointer: ObjectPointer,
    ) -> Result<&mut Object, InterpreterError> {
        match pointer {
            ObjectPointer::Stack { frame_index, name } => {
                let pointer = self
                    .frame_stack
                    .get_mut(frame_index)
                    .and_then(|frame| frame.get_local(&name))
                    .ok_or(InterpreterError::NullDeref)?
                    .clone();

                self.deref_pointer_mut(pointer) // TODO: make less bad)
            }
            ObjectPointer::Heap { name } => {
                self.heap.get_mut(&name).ok_or(InterpreterError::NullDeref)
            }
            ObjectPointer::Null => Err(InterpreterError::NullDeref),
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

impl std::fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Error for InterpreterError {}
