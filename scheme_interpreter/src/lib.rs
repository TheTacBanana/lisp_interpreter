#![feature(let_chains)]

use std::{
    collections::HashMap, error::Error, sync::{Arc, Mutex}
};

use frame::Frame;
use object::{Func, Object};

pub mod frame;
pub mod object;

pub struct InterpreterContext {
    frame_stack: Vec<Frame>,
    data_stack: Vec<ObjectPointer>,
    global_data: HashMap<String, Object>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ObjectPointer {
    Stack { frame_index: usize, name: String },
    Heap { name: String },
}

impl InterpreterContext {
    pub fn new() -> Self {
        Self {
            data_stack: Vec::new(),
            frame_stack: Vec::new(),
            global_data: HashMap::new(),
        }
    }

    pub fn make_frame(&mut self, pointer: ObjectPointer) -> Result<(), InterpreterError> {
        todo!()
    }

    pub fn pop_frame(&mut self) -> Result<(), InterpreterError> {
        todo!()
    }

    pub fn resolve_identifier(&self, ident: &str) -> Result<ObjectPointer, InterpreterError> {
        if let Some(index) = self
            .frame_stack
            .iter()
            .enumerate()
            .rev()
            .find_map(|(i, frame)| frame.get_local(ident).map(|_| i))
        {
            return Ok(ObjectPointer::Stack { frame_index: index, name: ident.to_string() })
        }

        if self.global_data.get(ident).is_some() {
            return Ok(ObjectPointer::Heap { name: ident.to_string() })
        }

        return Err(InterpreterError::InvalidIdentifier)
    }

    pub fn deref_pointer(&self, pointer: ObjectPointer) -> Result<&Object, InterpreterError> {
        match pointer {
            ObjectPointer::Stack { frame_index, name } => self
                .frame_stack
                .get(frame_index)
                .and_then(|frame| frame.get_local(&name))
                .ok_or(InterpreterError::NullDeref),
            ObjectPointer::Heap { name } => self
                .global_data
                .get(&name)
                .ok_or(InterpreterError::NullDeref),
        }
    }

    pub fn deref_pointer_mut(
        &mut self,
        pointer: ObjectPointer,
    ) -> Result<&mut Object, InterpreterError> {
        match pointer {
            ObjectPointer::Stack { frame_index, name } => self
                .frame_stack
                .get_mut(frame_index)
                .and_then(|frame| frame.get_local_mut(&name))
                .ok_or(InterpreterError::NullDeref),
            ObjectPointer::Heap { name } => self
                .global_data
                .get_mut(&name)
                .ok_or(InterpreterError::NullDeref),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterpreterError {
    NullDeref,
    PointerIsNotFn,
    EmptyStack,
    InvalidIdentifier,
}
