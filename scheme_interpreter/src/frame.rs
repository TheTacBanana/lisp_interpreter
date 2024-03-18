use std::collections::HashMap;

use crate::{object::{Func, Object}, InterpreterContext, ObjectPointer};

pub struct Frame {
    name: String,
    stack_index: usize,
    locals: HashMap<String, ObjectPointer>,
}

impl Frame {
    pub fn new(stack_index: usize, name: String) -> Self {
        Self {
            name,
            stack_index,
            locals: HashMap::new(),
        }
    }

    pub fn get_local(&self, ident: &str) -> Option<&ObjectPointer> {
        self.locals.get(ident)
    }

    pub fn get_local_mut(&mut self, ident: &str) -> Option<&mut ObjectPointer> {
        self.locals.get_mut(ident)
    }

    pub fn insert_local(&mut self, ident: &str, obj: ObjectPointer) {
        self.locals.insert(ident.to_string(), obj);
    }
}
