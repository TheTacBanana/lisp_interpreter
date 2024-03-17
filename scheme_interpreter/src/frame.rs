use std::collections::HashMap;

use crate::{object::{Func, Object}, InterpreterContext};

pub struct Frame {
    f_name: String,
    locals: HashMap<String, Object>,
}

impl Frame {
    pub fn new(f_name: String, body: Func) -> Self {
        Self {
            f_name,
            locals: HashMap::new(),
        }
    }

    pub fn get_local(&self, ident: &str) -> Option<&Object> {
        self.locals.get(ident)
    }

    pub fn get_local_mut(&mut self, ident: &str) -> Option<&mut Object> {
        self.locals.get_mut(ident)
    }
}
