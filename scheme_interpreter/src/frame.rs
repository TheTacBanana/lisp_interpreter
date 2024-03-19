use std::collections::HashMap;

use crate::{object::StackObject, ObjectPointer};

pub struct Frame {
    name: String,
    stack_index: usize,
    ident_mapping: HashMap<String, usize>,
    locals: Vec<Option<StackObject>>,
}

impl Frame {
    pub fn new(stack_index: usize, name: String) -> Self {
        Self {
            name,
            stack_index,
            ident_mapping: HashMap::new(),
            locals: Vec::new(),
        }
    }

    pub fn get_local_by_index(&self, index: usize) -> Option<&StackObject> {
        self.locals.get(index).and_then(|p| p.as_ref())
    }

    pub fn get_local(&self, ident: &str) -> Option<&StackObject> {
        self.ident_mapping
            .get(ident)
            .and_then(|i| self.locals.get(*i).and_then(|c| c.as_ref()))
    }

    pub fn get_local_ptr(&self, ident: &str) -> Option<ObjectPointer> {
        self.ident_mapping
            .get(ident)
            .map(|i| ObjectPointer::Stack(self.stack_index, *i))
    }

    pub fn insert_local(&mut self, ident: &str, obj: StackObject) -> ObjectPointer {
        let id = self
            .locals
            .iter()
            .enumerate()
            .find_map(|(i, o)| o.is_none().then(|| i))
            .unwrap_or_else(|| self.locals.len());
        if id >= self.locals.len() {
            let extend = (self.locals.len()..=id + 1).into_iter().map(|_| None);
            self.locals.extend(extend);
        }
        self.ident_mapping.insert(ident.to_string(), id);
        let _ = self.locals.get_mut(id).as_mut().unwrap().insert(obj);
        ObjectPointer::Stack(self.stack_index, id)
    }
}
