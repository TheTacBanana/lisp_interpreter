use std::collections::HashMap;

use crate::{func::Func, ObjectPointer};

#[derive(Debug)]
pub struct Frame {
    pub name: String,
    pub func_hash: u64,
    pub stack_index: usize,
    pub ident_mapping: HashMap<String, usize>,
    pub locals: Vec<Option<ObjectPointer>>,
}

impl Frame {
    pub fn new(stack_index: usize, func: &Func) -> Self {
        Self {
            name: func.to_string(),
            func_hash: func.calc_hash(),
            stack_index,
            ident_mapping: HashMap::new(),
            locals: Vec::new(),
        }
    }

    pub fn get_local_by_index(&self, index: usize) -> Option<ObjectPointer> {
        self.locals.get(index).and_then(|p| p.clone())
    }

    pub fn get_local(&self, ident: &str) -> Option<ObjectPointer> {
        self.ident_mapping
            .get(ident)
            .and_then(|i| self.locals.get(*i).and_then(|c| c.clone()))
    }

    pub fn insert_local(&mut self, ident: &str, pointer: ObjectPointer) -> ObjectPointer{
        let id = self
            .locals
            .iter()
            .enumerate()
            .find_map(|(i, o)| o.is_none().then_some(i))
            .unwrap_or(self.locals.len());
        if id >= self.locals.len() {
            let extend = (self.locals.len()..=id + 1).map(|_| None);
            self.locals.extend(extend);
        }
        self.ident_mapping.insert(ident.to_string(), id);
        let _ = self.locals.get_mut(id).as_mut().unwrap().insert(pointer);
        ObjectPointer::Stack(self.stack_index, id)
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

impl std::fmt::Display for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Frame[{}]: {}", self.stack_index, self.name)?;

        for (ident, i) in self.ident_mapping.iter() {
            writeln!(f, "{ident:?}: {:?}", self.locals.get(*i).unwrap().clone().unwrap())?
        }
        Ok(())
    }
}
