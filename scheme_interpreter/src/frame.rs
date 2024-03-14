use std::collections::{HashMap, VecDeque};

use scheme_core::{parser::{ast::AST, token::Literal}, token::span::Span};

use crate::symbol::{FunctionCall, Symbol};

#[derive(Debug)]
pub struct StackFrame {
    pub items: HashMap<String, Symbol>,
}

impl StackFrame {
    pub fn new() -> Self {
        StackFrame {
            items: HashMap::default(),
        }
    }

    pub fn prelude() -> Self {
        let mut new = Self::new();
        new.add_item("+", Symbol::FunctionCall(FunctionCall::Native(add_def)));
        new.add_item("-", Symbol::FunctionCall(FunctionCall::Native(sub_def)));
        new
    }


    pub fn get(&self, symbol: &str) -> Option<&Symbol> {
        self.items.get(symbol)
    }

    pub fn get_mut(&mut self, symbol: &str) -> Option<&mut Symbol> {
        self.items.get_mut(symbol)
    }

    pub fn add_item(&mut self, symbol: &str, value: Symbol) {
        self.items.insert(symbol.to_string(), value);
    }
}

pub fn add_def(mut vec: Vec<Symbol>) -> Symbol {
    let mut values = vec.drain(..).map(|v| match v {
        Symbol::Value(l) => l,
        _ => panic!("Cannot add non values together")
    }).collect::<VecDeque<_>>();

    let first = values.pop_front().unwrap();
    let out = values.drain(..).fold(first,|l, r| {
        match (l, r) {
            (Literal::Numeric(l), Literal::Numeric(r)) => Literal::Numeric(l + r),
            _ => panic!()
        }
    });
    Symbol::Value(out.clone())
}

pub fn sub_def(mut vec: Vec<Symbol>) -> Symbol {
    let mut values = vec.drain(..).map(|v| match v {
        Symbol::Value(l) => l,
        _ => panic!("Cannot sub non values together")
    }).collect::<VecDeque<_>>();

    let first = values.pop_front().unwrap();
    let out = values.drain(..).fold(first,|l, r| {
        match (l, r) {
            (Literal::Numeric(l), Literal::Numeric(r)) => Literal::Numeric(l - r),
            _ => panic!()
        }
    });
    Symbol::Value(out.clone())
}