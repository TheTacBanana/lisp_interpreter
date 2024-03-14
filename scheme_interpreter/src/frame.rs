use std::collections::{HashMap, VecDeque};

use scheme_core::{parser::{ast::AST, token::Literal}, token::span::Span};

use crate::symbol::{FunctionCall, Symbol};

pub struct StackFrame {
    pub items: HashMap<&'static str, Symbol>,
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
        new
    }


    pub fn get(&self, symbol: &str) -> Option<&Symbol> {
        self.items.get(symbol)
    }

    pub fn get_mut(&mut self, symbol: &str) -> Option<&mut Symbol> {
        self.items.get_mut(symbol)
    }

    pub fn add_item(&mut self, symbol: &'static str, value: Symbol) {
        self.items.insert(symbol, value);
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
            // (Literal::String(s1), Literal::String(s)) => todo!(),
            // (Literal::String(_), Literal::Character(_)) => todo!(),
            // (Literal::String(_), Literal::Numeric(_)) => todo!(),
            // (Literal::String(_), Literal::Boolean(_)) => todo!(),
            // (Literal::Character(_), Literal::String(_)) => todo!(),
            // (Literal::Character(_), Literal::Character(_)) => todo!(),
            // (Literal::Character(_), Literal::Numeric(_)) => todo!(),
            // (Literal::Character(_), Literal::Boolean(_)) => todo!(),
            // (Literal::Numeric(_), Literal::String(_)) => todo!(),
            // (Literal::Numeric(_), Literal::Character(_)) => todo!(),
            // (Literal::Numeric(_), Literal::Boolean(_)) => todo!(),
            // (Literal::Boolean(_), Literal::String(_)) => todo!(),
            // (Literal::Boolean(_), Literal::Character(_)) => todo!(),
            // (Literal::Boolean(_), Literal::Numeric(_)) => todo!(),
            // (Literal::Boolean(_), Literal::Boolean(_)) => todo!(),
        }
    });
    Symbol::Value(out.clone())
}