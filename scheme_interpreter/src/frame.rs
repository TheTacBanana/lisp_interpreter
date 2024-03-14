use std::collections::{HashMap, VecDeque};

use scheme_core::{
    parser::{ast::AST, token::Literal},
    token::span::Span,
};

use crate::{
    symbol::{FunctionCall, Symbol},
    Interpreter,
};

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
        new.add_item("+", Symbol::FunctionCall(FunctionCall::Native(add_op)));
        new.add_item("-", Symbol::FunctionCall(FunctionCall::Native(sub_op)));
        new.add_item("*", Symbol::FunctionCall(FunctionCall::Native(mul_op)));
        new.add_item("/", Symbol::FunctionCall(FunctionCall::Native(div_op)));
        new.add_item("if", Symbol::FunctionCall(FunctionCall::Native(if_op)));
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

pub fn if_op(interpreter: &mut Interpreter, mut vec: Vec<Symbol>) -> Symbol {
    let mut drain = vec.drain(..);
    let symbol = match drain.next().unwrap() {
        s @ Symbol::Value(_) => s,
        Symbol::Tokens(ast) => interpreter.interpret(ast),
        _ => panic!(),
    };
    let Symbol::Value(lit) = symbol else { panic!() };

    if lit.is_truthy() {
        match drain.next().unwrap() {
            s @ Symbol::Value(_) => s,
            Symbol::Tokens(ast) => interpreter.interpret(ast),
            _ => panic!(),
        }
    } else if let Some(token) = drain.skip(1).next() {
        match token {
            s @ Symbol::Value(_) => s,
            Symbol::Tokens(ast) => interpreter.interpret(ast),
            _ => panic!(),
        }
    } else {
        panic!()
    }
}

macro_rules! binary_op {
    ($name:ident, $l:ident, $r:ident, $op:expr) => {
        pub fn $name(interpreter: &mut Interpreter, mut vec: Vec<Symbol>) -> Symbol {
            let out = vec
                .drain(..)
                .map(|s| match s {
                    Symbol::Value(l) => l,
                    Symbol::Tokens(ast) => {
                        let Symbol::Value(lit) = interpreter.interpret(ast) else {
                            panic!()
                        };
                        lit
                    }
                    _ => panic!(),
                })
                .reduce(|$l, $r| match ($l, $r) {
                    (Literal::Numeric($l), Literal::Numeric($r)) => Literal::Numeric($op),
                    _ => panic!(),
                })
                .unwrap();
            Symbol::Value(out)
        }
    };
}

// pub fn add_op(interpreter: &mut Interpreter,mut vec:Vec<Symbol>) -> Symbol {
//     let out = vec.drain(..).map(|s|match s {
//         Symbol::Value(l) => l,
//         Symbol::Tokens(ast) => {
//             let out = interpreter.interpret(ast);
//             println!("{:?}", out);
//             let Symbol::Value(lit) = out else {
//                 panic!()
//             };
//             lit
//         }
//         _ => panic!(),

//         }).reduce(|l,r|match(l,r){
//         (Literal::Numeric(l),Literal::Numeric(r)) => Literal::Numeric((l <= r)),
//         _ => panic!(),

//         }).unwrap();
//     Symbol::Value(out)
// }
binary_op!(add_op, l, r, l + r);
binary_op!(sub_op, l, r, l - r);
binary_op!(mul_op, l, r, l * r);
binary_op!(div_op, l, r, l / r);
