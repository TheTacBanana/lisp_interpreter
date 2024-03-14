#![feature(let_chains)]

use core::panic;

use frame::StackFrame;
use scheme_core::parser::{ast::AST, token::Literal};
use symbol::{FunctionCall, Symbol};

pub mod frame;
pub mod symbol;

pub struct Interpreter {
    stack: Vec<StackFrame>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            stack: vec![StackFrame::prelude()],
        }
    }

    pub fn run_ast(&mut self, ast: AST) {
        let res = self.interpret(ast);
        println!("{res}")
    }

    fn interpret(&mut self, ast: AST) -> Symbol {
        match ast {
            AST::Literal(l, _) => Symbol::Value(l),
            AST::Identifier(ident, _) => self.resolve_symbol(&ident).cloned().unwrap(),
            AST::Operation(op, mut params) => match *op {
                AST::Identifier(ident, _) => {
                    let params = params
                        .drain(..)
                        .map(|ast| self.interpret(ast))
                        .collect::<Vec<_>>();
                    match self.stack[0].get(&ident).expect("Func not defined") {
                        Symbol::FunctionCall(f) => self.apply_operation(f, params),
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            },
            AST::List(_) => todo!(),
            _ => todo!(),
        }
    }

    fn resolve_symbol(&self, symbol: &str) -> Option<&Symbol> {
        self.stack.iter().rev().find_map(|st| st.get(symbol))
    }

    // fn write_symbol(&self, symbol: &str, )

    fn apply_operation(&self, f: &FunctionCall, params: Vec<Symbol>) -> Symbol {
        match f {
            FunctionCall::Native(f) => f(params),
        }
    }
}
