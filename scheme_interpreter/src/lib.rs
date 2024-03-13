#![feature(let_chains)]

use core::panic;

use frame::StackFrame;
use scheme_core::parser::ast::AST;
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
        if let Some(ast) = self.interpret(ast) {
            println!("{}", ast);
        }
    }

    fn interpret(&mut self, ast: AST) -> Option<AST> {
        match ast {
            // AST::Identifier(ident, _) => {
            //     Some(self.resolve_symbol(&ident).expect("Func not defined"))
            // },
            l @ AST::Literal(_, _) => Some(l),
            AST::Operation(op, params) => match *op {
                AST::Identifier(ident, _) => {
                    match self.stack[0].get(&ident).expect("Func not defined") {
                        Symbol::FunctionCall(f) => self.apply_operation(f, &params),
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            },
            AST::List(_) => todo!(),
            _ => todo!(),
        }
    }

    fn apply_operation(&self, f: &FunctionCall, params: &Vec<AST>) -> Option<AST> {
        match f {
            FunctionCall::Native(f) => f(params),
        }
    }
}
