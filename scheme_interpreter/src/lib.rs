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
            AST::Operation(op, params) => self.operation(*op, params),
            AST::List(_) => todo!(),
        }
    }

    fn operation(&mut self, op: AST, mut params: Vec<AST>) -> Symbol {
        match op {
            AST::Identifier(def, _) if def == "define" => {
                let mut params = params.drain(..);
                match params.next().unwrap() {
                    AST::Identifier(ident_name, _) => {
                        println!("{:?}", params);
                        let value = self.interpret(params.next().unwrap());
                        self.define_symbol(&ident_name, value);
                    }
                    AST::Operation(f_name, mut param_names) => {
                        let AST::Identifier(f_name, _) = *f_name else {
                            panic!()
                        };
                        let param_names = param_names
                            .drain(..)
                            .map(|p| match p {
                                AST::Identifier(i, _) => i,
                                _ => panic!(),
                            })
                            .collect();
                        let f_ast = params.next().unwrap();

                        self.define_symbol(
                            &f_name,
                            Symbol::FunctionCall(FunctionCall::Defined(param_names, f_ast)),
                        );
                    }
                    _ => panic!(),
                }
                Symbol::Bottom
            }
            AST::Identifier(ident, _) => {
                let params = params
                    .drain(..)
                    .map(|ast| self.interpret(ast))
                    .collect::<Vec<_>>();
                match self.resolve_symbol(&ident) {
                    Some(Symbol::FunctionCall(f)) => self.func_call(f.clone(), params),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    fn resolve_symbol(&self, symbol: &str) -> Option<&Symbol> {
        self.stack.iter().rev().find_map(|st| st.get(symbol))
    }

    fn define_symbol(&mut self, symbol: &str, value: Symbol) {
        match self
            .stack
            .iter_mut()
            .rev()
            .find_map(|st| st.get_mut(symbol))
        {
            None => self.stack.last_mut().unwrap().add_item(symbol, value),
            Some(r) => *r = value,
        }
    }

    fn top_stack(&mut self) -> &mut StackFrame {
        self.stack.last_mut().unwrap()
    }

    fn create_stack_frame(&mut self) {
        self.stack.push(StackFrame::new())
    }

    fn pop_stack_frame(&mut self) {
        self.stack.pop();
    }

    fn func_call(&mut self, f: FunctionCall, params: Vec<Symbol>) -> Symbol {
        match f {
            FunctionCall::Native(f) => f(params),
            FunctionCall::Defined(param_names, ast) => {
                self.create_stack_frame();
                let stack = self.top_stack();

                param_names
                    .iter()
                    .zip(params)
                    .for_each(|(p_name, p_val)| stack.add_item(&p_name, p_val));

                let result = self.interpret(ast);
                self.pop_stack_frame();
                result
            }
        }
    }
}

#[cfg(test)]
pub mod test {
    use scheme_core::{
        parser::{
            ast::AST,
            token::{Literal, Numeric},
        },
        token::span::Span,
    };

    use crate::{symbol::Symbol, Interpreter};

    #[test]
    pub fn define_value() {
        let pi_def = AST::Operation(
            Box::new(AST::Identifier("define".into(), Span::zero())),
            vec![
                AST::Identifier("pi".into(), Span::zero()),
                AST::Literal(Literal::Numeric(Numeric::Float(3.14)), Span::zero()),
            ],
        );
        let pi_use = AST::Identifier("pi".into(), Span::zero());

        let mut i = Interpreter::new();

        i.interpret(pi_def);
        println!("{:?}", i.stack);
        assert_eq!(
            i.interpret(pi_use),
            Symbol::Value(Literal::Numeric(Numeric::Float(3.14)))
        );
    }

    #[test]
    pub fn add_func() {
        let f_def = AST::Operation(
            Box::new(AST::Identifier("define".into(), Span::zero())),
            vec![
                AST::Operation(
                    Box::new(AST::Identifier("add".into(), Span::zero())),
                    vec![
                        AST::Identifier("x".into(), Span::zero()),
                        AST::Identifier("y".into(), Span::zero()),
                    ],
                ),
                AST::Operation(
                    Box::new(AST::Identifier("+".into(), Span::zero())),
                    vec![
                        AST::Identifier("x".into(), Span::zero()),
                        AST::Identifier("y".into(), Span::zero()),
                    ],
                ),
            ],
        );

        let f_use = AST::Operation(
            Box::new(AST::Identifier("add".into(), Span::zero())),
            vec![
                AST::Literal(Literal::Numeric(Numeric::Int(1)), Span::zero()),
                AST::Literal(Literal::Numeric(Numeric::Int(2)), Span::zero()),
            ],
        );

        let mut i = Interpreter::new();

        i.interpret(f_def);
        assert_eq!(
            i.interpret(f_use),
            Symbol::Value(Literal::Numeric(Numeric::Int(3)))
        );
    }
}
