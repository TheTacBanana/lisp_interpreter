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

    pub fn interpret(&mut self, ast: AST) -> Symbol {
        // println!("{:?}", ast);
        match ast {
            AST::Literal(l) => Symbol::Value(l),
            AST::Identifier(ident) => match self.resolve_symbol(&ident).cloned().unwrap() {
                Symbol::Tokens(ast) => self.interpret(ast),
                v => v,
            },
            AST::Operation(op, params) => self.operation(*op, params),

            AST::List(head, tail) if *tail == AST::EmptyList => {
                let head = self.interpret(*head);
                Symbol::List(Box::new(head), Box::new(Symbol::Bottom))
            }
            AST::List(head, tail) => {
                let head = self.interpret(*head);
                Symbol::List(Box::new(head), Box::new(Symbol::Tokens(*tail)))
            }
            AST::EmptyList => Symbol::Bottom,
        }
    }

    fn operation(&mut self, op: AST, mut params: Vec<AST>) -> Symbol {
        match op {
            AST::Identifier(def) if def == "define" => {
                let mut params = params.drain(..);
                match params.next().unwrap() {
                    AST::Identifier(ident_name) => {
                        let value = self.interpret(params.next().unwrap());
                        self.define_symbol(&ident_name, value);
                    }
                    AST::Operation(f_name, mut param_names) => {
                        let AST::Identifier(f_name) = *f_name else {
                            panic!()
                        };
                        let param_names = param_names
                            .drain(..)
                            .map(|p| match p {
                                AST::Identifier(i) => i,
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
            AST::Identifier(ident) => {
                let params = params.drain(..).map(|ast| Symbol::Tokens(ast)).collect();
                match self.resolve_symbol(&ident) {
                    Some(Symbol::FunctionCall(f)) => {
                        let symbol = self.func_call(f.clone(), params);
                        match symbol {
                            Symbol::Tokens(ast) => self.interpret(ast),
                            s => s,
                        }
                    }
                    e => panic!("{ident} {e:?}"),
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

    fn func_call(&mut self, f: FunctionCall, mut params: Vec<Symbol>) -> Symbol {
        self.create_stack_frame();
        let result = match f {
            FunctionCall::Native(f) => {
                let result = match f(self, params) {
                    Symbol::Tokens(ast) => self.interpret(ast),
                    v => v,
                };
                result
            }
            FunctionCall::Defined(param_names, ast) => {
                let params = params
                    .drain(..)
                    .map(|p| match p {
                        Symbol::Tokens(ast) => self.interpret(ast),
                        t => t,
                    })
                    .collect::<Vec<_>>();

                let stack = self.top_stack();
                param_names
                    .iter()
                    .zip(params)
                    .for_each(|(p_name, p_val)| stack.add_item(&p_name, p_val));

                let result = self.interpret(ast);
                result
            }
        };
        self.pop_stack_frame();
        result
    }
}

#[cfg(test)]
pub mod test {
    use scheme_core::{
        lexer::Lexer,
        parser::{
            ast::AST,
            token::{Literal, Numeric},
            Parser,
        },
        token::span::Span,
    };

    use crate::{symbol::Symbol, Interpreter};

    #[test]
    pub fn define_value() {
        let pi_def = AST::Operation(
            Box::new(AST::Identifier("define".into())),
            vec![
                AST::Identifier("pi".into()),
                AST::Literal(Literal::Numeric(Numeric::Float(3.14))),
            ],
        );
        let pi_use = AST::Identifier("pi".into());

        let mut i = Interpreter::new();

        i.interpret(pi_def);
        assert_eq!(
            i.interpret(pi_use),
            Symbol::Value(Literal::Numeric(Numeric::Float(3.14)))
        );
    }

    #[test]
    pub fn define_func() {
        let f_def = AST::Operation(
            Box::new(AST::Identifier("define".into())),
            vec![
                AST::Operation(
                    Box::new(AST::Identifier("add".into())),
                    vec![
                        AST::Identifier("x".into()),
                        AST::Identifier("y".into()),
                    ],
                ),
                AST::Operation(
                    Box::new(AST::Identifier("+".into())),
                    vec![
                        AST::Identifier("x".into()),
                        AST::Identifier("y".into()),
                    ],
                ),
            ],
        );

        let f_use = AST::Operation(
            Box::new(AST::Identifier("add".into())),
            vec![
                AST::Literal(Literal::Numeric(Numeric::Int(1))),
                AST::Literal(Literal::Numeric(Numeric::Int(2))),
            ],
        );

        let mut i = Interpreter::new();

        i.interpret(f_def);
        assert_eq!(
            i.interpret(f_use),
            Symbol::Value(Literal::Numeric(Numeric::Int(3)))
        );
    }

    #[test]
    pub fn if_op() {
        let if_def = AST::Operation(
            Box::new(AST::Identifier("if".into())),
            vec![
                AST::Literal(Literal::Boolean(true)),
                AST::Operation(
                    Box::new(AST::Identifier("if".into())),
                    vec![
                        AST::Literal(Literal::Boolean(false)),
                        AST::Literal(Literal::Numeric(Numeric::Int(1))),
                        AST::Literal(Literal::Numeric(Numeric::Int(2))),
                    ],
                ),
                AST::Literal(Literal::Numeric(Numeric::Int(3))),
            ],
        );

        let mut i = Interpreter::new();

        assert_eq!(
            i.interpret(if_def),
            Symbol::Value(Literal::Numeric(Numeric::Int(2)))
        );
    }

    #[test]
    pub fn lt_ops() {
        let lt_def = AST::Operation(
            Box::new(AST::Identifier("<".into())),
            vec![
                AST::Literal(Literal::Numeric(Numeric::Int(1))),
                AST::Literal(Literal::Numeric(Numeric::Int(2))),
            ],
        );

        let lt_def_fail = AST::Operation(
            Box::new(AST::Identifier("<".into())),
            vec![
                AST::Literal(Literal::Numeric(Numeric::Int(2))),
                AST::Literal(Literal::Numeric(Numeric::Int(2))),
            ],
        );

        let lt_eq_def = AST::Operation(
            Box::new(AST::Identifier("<=".into())),
            vec![
                AST::Literal(Literal::Numeric(Numeric::Int(1))),
                AST::Literal(Literal::Numeric(Numeric::Int(2))),
            ],
        );

        let lt_eq_def2 = AST::Operation(
            Box::new(AST::Identifier("<=".into())),
            vec![
                AST::Literal(Literal::Numeric(Numeric::Int(2))),
                AST::Literal(Literal::Numeric(Numeric::Int(2))),
            ],
        );

        let lt_eq_def_fail = AST::Operation(
            Box::new(AST::Identifier("<=".into())),
            vec![
                AST::Literal(Literal::Numeric(Numeric::Int(3))),
                AST::Literal(Literal::Numeric(Numeric::Int(2))),
            ],
        );

        let mut i = Interpreter::new();

        assert_eq!(i.interpret(lt_def), Symbol::Value(Literal::Boolean(true)));
        assert_eq!(
            i.interpret(lt_def_fail),
            Symbol::Value(Literal::Boolean(false))
        );
        assert_eq!(
            i.interpret(lt_eq_def),
            Symbol::Value(Literal::Boolean(true))
        );
        assert_eq!(
            i.interpret(lt_eq_def2),
            Symbol::Value(Literal::Boolean(true))
        );
        assert_eq!(
            i.interpret(lt_eq_def_fail),
            Symbol::Value(Literal::Boolean(false))
        );
    }

    #[test]
    pub fn gt_ops() {
        let lt_def = AST::Operation(
            Box::new(AST::Identifier(">".into())),
            vec![
                AST::Literal(Literal::Numeric(Numeric::Int(2))),
                AST::Literal(Literal::Numeric(Numeric::Int(1))),
            ],
        );

        let lt_def_fail = AST::Operation(
            Box::new(AST::Identifier(">".into())),
            vec![
                AST::Literal(Literal::Numeric(Numeric::Int(2))),
                AST::Literal(Literal::Numeric(Numeric::Int(2))),
            ],
        );

        let lt_eq_def = AST::Operation(
            Box::new(AST::Identifier(">=".into())),
            vec![
                AST::Literal(Literal::Numeric(Numeric::Int(2))),
                AST::Literal(Literal::Numeric(Numeric::Int(1))),
            ],
        );

        let lt_eq_def2 = AST::Operation(
            Box::new(AST::Identifier(">=".into())),
            vec![
                AST::Literal(Literal::Numeric(Numeric::Int(2))),
                AST::Literal(Literal::Numeric(Numeric::Int(2))),
            ],
        );

        let lt_eq_def_fail = AST::Operation(
            Box::new(AST::Identifier(">=".into())),
            vec![
                AST::Literal(Literal::Numeric(Numeric::Int(2))),
                AST::Literal(Literal::Numeric(Numeric::Int(3))),
            ],
        );

        let mut i = Interpreter::new();

        assert_eq!(i.interpret(lt_def), Symbol::Value(Literal::Boolean(true)));
        assert_eq!(
            i.interpret(lt_def_fail),
            Symbol::Value(Literal::Boolean(false))
        );
        assert_eq!(
            i.interpret(lt_eq_def),
            Symbol::Value(Literal::Boolean(true))
        );
        assert_eq!(
            i.interpret(lt_eq_def2),
            Symbol::Value(Literal::Boolean(true))
        );
        assert_eq!(
            i.interpret(lt_eq_def_fail),
            Symbol::Value(Literal::Boolean(false))
        );
    }

    #[test]
    pub fn fibonacci() {
        let program: String = "(define (fib n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))
        (fib 7)
        "
        .into();

        let mut interpreter = Interpreter::new();

        let lexer_result = Lexer::from_string(program.clone()).lex();
        assert!(lexer_result.error_writer().is_none());

        let mut parser_result = Parser::new(lexer_result.tokens).parse();
        parser_result.error_writer(&program);

        let mut program = parser_result.ast.drain(..);
        assert_eq!(
            interpreter.interpret(program.next().unwrap()),
            Symbol::Bottom
        );

        assert_eq!(
            interpreter.interpret(program.next().unwrap()),
            Symbol::Value(Literal::Numeric(Numeric::Int(13)))
        );
    }
}
