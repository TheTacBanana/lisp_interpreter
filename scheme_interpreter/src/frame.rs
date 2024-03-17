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
        new.add_item("eq?", FunctionCall::Native(eq_cmp).into());
        new.add_item("+", FunctionCall::Native(add_op).into());
        new.add_item("-", FunctionCall::Native(sub_op).into());
        new.add_item("*", FunctionCall::Native(mul_op).into());
        new.add_item("/", FunctionCall::Native(div_op).into());

        new.add_item("<", FunctionCall::Native(lt_cmp).into());
        new.add_item("<=", FunctionCall::Native(lteq_cmp).into());
        new.add_item(">", FunctionCall::Native(gt_cmp).into());
        new.add_item(">=", FunctionCall::Native(gteq_cmp).into());

        new.add_item("write", FunctionCall::Native(write_op).into());

        new.add_item("lambda", FunctionCall::Native(lambda).into());

        new.add_item("if", FunctionCall::Native(if_op).into());
        new.add_item("cond", FunctionCall::Native(cond_op).into());
        new.add_item("not", FunctionCall::Native(not_op).into());

        new.add_item("car", FunctionCall::Native(car_op).into());
        new.add_item("cdr", FunctionCall::Native(cdr_op).into());
        new.add_item("cons", FunctionCall::Native(cons_op).into());
        new.add_item("empty?", FunctionCall::Native(empty_op).into());
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

pub fn write_op(interpreter: &mut Interpreter, mut vec: Vec<Symbol>) -> Symbol {
    assert!(vec.len() == 1);
    // println!("{vec:?}");
    let s = match vec.drain(..).next().unwrap() {
        Symbol::Tokens(ast) => interpreter.interpret(ast),
        Symbol::List(head, tail) => {
            let s = match *tail {
                Symbol::Tokens(tail) => interpreter.interpret(tail),
                v => v,
            };
            Symbol::List(head, Box::new(s))
        }
        v => v
    };
    println!("{}", s);
    Symbol::Bottom
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

pub fn cond_op(interpreter: &mut Interpreter, mut vec: Vec<Symbol>) -> Symbol {
    let drain = vec.drain(..);
    for d in drain {
        match d {
            Symbol::Tokens(AST::Operation(cond, mut body)) => {
                let Symbol::Value(lit) = interpreter.interpret(*cond) else { panic!() };
                if lit.is_truthy() {
                    return interpreter.interpret(body.drain(..).next().unwrap())
                }
            },
            _ => panic!(),
        };
    }
    panic!()
}

pub fn not_op(interpreter: &mut Interpreter, mut vec: Vec<Symbol>) -> Symbol {
    let mut drain = vec.drain(..);
    let symbol = match drain.next().unwrap() {
        s @ Symbol::Value(_) => s,
        Symbol::Tokens(ast) => interpreter.interpret(ast),
        _ => panic!(),
    };
    let Symbol::Value(lit) = symbol else { panic!() };
    Symbol::Value(Literal::from_bool(!lit.is_truthy()))
}

pub fn lambda(interpreter: &mut Interpreter, mut vec: Vec<Symbol>) -> Symbol {
    let mut params = vec.drain(..);
    match params.next().unwrap() {
        Symbol::Tokens(AST::Operation(var1, mut varxs)) => {
            let mut param_names = vec![*var1];
            param_names.extend(varxs.drain(..));
            let param_names = param_names
                .drain(..)
                .map(|p| match p {
                    AST::Identifier(i) => i,
                    _ => panic!(),
                })
                .collect();
            let Symbol::Tokens(f_ast) = params.next().unwrap() else { panic!() };

            Symbol::FunctionCall(FunctionCall::Defined(param_names, f_ast))
        }
        Symbol::Tokens(AST::Identifier(var1)) => {
            let param_names = vec![var1];
            let Symbol::Tokens(f_ast) = params.next().unwrap() else { panic!() };
            Symbol::FunctionCall(FunctionCall::Defined(param_names, f_ast))
        }
        e => panic!("{e:?}"),
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

binary_op!(add_op, l, r, l + r);
binary_op!(sub_op, l, r, l - r);
binary_op!(mul_op, l, r, l * r);
binary_op!(div_op, l, r, l / r);

macro_rules! comparison_op {
    ($name:ident, $l:ident, $r:ident, $op:expr) => {
        pub fn $name(interpreter: &mut Interpreter, mut vec: Vec<Symbol>) -> Symbol {
            let drain = vec
                .drain(..)
                .map(|s| match s {
                    Symbol::Value(l) => l,
                    Symbol::Tokens(ast) => {
                        let Symbol::Value(lit) = interpreter.interpret(ast) else { panic!() };
                        lit
                    }
                    _ => panic!(),
                }).collect::<Vec<_>>();

            let mut out = true;
            drain
                .windows(2)
                .for_each(|pairs| match (pairs.get(0).unwrap(), pairs.get(1).unwrap()) {
                    (Literal::Numeric($l), Literal::Numeric($r)) => {
                        out = out && ($op)
                    },
                    _ => panic!(),
                });
            Symbol::Value(Literal::Boolean(out))
        }
    };
}

comparison_op!(eq_cmp, l, r, l == r);
comparison_op!(lt_cmp, l, r, l < r);
comparison_op!(lteq_cmp, l, r, l <= r);
comparison_op!(gt_cmp, l, r, l > r);
comparison_op!(gteq_cmp, l, r, l >= r);

pub fn car_op(interpreter: &mut Interpreter, mut vec: Vec<Symbol>) -> Symbol {
    assert!(vec.len() == 1);
    match vec.drain(..).next().unwrap() {
        Symbol::List(head, _) => *head,
        Symbol::Tokens(l) => {
            match interpreter.interpret(l) {
                v @ Symbol::Value(_) => v,
                Symbol::List(head, _) => *head,
                e => panic!("{e:?}")
            }
        },
        v @ Symbol::Value(_) => v,
        e => panic!("Unexpected {e}"),
    }
}

pub fn cdr_op(interpreter: &mut Interpreter, mut vec: Vec<Symbol>) -> Symbol {
    assert!(vec.len() == 1);
    match vec.drain(..).next().unwrap() {
        Symbol::List(_, tail) => *tail,
        Symbol::Tokens(l) => {
            let Symbol::List(_, tail) = interpreter.interpret(l) else { panic!() };
            *tail
        },
        e => panic!("Unexpected {e}"),
    }
}

pub fn cons_op(interpreter: &mut Interpreter, mut vec: Vec<Symbol>) -> Symbol {
    assert!(vec.len() == 2);
    let mut drain = vec.drain(..);

    let head = match drain.next().unwrap() {
        Symbol::Tokens(l) => interpreter.interpret(l),
        v  => v,
    };
    let tail = match drain.next().unwrap() {
        Symbol::Tokens(l) => interpreter.interpret(l),
        v  => v,
    };

    Symbol::List(Box::new(head), Box::new(tail))
}

pub fn empty_op(interpreter: &mut Interpreter, mut vec: Vec<Symbol>) -> Symbol {
    assert!(vec.len() == 1);
    match vec.drain(..).next().unwrap() {
        Symbol::Bottom => Symbol::Value(Literal::Boolean(true)),
        Symbol::Tokens(ast) => match interpreter.interpret(ast) {
            Symbol::Bottom => Symbol::Value(Literal::Boolean(true)),
            _ => Symbol::Value(Literal::Boolean(false)),
        }
        _ => Symbol::Value(Literal::Boolean(false)),
    }
}