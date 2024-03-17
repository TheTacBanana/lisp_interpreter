use std::default;

use scheme_core::parser::{ast::AST, token::Literal};

use crate::InterpreterContext;

type P<T> = Box<T>;

#[derive(Debug, Default)]
pub enum Object {
    #[default]
    Bottom,
    List(P<Object>, P<Object>),
    Value(Literal),
    Func(Func),
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Bottom => write!(f, "()"),
            Object::List(head, tail) => write!(f, "{head} {tail}"),
            Object::Value(v) => write!(f, "{v}"),
            Object::Func(fid) => write!(f, "{fid}"),
        }
    }
}

pub type NativeFunc = fn(&mut InterpreterContext, usize);

#[derive(Debug)]
pub enum Func {
    Native(NativeFunc),
    Defined(Vec<String>, AST),
}

impl std::fmt::Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Func::Native(n) => write!(f, "{n:?}"),
            Func::Defined(args, body) => write!(f, "Fn({args:?})"),
        }
    }
}
