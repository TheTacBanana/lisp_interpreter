use scheme_core::parser::{ast::AST, token::Literal};

use crate::InterpreterContext;

#[derive(Debug, Clone, PartialEq)]
pub enum ObjectPointer {
    Object(Object),
    Stack { frame_index: usize, name: String },
    Heap { name: String },
}

type P<T> = Box<T>;

#[derive(Debug, Default, Clone, PartialEq)]
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
pub type MacroFunc = fn(&mut InterpreterContext, Vec<AST>);

#[derive(Debug, Clone, PartialEq)]
pub enum Func {
    Native(String, NativeFunc),
    Macro(String, MacroFunc),
    Defined(Option<String>, Vec<String>, AST),
}


impl Func {
    pub fn to_string(&self) -> String {
        format!("{self}")
    }
}

impl std::fmt::Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Func::Native(name, n) => write!(f, "{name} {n:?}"),
            Func::Macro(name, n) => write!(f, "{name} {n:?}"),
            Func::Defined(Some(name), args, body) => write!(f, "{name}({args:?})"),
            Func::Defined(None, args, body) => write!(f, "Lambda({args:?})"),
        }
    }
}
