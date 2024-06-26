use core::parser::ast::AST;
use std::hash::{DefaultHasher, Hash, Hasher};

use crate::{InterpreterContext, InterpreterResult};

pub type NativeFunc = fn(&InterpreterContext, usize) -> InterpreterResult<()>;
pub type TokenNativeFunc = fn(&InterpreterContext, Vec<&AST>) -> InterpreterResult<()>;
pub type MacroFunc = fn(&InterpreterContext, Vec<&AST>) -> InterpreterResult<usize>;

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Func {
    Native(String, NativeFunc),
    TokenNative(String, TokenNativeFunc),
    Macro(String, MacroFunc),
    Defined(Option<String>, Vec<String>, AST),
}


impl Func {
    pub fn calc_hash(&self) -> u64 {
        let mut s = DefaultHasher::new();
        self.hash(&mut s);
        s.finish()
    }
}

impl std::fmt::Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Func::Native(name, n) => write!(f, "{name} {n:?}"),
            Func::TokenNative(name, n) => write!(f, "{name} {n:?}"),
            Func::Macro(name, n) => write!(f, "{name} {n:?}"),
            Func::Defined(Some(name), args, _body) => write!(f, "{name}({args:?})"),
            Func::Defined(None, args, _body) => write!(f, "Lambda({args:?})"),
        }
    }
}
