use scheme_core::parser::{ast::AST, token::Literal};

use crate::Interpreter;

#[derive(Debug, Clone, PartialEq)]
pub enum Symbol {
    Value(Literal),
    Tokens(AST),
    FunctionCall(FunctionCall),
    Bottom,
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::Value(ast) => write!(f, "{ast}")?,
            Symbol::FunctionCall(FunctionCall::Native(_)) => write!(f, "NativeFn")?,
            Symbol::FunctionCall(FunctionCall::Defined(_, _)) => write!(f, "UserFn")?,
            Symbol::Tokens(ast) => write!(f, "{ast}")?,
            Symbol::Bottom => (),
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionCall {
    /// Native function call
    Native(fn(&mut Interpreter, Vec<Symbol>) -> Symbol),

    /// List of Param Identifiers,
    Defined(Vec<String>, AST)
}