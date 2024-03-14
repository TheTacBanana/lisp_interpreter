use crate::token::span::Span;

use super::token::Literal;

type P<T> = Box<T>;

#[derive(Debug, Clone, PartialEq)]
pub enum AST {
    /// Identifer
    Identifier(String, Span),
    /// Literal Value
    Literal(Literal, Span),
    /// Operation, List of Parameter Names
    Operation(P<AST>, Vec<AST>),
    /// List of Items
    List(Vec<AST>),
}

impl std::fmt::Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AST::Identifier(ident, _) => write!(f, "{ident}"),
            AST::Literal(lit, _) => write!(f, "{lit}"),
            AST::List(list) => write!(f, "{list:?}"),
            AST::Operation(ident, params) => write!(f, "{ident} {params:?}"),
        }
    }
}
