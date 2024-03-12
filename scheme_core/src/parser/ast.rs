use crate::token::span::Span;

use super::token::Literal;

type P<T> = Box<T>;

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
