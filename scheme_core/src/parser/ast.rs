use crate::token::span::Span;

use super::token::Literal;

type P<T> = Box<T>;

#[derive(Debug, Clone, PartialEq)]
pub enum AST {
    /// Identifer
    Identifier(String),
    /// Literal Value
    Literal(Literal),
    /// Operation, List of Parameter Names
    Operation(P<AST>, Vec<AST>),
    /// Head, Tail
    List(P<AST>, P<AST>),
    // Empty List
    EmptyList,
}

impl AST {
    pub fn list_from_vec(mut vec: Vec<AST>) -> AST {
        if vec.is_empty() {
            return AST::EmptyList;
        }
        let mut drain = vec.drain(..).rev();
        let mut head = AST::List(Box::new(drain.next().unwrap()), Box::new(AST::EmptyList));
        for v in drain {
            head = AST::List(Box::new(v), Box::new(head));
        }
        return head;
    }
}

impl std::fmt::Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AST::Identifier(ident) => write!(f, "{ident}"),
            AST::Literal(lit) => write!(f, "{lit}"),
            AST::Operation(ident, params) => write!(f, "{ident} {params:?}"),
            AST::List(head, tail) => write!(f, "{head}:{tail}"),
            AST::EmptyList => write!(f, "()"),
        }
    }
}
