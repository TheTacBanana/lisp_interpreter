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
    /// Head, Tail
    List(P<AST>, Option<P<AST>>),
}

impl AST {
    pub fn list_from_vec(mut vec: Vec<AST>) -> AST {
        let mut drain = vec.drain(..).rev();
        let mut head = AST::List(Box::new(drain.next().unwrap()), None);
        for v in drain {
            head = AST::List(Box::new(v), Some(Box::new(head)));
        }
        return head;
    }
}

impl std::fmt::Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AST::Identifier(ident, _) => write!(f, "{ident}"),
            AST::Literal(lit, _) => write!(f, "{lit}"),
            AST::List(head, tail) => match tail {
                Some(tail) => write!(f, "{head}:{tail}"),
                None => write!(f, "{head}:()"),
            }
                ,
            AST::Operation(ident, params) => write!(f, "{ident} {params:?}"),
        }
    }
}
