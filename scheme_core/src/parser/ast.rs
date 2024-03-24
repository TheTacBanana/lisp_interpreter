use crate::{literal::Literal, token::span::Span};

type P<T> = Box<T>;

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum AST {
    /// Identifer
    Identifier(String, Span),
    /// Literal Value
    Literal(Literal, Span),
    /// String Literal
    StringLiteral(String, Span),
    /// Operation, List of Parameter Names
    Operation(P<AST>, Vec<AST>, Span),
    /// Head, Tail
    List(P<AST>, P<AST>, Span),
    // Empty List
    EmptyList(Span),
}

impl AST {
    pub fn span(&self) -> Span {
        match self {
            AST::Identifier(_, s)
            | AST::Literal(_, s)
            | AST::StringLiteral(_, s)
            | AST::Operation(_, _, s)
            | AST::List(_, _, s)
            | AST::EmptyList(s) => *s,
        }
    }

    pub fn list_from_vec(mut vec: Vec<AST>) -> AST {
        let mut drain = vec.drain(..).rev();
        let head = drain.next().unwrap();
        let span = head.span();

        let mut head = AST::List(Box::new(head), Box::new(AST::EmptyList(span)), span);
        for v in drain {
            let span = v.span();
            head = AST::List(Box::new(v), Box::new(head), span);
        }
        return head;
    }
}

impl std::fmt::Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let var_name = match self {
            AST::Identifier(ident, _) => write!(f, "{ident}"),
            AST::Literal(lit, _) => write!(f, "{lit}"),
            AST::Operation(ident, params, _) => write!(f, "{ident} {params:?}"),
            AST::List(head, tail, _) => write!(f, "{head}:{tail}"),
            AST::StringLiteral(s, _) => write!(f, "{s}"),
            AST::EmptyList(_) => write!(f, "()"),
        };
        var_name
    }
}
