use super::token::Literal;

type P<T> = Box<T>;

pub enum AST {
    /// Identifer
    Identifier(String),
    /// Literal Value
    Literal(Literal),
    /// Operation, List of parameters
    Operation(P<AST>, Vec<AST>),
    /// List of Items
    List(Vec<AST>),
}
