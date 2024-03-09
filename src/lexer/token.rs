use crate::token::{ErrorToken, Token, TokenKind};
use std::error::Error;

use super::literal::NumericLiteral;

pub type LexerToken = Token<LexerTokenKind>;

pub type LexerErr = ErrorToken<LexerTokenKind, LexerTokenError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexerTokenKind{
    Whitespace(String),
    Identifer(String),
    Boolean(String),
    Numeric(NumericLiteral),
    Character(String),
    String(String),
    Symbol(String),
    EOF,
}

impl TokenKind for LexerTokenKind {
    fn inner_mut(&mut self) -> &mut String {
        match self {
            LexerTokenKind::Whitespace(s) |
            LexerTokenKind::Identifer(s) |
            LexerTokenKind::Boolean(s) |
            LexerTokenKind::Character(s) |
            LexerTokenKind::String(s) |
            LexerTokenKind::Symbol(s) => s,
            LexerTokenKind::Numeric(s) => s.inner(),
            LexerTokenKind::EOF => panic!(),
        }
    }

    fn the_same(&self, other: &Self) -> bool {
        use std::mem::discriminant;
        discriminant(self) == discriminant(other)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum LexerTokenError {
    /// Invalid character used in a Float
    InvalidInFloat,
    /// Multiple points '.' have appeared in a FloatLiteral
    MultiplePointsInFloat,
    /// Invalid character used in an N-ary Literal
    InvalidInNAryLiteral(usize),
}

impl std::fmt::Display for LexerTokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self);
        Ok(())
    }
}

impl Error for LexerTokenError {}