use crate::token::{ErrorToken, Token};
use std::error::Error;

use super::literal::NumericLiteral;

pub type LexerToken = Token<LexerTokenKind>;

pub type LexerErr = ErrorToken<LexerTokenError>;

#[derive(Debug, Clone)]
pub enum LexerTokenKind{
    Whitespace(String),
    Identifer(String),
    Boolean(String),
    Numeric(NumericLiteral),
    Character(String),
    String(String),
}

#[derive(Debug)]
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
        Ok(())
    }
}

impl Error for LexerTokenError {}