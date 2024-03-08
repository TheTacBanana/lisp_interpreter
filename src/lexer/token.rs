use crate::token::{ErrorToken, Token};
use std::error::Error;

pub type LexerToken = Token<LexerTokenKind>;

pub type LexerErr = ErrorToken<LexerTokenError>;

#[derive(Debug, Clone)]
pub enum LexerTokenKind{
    Identifer(String),
    Whitespace(String),
    IntLiteral(String),
    StringLiteral(String),
}

#[derive(Debug)]
pub enum LexerTokenError {

}

impl std::fmt::Display for LexerTokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

impl Error for LexerTokenError {}