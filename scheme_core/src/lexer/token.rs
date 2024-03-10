use crate::token::{ErrorToken, Token, TokenKind};
use std::error::Error;

use super::literal::NumericLiteral;

pub type LexerToken = Token<LexerTokenKind>;

pub type LexerErr = ErrorToken<LexerTokenKind, LexerTokenError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexerTokenKind {
    Whitespace(String),
    Identifer(String),
    Boolean(String),
    Numeric(NumericLiteral),
    Character(String),
    String(String),
    Symbol(String),
    Comment(String),
    EOF,
}

impl TokenKind for LexerTokenKind {
    fn inner(&self) -> &String {
        match self {
            LexerTokenKind::Whitespace(s)
            | LexerTokenKind::Identifer(s)
            | LexerTokenKind::Boolean(s)
            | LexerTokenKind::Character(s)
            | LexerTokenKind::String(s)
            | LexerTokenKind::Comment(s)
            | LexerTokenKind::Symbol(s) => s,
            LexerTokenKind::Numeric(s) => s.inner(),
            LexerTokenKind::EOF => panic!(),
        }
    }

    fn inner_mut(&mut self) -> &mut String {
        match self {
            LexerTokenKind::Whitespace(s)
            | LexerTokenKind::Identifer(s)
            | LexerTokenKind::Boolean(s)
            | LexerTokenKind::Character(s)
            | LexerTokenKind::String(s)
            | LexerTokenKind::Comment(s)
            | LexerTokenKind::Symbol(s) => s,
            LexerTokenKind::Numeric(s) => s.inner_mut(),
            LexerTokenKind::EOF => panic!(),
        }
    }

    fn to_string(self) -> String {
        match self {
            LexerTokenKind::Whitespace(s)
            | LexerTokenKind::Identifer(s)
            | LexerTokenKind::Boolean(s)
            | LexerTokenKind::Character(s)
            | LexerTokenKind::String(s)
            | LexerTokenKind::Comment(s)
            | LexerTokenKind::Symbol(s) => s,
            LexerTokenKind::Numeric(s) => s.to_string(),
            LexerTokenKind::EOF => panic!(),
        }
    }

    fn the_same(&self, other: &Self) -> bool {
        use std::mem::discriminant;
        discriminant(self) == discriminant(other)
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum LexerTokenError {
    /// Escape character expected after '\'
    EscapeCharacterExpected,
    /// End of file encountered in String Literal
    EOFInStringLiteral,
    /// Invalid character used in a Float
    InvalidInFloat,
    /// Multiple points '.' have appeared in a FloatLiteral
    MultiplePointsInFloat,
    /// Invalid character used in an N-ary Literal
    InvalidInNAryLiteral(usize),
    /// Point used in N-ary Literal
    PointInNAryLiteral(usize),
}

impl std::fmt::Display for LexerTokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tmp;
        let s = match self {
            LexerTokenError::EscapeCharacterExpected => " Escape character expected after '\'",
            LexerTokenError::EOFInStringLiteral => "End of file encountered in String Literal",
            LexerTokenError::InvalidInFloat => "Invalid character in Floating Point Number",
            LexerTokenError::MultiplePointsInFloat => {
                "Multiple decimal points in Floating Point Number"
            }
            LexerTokenError::InvalidInNAryLiteral(n) => {
                tmp = format!("Invalid character in Base {n} Literal");
                &tmp
            }
            LexerTokenError::PointInNAryLiteral(n) => {
                tmp = format!("Decimal point in Base {n} Literal");
                &tmp
            }
        }
        .to_string();
        write!(f, "{}", s)?;
        Ok(())
    }
}

impl Error for LexerTokenError {}
