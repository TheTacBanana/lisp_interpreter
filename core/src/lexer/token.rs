use crate::{error::{ErrorWriter, FormattedError}, token::{span::Span, ErrorToken, Token}};
use std::error::Error;

use super::literal::NumericLiteral;

pub type LexerToken = Token<LexerTokenKind>;

pub type LexerErr = ErrorToken<LexerTokenKind, LexerTokenErrorKind>;

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

impl LexerTokenKind {
    pub fn inner(&self) -> &String {
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

    pub fn inner_mut(&mut self) -> &mut String {
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

    pub fn to_string(self) -> String {
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

    pub fn the_same(&self, other: &Self) -> bool {
        use std::mem::discriminant;
        discriminant(self) == discriminant(other)
    }

    pub fn push_to_inner(&mut self, ch: char) {
        self.inner_mut().push(ch);
    }
}

#[derive(Debug)]
pub struct LexerError {
    pub span: Span,
    pub kind: LexerTokenErrorKind,
}

impl LexerError {
    pub fn new(kind: LexerTokenErrorKind, span: Span) -> Self {
        Self { span, kind }
    }
}

impl FormattedError for LexerError {
    fn message(&self) -> String {
        self.kind.to_string()
    }

    fn span(&self) -> Option<Span> {
        Some(self.span)
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum LexerTokenErrorKind {
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
    InvalidInNumericLiteral,
}

impl std::fmt::Display for LexerTokenErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tmp;
        let s = match self {
            LexerTokenErrorKind::EscapeCharacterExpected => "Escape character expected after '\\'",
            LexerTokenErrorKind::EOFInStringLiteral => "End of file encountered in String Literal",
            LexerTokenErrorKind::InvalidInFloat => "Invalid character in Floating Point Number",
            LexerTokenErrorKind::MultiplePointsInFloat => {
                "Multiple decimal points in Floating Point Number"
            }
            LexerTokenErrorKind::InvalidInNAryLiteral(n) => {
                tmp = format!("Invalid character in Base {n} Literal");
                &tmp
            }
            LexerTokenErrorKind::PointInNAryLiteral(n) => {
                tmp = format!("Decimal point in Base {n} Literal");
                &tmp
            }
            LexerTokenErrorKind::InvalidInNumericLiteral => "Invalid character in Numeric Literal",
        }
        .to_string();
        write!(f, "{}", s)?;
        Ok(())
    }
}

impl Error for LexerTokenErrorKind {}
