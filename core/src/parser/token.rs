use std::{
    error::Error,
};

use crate::{
    error::{ErrorWriter, FormattedError, LispError}, literal::Literal, token::{span::Span, Token}
};

pub type ParserToken = Token<ParserTokenKind>;

#[derive(Debug, Clone, PartialEq)]
pub enum ParserTokenKind {
    Symbol(String),
    Identifier(String),
    Literal(Literal),
    String(String),
}

pub type ParserError = LispError<ParseTokenError>;

#[derive(Debug, Clone, Copy)]
pub enum ParseTokenError {
    /// Unmatched Bracket
    UnmatchedBrackets,
    /// Missing bracket
    MissingBracket,
    /// No parsable item found in Block
    NoItemFound,
    /// Block does not contain any items
    EmptyBlock,

    QuoteWithoutItem,

    ItemCannotBeQuoted,
}

impl Error for ParseTokenError {}

impl std::fmt::Display for ParseTokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // let tmp;
        let s = match self {
            ParseTokenError::UnmatchedBrackets => "Mismatched closing bracket",
            ParseTokenError::MissingBracket => "Missing closing Bracket",
            ParseTokenError::NoItemFound => "No parseable item found",
            ParseTokenError::EmptyBlock => "Block is empty",
            ParseTokenError::QuoteWithoutItem => "No Item found to quote",
            ParseTokenError::ItemCannotBeQuoted => "Item cannot be quoted",
        }
        .to_string();
        write!(f, "{}", s)?;
        Ok(())
    }
}
