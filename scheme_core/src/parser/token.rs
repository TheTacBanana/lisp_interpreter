use std::{
    error::Error,
    ops::{Add, Div, Mul, Sub},
};

use crate::{
    error::{ErrorWriter, FormattedError}, lexer::literal::NumericLiteral, literal::Literal, token::{span::Span, Token, TokenKind}
};

pub type ParserToken = Token<ParserTokenKind>;

#[derive(Debug, Clone, PartialEq)]
pub enum ParserTokenKind {
    Symbol(String),
    Identifier(String),
    Literal(Literal),
    String(String),
}

impl TokenKind for ParserTokenKind {}

#[derive(Debug)]
pub struct ParserError {
    pub span: Span,
    pub kind: ParseTokenError,
}

impl ParserError {
    pub fn new(kind: ParseTokenError, span: Span) -> Self {
        Self { span, kind }
    }
}

impl FormattedError for ParserError {
    fn fmt_err(&self, ew: &crate::error::ErrorWriter) -> std::fmt::Result {
        println!("Error: {}", self.kind);
        for span in ew.span_to_lines(self.span).unwrap() {
            println!("{}", ew.get_line(span.file_id, span.start.line).unwrap());
            println!("{}", ErrorWriter::underline_span(span));
        }
        Ok(())
    }
}


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
