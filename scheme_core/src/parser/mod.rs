use core::panic;

use crate::{
    lexer::token::{LexerToken, LexerTokenKind}, literal::Literal, token::{
        span::Span,
        stream::{TokenStream, TokenStreamExt},
        Token,
    }
};

use self::{ast::AST, token::{ParseTokenError, ParserError, ParserTokenKind}};

pub mod ast;
pub mod token;

pub struct Parser {
    pub tokens: TokenStream,
}

impl Parser {
    pub fn new(mut tokens_in: Vec<LexerToken>) -> Self {
        let tokens = tokens_in
            .drain(..)
            .fold(TokenStream::default(), |mut l, r| {
                let Token { kind, span } = r;

                let token = match kind {
                    LexerTokenKind::Boolean(b) => {
                        let b = match b.chars().nth(1).unwrap() {
                            't' | 'T' => true,
                            'f' | 'F' => false,
                            _ => panic!(),
                        };
                        Some(Literal::from_bool(b).into())
                    }
                    LexerTokenKind::Numeric(nl) => Some(Literal::from_numeric(nl).into()),
                    LexerTokenKind::Character(ch) => {
                        Some(Literal::from_char(ch.chars().nth(2).unwrap()).into())
                    }
                    LexerTokenKind::Identifer(i) => Some(ParserTokenKind::Identifier(i)),
                    LexerTokenKind::String(s) => {
                        Some(ParserTokenKind::String(s[1..(s.len() - 1)].to_string()).into())
                    }
                    LexerTokenKind::Symbol(s) => {
                        Some(ParserTokenKind::Symbol(s.chars().next().unwrap()))
                    }
                    _ => None,
                };

                if let Some(token) = token {
                    l.push_back(Token { kind: token, span });
                };
                l
            });

        Parser { tokens }
    }

    pub fn parse(mut self) -> ParseResult {
        let mut errors = Vec::new();
        let mut items = Vec::new();
        match self.tokens.peek_front().unwrap() {
            ParserTokenKind::Symbol(_)
            | ParserTokenKind::Literal(_)
            | ParserTokenKind::String(_) => {
                while !self.tokens.is_empty() {
                    match Self::parse_item(&mut self.tokens) {
                        Ok(item) => items.push(item),
                        Err(e) => errors.push(e),
                    };
                }
            }
            ParserTokenKind::Identifier(_) => match Self::parse_block(self.tokens) {
                Ok(item) => items.push(item),
                Err(e) => errors.push(e),
            },
        };
        ParseResult { ast: items, errors }
    }

    fn parse_item(stream: &mut TokenStream) -> Result<AST, ParserError> {
        use ParserTokenKind as TK;

        let Token { kind, span } = stream.pop_front().unwrap();
        match kind {
            TK::Literal(lit) => Ok(AST::Literal(lit, span)),
            TK::Identifier(ident) => Ok(AST::Identifier(ident, span)),
            TK::String(s) => Ok(AST::StringLiteral(s, span)),

            // New block
            b @ TK::Symbol('(') => {
                let index = match stream.opposite(b) {
                    Ok(index) => index,
                    Err(_) => Err(ParserError::new(ParseTokenError::MissingBracket, span))?,
                };
                let mut block = stream.take_n(index + 1).unwrap();
                block.pop_back();

                Self::parse_block(block)
            }

            // Quote
            TK::Symbol('\'') => Self::parse_quoted(stream),

            _ => panic!("No item found"),
        }
    }

    fn parse_block(mut stream: TokenStream) -> Result<AST, ParserError> {
        let total_span = stream.total_span().unwrap(); // TODO:
        let item = Self::parse_item(&mut stream)?;

        if stream.is_empty() {
            return Ok(item);
        }

        let mut items = Vec::new();
        while !stream.is_empty() {
            let item = Self::parse_item(&mut stream)?;
            items.push(item);
        }
        Ok(AST::Operation(Box::new(item), items, total_span))
    }

    fn parse_quoted(stream: &mut TokenStream) -> Result<AST, ParserError> {
        let Token { kind, span } = stream.pop_front().unwrap();
        match kind {
            b @ ParserTokenKind::Symbol('(') => {
                let index = match stream.opposite(b) {
                    Ok(index) => index,
                    Err(_) => Err(ParserError::new(ParseTokenError::MissingBracket, span))?,
                };
                let mut block = stream.take_n(index + 1).unwrap();
                let last = block.pop_back().unwrap();

                Self::parse_list(block, span.max_span(last.span))
            }
            ParserTokenKind::Identifier(ident) => Ok(AST::Identifier(ident, span)),
            ParserTokenKind::Literal(lit) => Ok(AST::Literal(lit, span)),
            _ => panic!("No Item Found"),
        }
    }

    fn parse_list(mut stream: TokenStream, span: Span) -> Result<AST, ParserError> {
        let mut items = Vec::new();
        while !stream.is_empty() {
            let item = Self::parse_item(&mut stream)?;
            items.push(item);
        }
        if items.is_empty() {
            Ok(AST::EmptyList(span))
        } else {
            Ok(AST::list_from_vec(items.into()))
        }
    }
}

#[derive(Debug)]
pub struct ParseResult {
    pub ast: Vec<AST>,
    pub errors: Vec<ParserError>,
}
