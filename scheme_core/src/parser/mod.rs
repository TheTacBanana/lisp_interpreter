use core::panic;

use crate::{
    lexer::token::{LexerToken, LexerTokenKind},
    literal::Literal,
    token::{
        span::Span,
        stream::{TokenStream, TokenStreamExt},
        Token,
    },
};

use self::{
    ast::AST,
    token::{ParseTokenError, ParserError, ParserTokenKind},
};

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
        if self.tokens.is_empty() {
            return ParseResult {
                ast: Vec::new(),
                errors: Vec::new(),
            };
        }

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
            ParserTokenKind::Identifier(_) => {
                let total_span = self.tokens.total_span().unwrap();
                match Self::parse_block(self.tokens, total_span) {
                    Ok(item) => items.push(item),
                    Err(e) => errors.push(e),
                }
            }
        };
        ParseResult { ast: items, errors }
    }

    fn parse_item(stream: &mut TokenStream) -> Result<AST, ParserError> {
        use ParserTokenKind as TK;

        let Token {
            kind,
            span: first_span,
        } = stream.pop_front().unwrap();
        match kind {
            TK::Literal(lit) => Ok(AST::Literal(lit, first_span)),
            TK::Identifier(ident) => Ok(AST::Identifier(ident, first_span)),
            TK::String(s) => Ok(AST::StringLiteral(s, first_span)),

            // New block
            b @ TK::Symbol('(') => {
                let index = match stream.opposite(b) {
                    Ok(index) => index,
                    Err(_) => Err(ParserError::new(
                        ParseTokenError::MissingBracket,
                        first_span,
                    ))?,
                };
                let mut block = stream.take_n(index + 1).unwrap();
                let end_token = block.pop_back().unwrap();
                let total_span = first_span.max_span(end_token.span);

                Self::parse_block(block, total_span)
            }

            // Quote
            TK::Symbol('\'') => Self::parse_quoted(stream),

            _ => Err(ParserError::new(ParseTokenError::NoItemFound, first_span)),
        }
    }

    fn parse_block(mut stream: TokenStream, span: Span) -> Result<AST, ParserError> {
        if stream.is_empty() {
            Err(ParserError::new(ParseTokenError::EmptyBlock, span))?;
        }

        let item = Self::parse_item(&mut stream)?;
        if stream.is_empty() {
            return Ok(item);
        }

        let mut items = Vec::new();
        while !stream.is_empty() {
            let item = Self::parse_item(&mut stream)?;
            items.push(item);
        }
        Ok(AST::Operation(Box::new(item), items, span))
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
