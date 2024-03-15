use core::panic;

use crate::{
    error::{ErrorWriter, IndividualError},
    lexer::token::{LexerToken, LexerTokenKind},
    token::{
        span::Span,
        stream::{TokenStream, TokenStreamExt},
        Token,
    },
};

use self::{
    ast::AST,
    token::{Literal, ParseTokenError, ParserToken, ParserTokenKind},
};

pub mod ast;
pub mod token;

pub struct Parser {
    pub tokens: TokenStream<ParserToken>,
}

impl Parser {
    pub fn new(mut tokens_in: TokenStream<LexerToken>) -> Self {
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
                        Some(Literal::from_string(s[1..(s.len() - 1)].to_string()).into())
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
            ParserTokenKind::Symbol(_) | ParserTokenKind::Literal(_) => {
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

    fn parse_item(
        stream: &mut TokenStream<Token<ParserTokenKind>>,
    ) -> Result<AST, (Span, ParseTokenError)> {
        use ParserTokenKind as TK;

        let Token { kind, span } = stream.pop_front().unwrap();
        match kind {
            TK::Literal(lit) => Ok(AST::Literal(lit, span)),
            TK::Identifier(ident) => Ok(AST::Identifier(ident, span)),

            // New block
            b @ TK::Symbol('(') => {
                let index = match stream.opposite(b) {
                    Ok(index) => index,
                    Err(_) => Err((span, ParseTokenError::MissingBracket))?,
                };
                let mut block = stream.take_n(index + 1).unwrap();
                block.pop_back();

                Self::parse_block(block)
            }

            // List
            // TK::Symbol()

            // Quote
            TK::Symbol('\'') => Self::parse_quoted(stream),

            _ => panic!("No item found"),
        }
    }

    fn parse_block(
        mut stream: TokenStream<Token<ParserTokenKind>>,
    ) -> Result<AST, (Span, ParseTokenError)> {
        let item = Self::parse_item(&mut stream)?;

        if stream.is_empty() {
            return Ok(item);
        }

        let mut items = Vec::new();
        while !stream.is_empty() {
            let item = Self::parse_item(&mut stream)?;
            items.push(item);
        }
        Ok(AST::Operation(Box::new(item), items))
    }

    fn parse_quoted(
        stream: &mut TokenStream<Token<ParserTokenKind>>,
    ) -> Result<AST, (Span, ParseTokenError)> {
        let Token { kind, span } = stream.pop_front().unwrap();
        match kind {
            b @ ParserTokenKind::Symbol('(') => {
                let index = match stream.opposite(b) {
                    Ok(index) => index,
                    Err(_) => Err((span, ParseTokenError::MissingBracket))?,
                };
                let mut block = stream.take_n(index + 1).unwrap();
                block.pop_back();

                Self::parse_list(block)
            }
            ParserTokenKind::Identifier(_) => todo!(),
            ParserTokenKind::Literal(_) => todo!(),
            _ => panic!("No Item Found"),
        }
    }

    fn parse_list(
        mut stream: TokenStream<Token<ParserTokenKind>>,
    ) -> Result<AST, (Span, ParseTokenError)> {
        let mut items = Vec::new();
        while !stream.is_empty() {
            let item = Self::parse_item(&mut stream)?;
            items.push(item);
        }
        Ok(AST::List(items))
    }
}

#[derive(Debug)]
pub struct ParseResult {
    pub ast: Vec<AST>,
    pub errors: Vec<(Span, ParseTokenError)>,
}

impl ParseResult {
    pub fn error_writer(&self, file: &String) -> Option<ErrorWriter<ParseTokenError>> {
        if self.errors.len() == 0 {
            return None;
        }

        let lines = file.lines().collect::<Vec<_>>();

        let mut formatted_errors = Vec::new();
        for e in self.errors.iter() {
            let span = e.0;
            let whole_line = lines.get(span.start.line).unwrap().to_string();

            formatted_errors.push(IndividualError {
                whole_line,
                span,
                error: e.1,
            })
        }

        Some(ErrorWriter {
            errors: formatted_errors,
        })
    }
}
