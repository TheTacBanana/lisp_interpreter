use core::panic;

use crate::{
    lexer::token::{LexerToken, LexerTokenKind},
    token::{
        stream::{TokenStream, TokenStreamExt},
        Token,
    },
};

use self::{
    ast::AST,
    token::{Literal, ParserToken, ParserTokenKind},
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
        let mut items = Vec::new();
        while !self.tokens.is_empty() {
            let item = match Self::parse_item(&mut self.tokens) {
                Ok(item) => item,
                Err(_) => todo!(),
            };
            items.push(item);
        }
        println!("{:?}", items);
        ParseResult {  }
    }

    fn parse_block(mut stream: TokenStream<Token<ParserTokenKind>>) -> Result<AST, ()> {
        let op = Self::parse_item(&mut stream)?;
        let mut items = Vec::new();
        while !stream.is_empty() {
            let item = match Self::parse_item(&mut stream) {
                Ok(item) => item,
                Err(_) => todo!(),
            };
            items.push(item);
        }
        Ok(AST::Operation(Box::new(op), items))
    }

    fn parse_list(mut stream: TokenStream<Token<ParserTokenKind>>) -> Result<AST, ()> {
        let mut items = Vec::new();
        while !stream.is_empty() {
            let item = match Self::parse_item(&mut stream) {
                Ok(item) => item,
                Err(_) => todo!(),
            };
            items.push(item);
        }
        Ok(AST::List(items))
    }

    fn parse_item(stream: &mut TokenStream<Token<ParserTokenKind>>) -> Result<AST, ()> {
        use ParserTokenKind as TK;

        let Token { kind, span } = stream.pop_front().unwrap();
        match kind {
            TK::Literal(lit) => Ok(AST::Literal(lit, span)),
            TK::Identifier(ident) => Ok(AST::Identifier(ident, span)),

            // New block
            b @ TK::Symbol('(') => {
                let index = stream.opposite(b).unwrap(); // TODO: Parse error unleveled brackets
                let mut block = stream.take_n(index + 1).unwrap();
                block.pop_back();

                Self::parse_block(block)
            },

            // List
            // TK::Symbol()

            // Quote
            TK::Symbol('\'') => {
                let Token { kind, span } = stream.pop_front().unwrap();
                let b @ TK::Symbol('(') = kind else { return Err(()) };

                let index = stream.opposite(b).unwrap(); // TODO: Parse error unleveled brackets
                let mut block = stream.take_n(index + 1).unwrap();
                block.pop_back();

                Self::parse_list(block)
            },

            _ => Err(()),
        }
    }
}

pub struct ParseResult {}
