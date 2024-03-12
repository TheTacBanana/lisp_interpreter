use core::panic;

use crate::{
    lexer::token::{LexerToken, LexerTokenKind},
    token::{stream::{TokenStream, TokenStreamExt}, Token},
};

use self::{
    ast::AST,
    token::{Numeric, ParserToken, ParserTokenKind},
};

pub mod ast;
pub mod token;

macro_rules! if_peek {
    ($($params:expr),*, $b:block) => {
        if let [$($params:expr),*] = self.tokens.front().map(|t| t.inner()) {
            $block
        }
    };
}
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
                        Some(ParserTokenKind::Boolean(b))
                    }
                    LexerTokenKind::Numeric(nl) => {
                        Some(ParserTokenKind::Numeric(Numeric::from_literal(nl)))
                    }
                    LexerTokenKind::Character(ch) => {
                        Some(ParserTokenKind::Character(ch.chars().nth(2).unwrap()))
                    }
                    LexerTokenKind::Identifer(i) => Some(ParserTokenKind::Identifier(i)),
                    LexerTokenKind::String(s) => {
                        Some(ParserTokenKind::String(s[1..(s.len() - 1)].to_string()))
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

    pub fn parse(self) -> ParseResult {
        use ParserTokenKind as Token;

        loop {
            if let Some(Token::OpenBracket(_)) = self.tokens.peek_front() {
                // Find Opposite Bracket


                self.tokens.opposite_bracket();
            }
        }
        todo!()
    }

    fn parse_block(stream: TokenStream<LexerToken>) -> AST {

        if let Some(Token::OpenBracket(_)) = self.tokens.peek_front() {
            // Find Opposite Bracket
            // Take from stream
            // parse_block(new)
        }
    }
}


pub struct ParseResult {}
