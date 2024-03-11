use core::panic;

use crate::{
    lexer::token::{LexerToken, LexerTokenKind},
    token::stream::TokenStream,
};

use self::token::{Numeric, ParserToken, ParserTokenKind};

pub mod ast;
pub mod token;

pub struct Parser {
    pub tokens: TokenStream<ParserToken>,
}

impl Parser {
    pub fn new(mut tokens_in: TokenStream<LexerToken>) -> Self {
        let tokens = tokens_in.drain(..).fold(TokenStream::default(), |l, r| {
            match r.kind {
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
                LexerTokenKind::Identifer(i) => {
                    Some(ParserTokenKind::Identifier(i))
                },
                LexerTokenKind::String(s) => {
                    Some(ParserTokenKind::String(s[1..(s.len() - 1)].to_string()))
                },
                _ => None,
            };
            l
        });

        Parser { tokens }
    }
}
