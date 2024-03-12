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

    pub fn parse(self) -> ParseResult {
        use ParserTokenKind as Token;

        loop {
            if let Some(Token::Symbol('(')) = self.tokens.peek_front() {
                // Find Opposite Bracket
                // Take from stream
                // Remove back and front bracket
                // Parse block
                self.tokens.opposite_bracket();
            }
        }
        todo!()
    }

    fn parse_block(stream: TokenStream<Token<ParserTokenKind>>) -> AST {
        use ParserTokenKind as TK;

        let items = Vec::new();

        let take_next = || {
            if let Some(TokenKind::Symbol('(' | '[')) = stream.peek_front() {
                take_next
            }

            if let Some(t) =
                stream.pop_if(|s| matches!(s.peek_front(), Some(TK::Identifier(_))))
            {
                let Token {
                    kind: TK::Identifier(ident),
                    span,
                } = t
                else {
                    unreachable!()
                };

                return Ok(AST::Identifier(ident, span));
            }

            if let Some(t) =
                stream.pop_if(|s| matches!(s.peek_front(), Some(TK::Literal(_))))
            {
                let Token {
                    kind: TK::Literal(lit),
                    span,
                } = t
                else {
                    unreachable!()
                };

                return Ok(AST::Literal(lit, span))
            }
            Err(())
        };
    }

    fn parse_list(strean: TokenStream<LexerToken>) -> AST {}
}

pub struct ParseResult {}
