use anyhow::Result;

use crate::token::stream::TokenStream;

use self::token::{LexerErr, LexerToken, LexerTokenError};

pub mod token;

pub struct Lexer {
    file: String,
    tokens: TokenStream<LexerToken>,
    errors: Vec<(usize, LexerErr)>,
}

impl Lexer {
    pub fn from_string(contents: String) -> Self {
        Self {
            file: contents,
            tokens: TokenStream::default(),
            errors: Vec::default(),
        }
    }

    pub fn lex(self) -> Result<LexResult> {
        Ok(LexResult {
            tokens: self.tokens,
            errors: self.errors,
        })
    }

    fn read_next_token(&mut self) -> Result<LexerToken, (LexerToken, LexerTokenError)> {





        Ok(())
    }
}

pub struct LexResult {
    tokens: TokenStream<LexerToken>,
    errors: Vec<(usize, LexerErr)>,
}
