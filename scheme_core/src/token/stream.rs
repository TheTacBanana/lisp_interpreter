use std::collections::VecDeque;

use crate::parser::token::ParserTokenKind;

use super::{span::Span, Token, TokenKind};

pub type TokenStream = VecDeque<Token<ParserTokenKind>>;

// pub type TokenStream<T> = VecDeque<T>;

pub trait TokenStreamExt: Sized {
    /// Find a token matching a given predicate
    fn find(&self, f: impl Fn(&Token<ParserTokenKind>) -> bool) -> Option<usize>;

    /// Locate the opposite of the token at the front of the stream
    fn opposite(&self, token: ParserTokenKind) -> Result<usize, ()>;

    fn peek_front(&self) -> Option<&ParserTokenKind>;

    fn pop_if(&mut self, f: impl FnOnce(&mut Self) -> bool) -> Option<Token<ParserTokenKind>>;

    // Peek the first N Tokens
    fn peek_n(&self, n: usize) -> Option<Vec<&ParserTokenKind>>;

    /// Take n tokens up till the index
    fn take_n(&mut self, n: usize) -> Option<Self>;

    fn total_span(&self) -> Option<Span>;
}

impl TokenStreamExt for TokenStream {
    fn find(&self, f: impl Fn(&Token<ParserTokenKind>) -> bool) -> Option<usize> {
        self.iter().position(|t| f(t))
    }

    // TODO: Fix brackets hhh
    fn opposite(&self, token: ParserTokenKind) -> Result<usize, ()> {
        use ParserTokenKind as Token;

        let mut stack = vec!['('];
        for (i, token) in self.iter().enumerate() {
            let token = token.inner();
            let top = stack.last().unwrap();
            match (top, token) {
                ('(', Token::Symbol(s)) if s == ")" => {
                    stack.pop();
                }
                (_, Token::Symbol(s)) if s == "(" => stack.push(s.chars().next().unwrap()),
                (_, _) => (),
            }

            if stack.is_empty() {
                return Ok(i);
            }
        }
        return Err(());
    }

    fn peek_front(&self) -> Option<&ParserTokenKind> {
        self.front().map(|t| t.inner())
    }

    fn pop_if(&mut self, f: impl FnOnce(&mut Self) -> bool) -> Option<Token<ParserTokenKind>> {
        f(self).then(|| self.pop_front().unwrap())
    }

    fn peek_n(&self, n: usize) -> Option<Vec<&ParserTokenKind>> {
        if n > self.len() {
            return None;
        }
        Some(self.iter().take(n).map(|t| t.inner()).collect::<Vec<_>>())
    }

    fn take_n(&mut self, n: usize) -> Option<Self> {
        if n > self.len() {
            return None;
        }
        Some(self.drain(0..n).collect())
    }

    fn total_span(&self) -> Option<Span> {
        self.iter().map(|t| t.span).reduce(|l, r| l.max_span(r))
    }
}
