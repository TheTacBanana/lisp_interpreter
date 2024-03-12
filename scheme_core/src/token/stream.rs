use std::collections::VecDeque;

use crate::parser::token::ParserTokenKind;

use super::{Token, TokenKind};

pub type TokenStream<T> = VecDeque<T>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenStreamError {
    EndOfStreamEncounted,
    UnleveledBrackets,
}

pub trait TokenStreamExt<T>: Sized {
    /// Find a token matching a given predicate
    fn find(&self, f: impl Fn(&Token<T>) -> bool) -> Option<usize>;

    /// Locate the opposite of the token at the front of the stream
    fn opposite_bracket(&self) -> Result<usize, TokenStreamError>;

    fn peek_front(&self) -> Option<&T>;

    fn pop_if(&mut self, f: impl FnOnce(&mut Self) -> bool) -> Option<Token<ParserTokenKind>>;

    // Peek the first N Tokens
    fn peek_n(&self, n: usize) -> Option<Vec<&T>>;

    /// Take n tokens up till the index
    fn take_n(&mut self, n: usize) -> Option<Self>;
}

impl TokenStreamExt<ParserTokenKind> for TokenStream<Token<ParserTokenKind>> {
    fn find(&self, f: impl Fn(&Token<ParserTokenKind>) -> bool) -> Option<usize> {
        self.iter().position(|t| f(t))
    }

    fn opposite_bracket(&self) -> Result<usize, TokenStreamError> {
        use ParserTokenKind as Token;
        let opposite_func = |t: &ParserTokenKind| match t {
            Token::OpenBracket('(') => Some(Token::CloseBracket(')')),
            Token::OpenBracket('[') => Some(Token::CloseBracket(']')),
            _ => None,
        };

        let open = self
            .front()
            .map(|t| t.inner())
            .ok_or(TokenStreamError::EndOfStreamEncounted)?;

        let mut stack = vec![open];
        for (i, token) in self.iter().skip(1).enumerate() {
            let token = token.inner();
            let top = stack.last().unwrap();
            match (top, token) {
                (Token::OpenBracket('('), Token::CloseBracket(')'))
                | (Token::OpenBracket('['), Token::CloseBracket(']')) => {
                    stack.pop();
                },
                (_, b @ Token::OpenBracket(_)) => stack.push(b),
                (_, b @ Token::CloseBracket(_)) => return Err(TokenStreamError::UnleveledBrackets),
            }

            if stack.is_empty() {
                return Ok(i);
            }
        }
        return Err(TokenStreamError::EndOfStreamEncounted);
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
}

#[cfg(test)]
pub mod test {
    use crate::{
        parser::token::ParserTokenKind,
        token::{span::Span, Token},
    };

    use super::{TokenStream, TokenStreamExt};

    #[test]
    pub fn take_n() {
        let mut stream = TokenStream::default();
        stream.push_back(Token {
            kind: ParserTokenKind::Character('a'),
            span: Span::zero(),
        });
        stream.push_back(Token {
            kind: ParserTokenKind::Character('b'),
            span: Span::zero(),
        });
        stream.push_back(Token {
            kind: ParserTokenKind::Character('c'),
            span: Span::zero(),
        });

        let mut against = TokenStream::default();
        against.push_back(Token {
            kind: ParserTokenKind::Character('a'),
            span: Span::zero(),
        });
        against.push_back(Token {
            kind: ParserTokenKind::Character('b'),
            span: Span::zero(),
        });

        assert_eq!(stream.take_n(2).unwrap(), against);
        assert_eq!(
            stream.front(),
            Some(&Token {
                kind: ParserTokenKind::Character('c'),
                span: Span::zero(),
            })
        )
    }
}
