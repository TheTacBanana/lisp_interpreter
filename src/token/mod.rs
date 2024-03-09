use std::ops::{Deref, DerefMut};

use self::span::Span;

pub mod span;
pub mod stream;

#[derive(Debug, Clone)]
pub struct Token<T: TokenKind> {
    pub kind: T,
    pub span: Span,
}

pub trait TokenKind {
    fn inner_mut(&mut self) -> &mut String;
    fn push_to_inner(&mut self, ch: char) {
        self.inner_mut().push(ch);
    }
    fn the_same(&self, other: &Self) -> bool;
}

pub struct ErrorToken<T: TokenKind, E> {
    pub token: Token<T>,
    pub err: E,
}

impl<T: TokenKind> Token<T> {
    pub fn inner(&self) -> &T {
        &self.kind
    }

    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.kind
    }

    pub fn with_error<E>(self, err: E) -> ErrorToken<T, E> {
        ErrorToken {
            token: self,
            err,
        }
    }
}

impl<T: TokenKind> Deref for Token<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

impl<T: TokenKind> DerefMut for Token<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.kind
    }
}

impl<T: TokenKind, E> ErrorToken<T, E> {
    pub fn unpack(self) -> (Token<T>, E) {
        (self.token, self.err)
    }
}
