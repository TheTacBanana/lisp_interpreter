use std::collections::VecDeque;

use super::{Token, TokenKind};

pub type TokenStream<T> = VecDeque<T>;

pub trait TokenStreamExt {

}

impl<T : TokenKind + Clone> TokenStreamExt for TokenStream<Token<T>> {

}