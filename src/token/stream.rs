use std::collections::VecDeque;

use super::Token;

pub type TokenStream<T> = VecDeque<T>;

pub trait TokenStreamExt {

}

impl<T : Clone> TokenStreamExt for TokenStream<Token<T>> {

}