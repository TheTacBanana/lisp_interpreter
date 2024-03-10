use std::error::Error;

use crate::token::{span::Span, ErrorToken, Token, TokenKind};


pub struct ErrorWriter<E: Error> {
    pub errors: Vec<IndividualError<E>>
}

impl<E: Error> ErrorWriter<E> {
    pub fn write(&self) {
        for e in self.errors.iter() {
            println!("{}", e);
        }
    }
}

pub struct IndividualError<E: Error> {
    pub whole_line: String,
    pub span: Span,
    pub error: E,
}

impl<E: Error> std::fmt::Display for IndividualError<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error: {}\n", self.error)?;
        write!(f, "\n")?;
        write!(f, "{}\n", self.whole_line)?;
        let padding = 0..self.span.start.col;
        let width = self.span.start.col..=self.span.end.col;
        let padding = padding.fold(String::new(), |l, _r| l + " ");
        let out = width.fold(padding, |l, _r| l + "^");
        write!(f, "{}\n", out)?;
        Ok(())
    }
}