use std::{error::Error, str::Lines, sync::atomic::AtomicPtr};

use crate::{lexer::token::LexerTokenError, token::{span::Span, ErrorToken, Token, TokenKind}};

pub struct ErrorWriter {
    pub lines: Vec<String>,
}

impl ErrorWriter {
    pub fn from_string(string: &str) -> Self {
        Self {
            lines: string
                .to_string()
                .lines()
                .map(|x| x.into())
                .collect::<Vec<_>>(),
        }
    }

    pub fn report_errors(&self, errors : Vec<impl FormattedError>) -> Result<(), ()>{
        if errors.is_empty() {
            return Ok(())
        }
        for e in errors {
            e.fmt_err(self).unwrap();
        }

        Err(())
    }

    pub fn get_line(&self, l: usize) -> Option<&String> {
        self.lines.get(l)
    }

    pub fn get_line_length(&self, l: usize) -> Option<usize> {
        self.lines.get(l).map(|l| l.len())
    }

    pub fn span_to_lines(&self, span: Span) -> Option<Vec<Span>> {
        let mut lines = span.lines().collect::<Vec<_>>();

        if lines.len() == 1 {
            Some(vec![span])
        } else {
            let mut lines = lines.drain(..);
            let mut spans = Vec::new();

            let first = lines.next().unwrap();
            spans.push(Span::from_to_on(
                span.start.col,
                self.get_line_length(first)?,
                first,
            ));

            for _ in 0..(lines.len() - 1) {
                let line = lines.next().unwrap();
                spans.push(Span::from_to_on(0, self.get_line_length(line)?, line))
            }

            let last = lines.next().unwrap();
            spans.push(Span::from_to_on(
                0,
                span.end.col,
                last,
            ));

            Some(spans)
        }
    }

    pub fn underline_span(span: Span) -> String {
        let padding = 0..span.start.col;
        let width = span.start.col..=span.end.col;
        let padding = padding.fold(String::new(), |l, _r| l + " ");
        let out = width.fold(padding, |l, _r| l + "^");
        out
    }
}

pub trait FormattedError {
    fn fmt_err(&self, f: &ErrorWriter) -> std::fmt::Result;
}
