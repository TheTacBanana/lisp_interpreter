use crate::{parser::ast::AST, rules::Rules};

/// Describes the Position of a Token in a File
/// Inclusive of both sides [start-end]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub file_id: usize,
    pub start: LineCol,
    pub end: LineCol,
}

impl Span {
    /// Span for first character in a new file
    pub const fn zero(file_id: usize) -> Self {
        Self {
            file_id,
            start: LineCol { line: 0, col: 0 },
            end: LineCol { line: 0, col: 0 },
        }
    }

    /// Span for a single character
    pub const fn single(file_id: usize, pos: LineCol) -> Self {
        Self {
            file_id,
            start: pos,
            end: pos,
        }
    }

    pub const fn from_to_on(file_id: usize, from : usize, to: usize, on: usize) -> Self {
        Self {
            file_id,
            start: LineCol { line: on, col: from },
            end: LineCol { line: on, col: to },
        }
    }

    /// Extends the span with a sequence of characters
    pub fn extend_with(&mut self, seq: String) {
        self.end.extend_with(seq);
    }

    pub fn lines(&self) -> impl Iterator<Item = usize> {
        self.start.line..=self.end.line
    }

    pub fn max_span(&self, other: Span) -> Span {
        assert!(self.file_id == other.file_id);
        Span {
            file_id: self.file_id,
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LineCol {
    pub line: usize,
    pub col: usize,
}

impl LineCol {
    pub const fn zero() -> Self {
        LineCol { line: 0, col: 0 }
    }

    /// Extends the Pos with a sequence of characters
    pub fn extend_with(&mut self, seq: String) {
        for ch in seq.chars() {
            if Rules::line_break(ch) {
                self.col = 0;
                self.line += 1;
            } else {
                self.col += 1;
            }
        }
    }
}

pub trait TotalSpan {
    fn total_span(&self) -> Option<Span>;
}


impl TotalSpan for Vec<&AST> {
    fn total_span(&self) -> Option<Span> {
        self.iter().map(|a| a.span()).reduce(|l, r| l.max_span(r))
    }
}