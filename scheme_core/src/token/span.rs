use crate::rules::Rules;

/// Describes the Position of a Token in a File
/// Inclusive of both sides [start-end]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    pub start: LineCol,
    pub end: LineCol,
}

impl Span {
    /// Span for first character in a new file
    pub const fn zero() -> Self {
        Self {
            start: LineCol { line: 0, col: 0 },
            end: LineCol { line: 0, col: 0 },
        }
    }

    /// Span for a single character
    pub const fn single(pos: LineCol) -> Self {
        Self {
            start: pos,
            end: pos,
        }
    }

    pub const fn from_to_on(from : usize, to: usize, on: usize) -> Self {
        Self {
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
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
