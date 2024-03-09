use crate::rules::Rules;

/// Describes the Position of a Token in a File
/// Inclusive of both sides [start-end]
#[derive(Debug, Clone, Copy)]
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

    /// Extends the span with a sequence of characters
    pub fn extend_with(&mut self, seq: String) {
        self.end.extend_with(seq);
    }
}

#[derive(Debug, Clone, Copy)]
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
                self.line = 0;
                self.col += 1;
            } else {
                self.col += 1;
            }
        }
    }
}
