use crate::rules::Rules;

/// Describes the Position of a Token in a File
/// Inclusive of both sides [start-end]
#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: LineCol,
    pub end: LineCol,
}

impl Span {
    /// Span for a single character
    pub fn single(pos: LineCol) -> Self {
        Self {
            start: pos,
            end: pos,
        }
    }

    /// Extends the span with a sequence of characters
    pub fn extend_with(&mut self, seq: String){
        for ch in seq.chars() {
            if Rules::line_break(ch) {
                self.end.line = 0;
                self.end.col += 1;
            } else {
                self.end.col += 1;
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LineCol {
    pub line: usize,
    pub col: usize
}
