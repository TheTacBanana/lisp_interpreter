/// Implements the rules of the language
pub struct Rules;

impl Rules {
    pub fn delimiter(ch: char) -> bool {
        match ch {
            '(' | ')' | '[' | ']' | '"' | ';' | '#' => true,
            x if Rules::whitespace(x) => true,
            _ => false
        }
    }

    pub fn whitespace(ch: char) -> bool {
        ch.is_whitespace()
    }

    pub fn line_break(ch: char) -> bool {
        match ch {
            '\n' | '\r' => true,
            _ => false
        }
    }
}
