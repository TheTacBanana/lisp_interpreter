/// Implements the rules of the language
pub struct Rules;

impl Rules {
    pub fn delimiter(ch: char) -> bool {
        match ch {
            '(' | ')' | '[' | ']' | '"' | ';' | '#' => true,
            x if Rules::whitespace(x) => true,
            _ => false,
        }
    }

    pub fn whitespace(ch: char) -> bool {
        ch.is_whitespace()
    }

    pub fn line_break(ch: char) -> bool {
        match ch {
            '\n' | '\r' => true,
            _ => false,
        }
    }

    // TODO: Inline hex escape
    // TODO: Actual character checking?
    pub fn start_identifier(ch: char) -> bool {
        !ch.is_control() && !ch.is_numeric()
    }

    pub fn identifier(ch: char) -> bool {
        !ch.is_control() && !Rules::delimiter(ch)
    }

    pub fn start_boolean(ch: char) -> bool {
        ch == '#'
    }

    pub fn boolean(ch: char) -> bool {
        match ch {
            't' | 'T' | 'f' | 'F' => true,
            _ => false,
        }
    }

    pub fn start_numeric(ch: char, ch2: Option<char>) -> bool {
        match (ch, ch2) {
            ('#', Some('b' | 'B' | 'o' | 'O' | 'd' | 'D' | 'x' | 'X')) => true,
            ('0'..='9', Some('.')) => true,
            ('0'..='9', Some(r)) if Rules::delimiter(r) => true,
            ('0'..='9', Some('0'..='9' | 'a'..='f' | 'A'..='F')) => true,
            ('0'..='9', None) => true,
            _ => false,
        }
    }

    pub fn start_character(ch: char) -> bool {
        ch == '#'
    }

    pub fn character_name(name: String) -> bool {
        match name.as_str() {
            "nul" | "alarm" | "backspace" | "tab" | "linefeed" | "newline" | "vtab" | "page"
            | "return" | "esc" | "space" | "delete" => true,
            _ => false,
        }
    }

    pub fn character(_ch: char) -> bool {
        true
    }

    pub fn start_string(ch: char) -> bool {
        ch == '"'
    }

    pub fn string(ch: char) -> bool {
        match ch {
            '"' | '\\' => false,
            _ => true,
        }
    }

    pub fn escaped_char(ch: char) -> bool {
        match ch {
            'a' | 'b' | 't' | 'n' | 'v' | 'f' | 'r' | '"' | '\\' => true,
            _ => false,
        }
    }

    pub fn start_symbol(ch: char) -> bool {
        match ch {
            '(' | ')' | '[' | ']' | '#' | '\'' | '`' | ',' | '.' => true,
            _ => false,
        }
    }

    pub fn symbol(ch: char) -> bool {
        match ch {
           '#' | '\'' | '`' | ',' | '.' => true,
            _ => false,
        }
    }

    pub fn start_comment(ch: char) -> bool {
        ch == ';'
    }
}
