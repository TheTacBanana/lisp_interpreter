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
        !ch.is_control()
    }

    pub fn identifier(ch: char) -> bool {
        !ch.is_control()
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

    pub fn start_numeric(ch: char) -> bool {
        match ch {
            '0'..='9' | '#' => true,
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
            _ => true
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
}
