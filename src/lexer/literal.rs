use super::token::LexerTokenError;

#[derive(Debug, Clone)]
pub enum NumericLiteral {
    Float(String),
    Dec(String),
    Bin(String),
    Oct(String),
    Hex(String),
}

impl NumericLiteral {
    pub fn start_numeric_literal(ch: char) -> bool {
        match ch {
            '#' | '0'..='9' => true,
            _ => false,
        }
    }

    pub fn new(ch: char) -> Self {
        let s = ch.to_string();
        match ch {
            '#' => NumericLiteral::Bin(s),
            '0'..='9' => NumericLiteral::Dec(s),
            _ => panic!(),
        }
    }

    pub fn push_to_inner(&mut self, ch: char) -> Result<(), LexerTokenError> {
        let mut char_error = None;

        let mut next = match (*self, self.inner().len(), ch) {
            (Self::Bin(s), 1, 'b' | 'B') => Self::Bin(s),
            (Self::Bin(s), 1, 'o' | 'O') => Self::Oct(s),
            (Self::Bin(s), 1, 'd' | 'D') => Self::Dec(s),
            (Self::Bin(s), 1, 'x' | 'X') => Self::Hex(s),

            (Self::Bin(s), _, '0' | '1') => Self::Bin(s),
            (Self::Bin(s), _, _) => {
                char_error = Some(LexerTokenError::InvalidInNAryLiteral(2));
                Self::Bin(s)
            }
            (Self::Oct(s), _, '0'..='7') => Self::Oct(s),
            (Self::Oct(s), _, _) => {
                char_error = Some(LexerTokenError::InvalidInNAryLiteral(8));
                Self::Oct(s)
            }
            (Self::Dec(s), _, '0'..='9') => Self::Dec(s),
            (Self::Dec(s), _, _) => {
                char_error = Some(LexerTokenError::InvalidInNAryLiteral(10));
                Self::Dec(s)
            }

            (Self::Hex(s), _, '0'..='9' | 'a'..='f' | 'A'..='F') => Self::Hex(s),
            (Self::Hex(s), _, _) => {
                char_error = Some(LexerTokenError::InvalidInNAryLiteral(16));
                Self::Hex(s)
            }

            (Self::Float(s), _, '0'..='9') => Self::Float(s),
            (Self::Float(s), n, '.') if n > 0 => {
                if s.contains('.') {
                    char_error = Some(LexerTokenError::MultiplePointsInFloat);
                }
                Self::Float(s)
            }
            (Self::Float(s), _, _) => {
                char_error = Some(LexerTokenError::InvalidInFloat);
                Self::Float(s)
            }
        };
        next.inner().push(ch);
        if let Some(err) = char_error {
            Err(err)
        } else {
            Ok(())
        }
    }

    pub fn inner(&mut self) -> &mut String {
        match self {
            NumericLiteral::Float(s)
            | NumericLiteral::Dec(s)
            | NumericLiteral::Bin(s)
            | NumericLiteral::Oct(s)
            | NumericLiteral::Hex(s) => s,
        }
    }
}
