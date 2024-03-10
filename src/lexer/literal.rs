use super::token::LexerTokenError;

#[derive(Debug, Clone, PartialEq, Eq)]
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

    pub fn new(first: char, second: Option<char>) -> Self {
        let s = first.to_string();
        match (first, second) {
            ('#', Some('b' | 'B')) => NumericLiteral::Bin(s),
            ('#', Some('o' | 'O')) => NumericLiteral::Oct(s),
            ('#', Some('d' | 'D')) => NumericLiteral::Dec(s),
            ('#', Some('x' | 'X')) => NumericLiteral::Hex(s),

            ('0'..='9', Some('.')) => NumericLiteral::Float(s),
            ('0'..='9', _) => NumericLiteral::Dec(s),
            _ => panic!(),
        }
    }

    pub fn push_to_inner(mut self, ch: char) -> Result<Self, (Self, LexerTokenError)> {
        let mut char_error = None;
        let len = self.inner_mut().len();

        self = match (self, len, ch) {
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
            (Self::Float(s) | Self::Dec(s), n, '.') if n > 0 => {
                if s.contains('.') {
                    char_error = Some(LexerTokenError::MultiplePointsInFloat);
                }
                Self::Float(s)
            }
            (Self::Float(s), _, _) => {
                char_error = Some(LexerTokenError::InvalidInFloat);
                Self::Float(s)
            }

            (Self::Bin(s), _,'.') => {
                char_error = Some(LexerTokenError::PointInNAryLiteral(2));
                Self::Bin(s)
            }
            (Self::Oct(s), _,'.') => {
                char_error = Some(LexerTokenError::PointInNAryLiteral(8));
                Self::Oct(s)
            }
            (Self::Hex(s), _,'.') => {
                char_error = Some(LexerTokenError::PointInNAryLiteral(16));
                Self::Hex(s)
            }
        };
        self.inner_mut().push(ch);
        match char_error {
            Some(err) => Err((self, err)),
            None => Ok(self),
        }
    }

    pub fn inner(&self) -> &String {
        match self {
            NumericLiteral::Float(s)
            | NumericLiteral::Dec(s)
            | NumericLiteral::Bin(s)
            | NumericLiteral::Oct(s)
            | NumericLiteral::Hex(s) => s,
        }
    }

    pub fn inner_mut(&mut self) -> &mut String {
        match self {
            NumericLiteral::Float(s)
            | NumericLiteral::Dec(s)
            | NumericLiteral::Bin(s)
            | NumericLiteral::Oct(s)
            | NumericLiteral::Hex(s) => s,
        }
    }

    pub fn to_string(self) -> String {
        match self {
            NumericLiteral::Float(s)
            | NumericLiteral::Dec(s)
            | NumericLiteral::Bin(s)
            | NumericLiteral::Oct(s)
            | NumericLiteral::Hex(s) => s,
        }
    }
}
