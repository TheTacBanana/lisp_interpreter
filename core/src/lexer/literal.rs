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
            ('-', Some('0'..='9')) => NumericLiteral::Dec(s),
            ('-', Some('#')) => NumericLiteral::Dec(s),
            e => todo!("{e:?}"),
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
