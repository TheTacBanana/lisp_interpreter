use crate::{lexer::literal::NumericLiteral, token::Token};

pub type ParserToken = Token<ParserTokenKind>;

#[derive(Debug, Clone, PartialEq)]
pub enum ParserTokenKind {
    Identifier(String),
    Character(char),
    String(String),
    Numeric(Numeric),
    Boolean(bool),
    OpenBracket(char),
    CloseBracket(char)
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Numeric {
    Int(i32),
    Float(f32),
    // Complex(f32,f32) // TODO:
}

impl Numeric {
    pub fn from_literal(lit: NumericLiteral) -> Self {
        match lit {
            NumericLiteral::Float(f) => Numeric::Float(f.parse::<f32>().unwrap()),
            NumericLiteral::Dec(d) => Numeric::Int(d.parse::<i32>().unwrap()),
            NumericLiteral::Bin(b) => Numeric::Int(i32::from_str_radix(&b[2..], 2).unwrap()),
            NumericLiteral::Oct(o) => Numeric::Int(i32::from_str_radix(&o[2..], 8).unwrap()),
            NumericLiteral::Hex(x) => Numeric::Int(i32::from_str_radix(&x[2..], 16).unwrap()),
        }
    }
}
