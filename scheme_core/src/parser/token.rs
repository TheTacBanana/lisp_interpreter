use crate::{lexer::literal::NumericLiteral, token::{Token, TokenKind}};

pub type ParserToken = Token<ParserTokenKind>;

#[derive(Debug, Clone, PartialEq)]
pub enum ParserTokenKind {
    Symbol(char),
    Identifier(String),
    Literal(Literal),
}

impl TokenKind for ParserTokenKind { }

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
    Character(char),
    Numeric(Numeric),
    Boolean(bool),
}

impl Literal {
    pub fn from_bool(b: bool) -> Self {
        Literal::Boolean(b)
    }

    pub fn from_char(c: char) -> Self {
        Literal::Character(c)
    }

    pub fn from_string(s: String) -> Self {
        Literal::String(s)
    }

    pub fn from_numeric(lit: NumericLiteral) -> Self {
        Literal::Numeric(Numeric::from_literal(lit))
    }
}

impl Into<ParserTokenKind> for Literal {
    fn into(self) -> ParserTokenKind {
        ParserTokenKind::Literal(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Numeric {
    Int(i32),
    Float(f32),
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
