use std::{error::Error, ops::{Add, Div, Mul, Sub}};

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

    pub fn is_truthy(&self) -> bool {
        match self {
            Literal::Boolean(false) => false,
            _ => true
        }
    }
}

impl Into<ParserTokenKind> for Literal {
    fn into(self) -> ParserTokenKind {
        ParserTokenKind::Literal(self)
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::String(s) => write!(f, "\"{s}\"")?,
            Literal::Character(c) => write!(f, "'{c}'")?,
            Literal::Numeric(n) => write!(f, "{n}")?,
            Literal::Boolean(true) => write!(f, "true")?,
            Literal::Boolean(false) => write!(f, "false")?,
        }
        Ok(())
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

impl std::fmt::Display for Numeric {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Numeric::Int(i) => write!(f, "{i}")?,
            Numeric::Float(fl) => write!(f, "{fl}")?,
        }
        Ok(())
    }
}

impl Add<Numeric> for Numeric {
    type Output = Numeric;

    fn add(self, rhs: Numeric) -> Self::Output {
        match (self, rhs) {
            (Numeric::Int(l), Numeric::Int(r)) => Numeric::Int(l + r),
            (Numeric::Int(l), Numeric::Float(r)) => Numeric::Float(l as f32 + r),
            (Numeric::Float(l), Numeric::Int(r)) => Numeric::Float(l + r as f32),
            (Numeric::Float(l), Numeric::Float(r)) => Numeric::Float(l + r),
        }
    }
}

impl Sub<Numeric> for Numeric {
    type Output = Numeric;

    fn sub(self, rhs: Numeric) -> Self::Output {
        match (self, rhs) {
            (Numeric::Int(l), Numeric::Int(r)) => Numeric::Int(l - r),
            (Numeric::Int(l), Numeric::Float(r)) => Numeric::Float(l as f32 - r),
            (Numeric::Float(l), Numeric::Int(r)) => Numeric::Float(l - r as f32),
            (Numeric::Float(l), Numeric::Float(r)) => Numeric::Float(l - r),
        }
    }
}

impl Mul<Numeric> for Numeric {
    type Output = Numeric;

    fn mul(self, rhs: Numeric) -> Self::Output {
        match (self, rhs) {
            (Numeric::Int(l), Numeric::Int(r)) => Numeric::Int(l * r),
            (Numeric::Int(l), Numeric::Float(r)) => Numeric::Float(l as f32 * r),
            (Numeric::Float(l), Numeric::Int(r)) => Numeric::Float(l * r as f32),
            (Numeric::Float(l), Numeric::Float(r)) => Numeric::Float(l * r),
        }
    }
}

impl Div<Numeric> for Numeric {
    type Output = Numeric;

    fn div(self, rhs: Numeric) -> Self::Output {
        match (self, rhs) {
            (Numeric::Int(l), Numeric::Int(r)) => Numeric::Int(l / r),
            (Numeric::Int(l), Numeric::Float(r)) => Numeric::Float(l as f32 / r),
            (Numeric::Float(l), Numeric::Int(r)) => Numeric::Float(l / r as f32),
            (Numeric::Float(l), Numeric::Float(r)) => Numeric::Float(l / r),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ParseTokenError {
    /// Unmatched Bracket
    UnmatchedBrackets,
    /// Missing bracket
    MissingBracket,
}

impl Error for ParseTokenError {}

impl std::fmt::Display for ParseTokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // let tmp;
        let s = match self {
            ParseTokenError::UnmatchedBrackets => "Mismatched closing bracket",
            ParseTokenError::MissingBracket => "Missing closing Bracket",
        }
        .to_string();
        write!(f, "{}", s)?;
        Ok(())
    }
}
