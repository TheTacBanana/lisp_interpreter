use std::{hash::Hash, ops::{Add, Div, Mul, Sub}};

use crate::{lexer::literal::NumericLiteral, parser::token::ParserTokenKind};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum Literal {
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

    pub fn from_numeric(lit: NumericLiteral) -> Option<Self> {
        Some(Literal::Numeric(Numeric::from_literal(lit)?))
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Literal::Boolean(false) => false,
            _ => true,
        }
    }
}

impl From<Literal> for ParserTokenKind {
    fn from(val: Literal) -> Self {
        ParserTokenKind::Literal(val)
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Character(c) => write!(f, "'{c}'"),
            Literal::Numeric(n) => write!(f, "{n}"),
            Literal::Boolean(true) => write!(f, "true"),
            Literal::Boolean(false) => write!(f, "false"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Numeric {
    Int(i32),
    Float(f32),
}

impl Hash for Numeric {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            Numeric::Int(i) => i.hash(state),
            Numeric::Float(f) => f.to_bits().hash(state)
        }
    }
}

impl Numeric {
    pub fn from_literal(lit: NumericLiteral) -> Option<Self> {
        Some(match lit {
            NumericLiteral::Float(f) => Numeric::Float(f.parse::<f32>().ok()?),
            NumericLiteral::Dec(d) => Numeric::Int(d.parse::<i32>().ok()?),
            NumericLiteral::Bin(b) => Numeric::Int(i32::from_str_radix(&b[2..], 2).ok()?),
            NumericLiteral::Oct(o) => Numeric::Int(i32::from_str_radix(&o[2..], 8).ok()?),
            NumericLiteral::Hex(x) => Numeric::Int(i32::from_str_radix(&x[2..], 16).ok()?),
        })
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
            (Numeric::Int(l), Numeric::Int(r)) => Numeric::Float(l as f32 / r as f32),
            (Numeric::Int(l), Numeric::Float(r)) => Numeric::Float(l as f32 / r),
            (Numeric::Float(l), Numeric::Int(r)) => Numeric::Float(l / r as f32),
            (Numeric::Float(l), Numeric::Float(r)) => Numeric::Float(l / r),
        }
    }
}

impl PartialOrd<Numeric> for Numeric {
    fn partial_cmp(&self, other: &Numeric) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Numeric::Int(l), Numeric::Int(r)) => Some(l.cmp(r)),
            (Numeric::Int(l), Numeric::Float(r)) => Some((*l as f32).total_cmp(r)),
            (Numeric::Float(l), Numeric::Int(r)) => Some(l.total_cmp(&(*r as f32))),
            (Numeric::Float(l), Numeric::Float(r)) => Some(l.total_cmp(r)),
        }
    }
}

impl Eq for Numeric {

}

impl Ord for Numeric {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Numeric::Int(l), Numeric::Int(r)) => l.cmp(r),
            (Numeric::Int(l), Numeric::Float(r)) => (*l as f32).total_cmp(r),
            (Numeric::Float(l), Numeric::Int(r)) => l.total_cmp(&(*r as f32)),
            (Numeric::Float(l), Numeric::Float(r)) => l.total_cmp(r),
        }
    }
}