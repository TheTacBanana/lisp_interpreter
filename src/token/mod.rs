use self::span::Span;

pub mod span;
pub mod stream;

#[derive(Debug, Clone)]
pub struct Token<T> {
    pub kind: T,
    pub span: Span,
}

pub struct ErrorToken<E> {
    pub reason: E,
    pub span: Span,
}