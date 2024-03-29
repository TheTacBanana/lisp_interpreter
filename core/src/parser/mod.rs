use std::collections::VecDeque;

use crate::{
    lexer::token::{LexerToken, LexerTokenKind},
    literal::Literal,
    token::{
        span::Span,
        stream::{TokenStream, TokenStreamExt},
        Token,
    },
};

use self::{
    ast::AST,
    token::{ParseTokenError, ParserError, ParserTokenKind},
};

pub mod ast;
pub mod token;

pub struct Parser {
    pub tokens: TokenStream,
}

impl Parser {
    pub fn new(mut tokens_in: Vec<LexerToken>) -> Option<Self> {
        let tokens = tokens_in
            .drain(..)
            .fold(Some(TokenStream::default()), |l, r| {
                let mut l = l?;

                let Token { kind, span } = r;

                let token = match kind {
                    LexerTokenKind::Boolean(b) => {
                        let b = match b.chars().nth(1)? {
                            't' | 'T' => true,
                            'f' | 'F' => false,
                            _ => unreachable!(),
                        };
                        Some(Literal::from_bool(b).into())
                    }
                    LexerTokenKind::Numeric(nl) => Some(Literal::from_numeric(nl).into()),
                    LexerTokenKind::Character(ch) => {
                        Some(Literal::from_char(ch.chars().nth(2)?).into())
                    }
                    LexerTokenKind::Identifer(i) => Some(ParserTokenKind::Identifier(i)),
                    LexerTokenKind::String(s) => {
                        let mut s = s[1..].to_string();

                        if !s.is_empty() && s.ends_with('"') {
                            s.pop();
                        }
                        Some(ParserTokenKind::String(s))
                    }
                    LexerTokenKind::Symbol(s) => Some(ParserTokenKind::Symbol(s)),
                    _ => None,
                };

                if let Some(token) = token {
                    l.push_back(Token { kind: token, span });
                };
                Some(l)
            });

        Some(Parser { tokens: tokens? })
    }

    pub fn parse(mut self) -> ParseResult {
        if self.tokens.is_empty() {
            return ParseResult {
                ast: Vec::new(),
                errors: Vec::new(),
            };
        }

        let mut errors = Vec::new();
        let mut items = Vec::new();
        match self.tokens.peek_front().unwrap() {
            ParserTokenKind::Symbol(_)
            | ParserTokenKind::Literal(_)
            | ParserTokenKind::String(_) => {
                while !self.tokens.is_empty() {
                    match Self::parse_item(&mut self.tokens) {
                        Ok(item) => items.push(item),
                        Err(e) => errors.push(e),
                    };
                }
            }
            ParserTokenKind::Identifier(_) => { //TODO: Make this less fucked
                let total_span = self.tokens.total_span().unwrap();
                let mut things = VecDeque::new();
                while !self.tokens.is_empty() {
                    match Self::parse_item(&mut self.tokens) {
                        Ok(item) => things.push_back(item),
                        Err(e) => errors.push(e),
                    };
                }
                let len = things.len();
                let mut drain = things.drain(..);
                if len == 1 {
                    items.push(drain.next().unwrap())
                } else if len == 0 {
                    todo!()
                } else {
                    let op = drain.next().unwrap();
                    let body = drain.collect::<Vec<_>>();
                    items.push(AST::Operation(Box::new(op), body, total_span))
                }

            }
        };
        ParseResult { ast: items, errors }
    }

    fn parse_item(stream: &mut TokenStream) -> Result<AST, ParserError> {
        use ParserTokenKind as TK;

        let Token { kind, span } = stream.pop_front().unwrap(); // SAFE
        match kind {
            TK::Literal(lit) => Ok(AST::Literal(lit, span)),
            TK::Identifier(ident) => Ok(AST::Identifier(ident, span)),
            TK::String(s) => Ok(AST::StringLiteral(s, span)),

            // New block
            TK::Symbol(s) if &s == "(" => {
                let index = match stream.opposite(TK::Symbol(s)) {
                    Ok(index) => index,
                    Err(_) => Err(ParserError::new(ParseTokenError::MissingBracket, span))?,
                };
                let mut block = stream.take_n(index + 1).unwrap();
                let end_token = block.pop_back().unwrap();
                let total_span = span.max_span(end_token.span);

                Self::parse_block(block, total_span)
            }

            // Quote
            TK::Symbol(s) if &s == "'" => Self::parse_quoted(stream, span),

            TK::Symbol(s) if &s == ".." => Ok(AST::Identifier("..".to_string(), span)),

            _ => Err(ParserError::new(ParseTokenError::NoItemFound, span)),
        }
    }

    fn parse_block(mut stream: TokenStream, span: Span) -> Result<AST, ParserError> {
        if stream.is_empty() {
            return Ok(AST::EmptyList(span));
        }

        let item = Self::parse_item(&mut stream)?;

        let mut items = Vec::new();
        while !stream.is_empty() {
            let item = Self::parse_item(&mut stream)?;
            items.push(item);
        }
        Ok(AST::Operation(Box::new(item), items, span))
    }

    fn parse_quoted(stream: &mut TokenStream, quote_span: Span) -> Result<AST, ParserError> {
        let Token { kind, span } = stream.pop_front().ok_or(ParserError::new(
            ParseTokenError::QuoteWithoutItem,
            quote_span,
        ))?;
        match kind {
            ParserTokenKind::Symbol(s) if &s == "(" => {
                let index = match stream.opposite(ParserTokenKind::Symbol(s)) {
                    Ok(index) => index,
                    Err(_) => Err(ParserError::new(ParseTokenError::MissingBracket, span))?,
                };
                let mut block = stream.take_n(index + 1).unwrap();
                let last = block.pop_back().unwrap();

                Self::parse_list(block, span.max_span(last.span))
            }
            ParserTokenKind::Identifier(ident) => Ok(AST::Identifier(ident, span)),
            ParserTokenKind::Literal(lit) => Ok(AST::Literal(lit, span)),
            ParserTokenKind::String(s) => Ok(AST::StringLiteral(s, span)),
            _ => Err(ParserError::new(ParseTokenError::ItemCannotBeQuoted, span)),
        }
    }

    fn parse_list(mut stream: TokenStream, span: Span) -> Result<AST, ParserError> {
        let mut items = Vec::new();
        while !stream.is_empty() {
            let item = Self::parse_item(&mut stream)?;
            items.push(item);
        }
        if items.is_empty() {
            Ok(AST::EmptyList(span))
        } else {
            Ok(AST::list_from_vec(items))
        }
    }
}

#[derive(Debug)]
pub struct ParseResult {
    pub ast: Vec<AST>,
    pub errors: Vec<ParserError>,
}
