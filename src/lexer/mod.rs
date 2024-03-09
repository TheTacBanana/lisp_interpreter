use std::collections::VecDeque;

use crate::{
    rules::Rules,
    token::{
        span::{LineCol, Span},
        stream::TokenStream,
        TokenKind,
    },
};

use self::{
    literal::NumericLiteral,
    token::{LexerErr, LexerToken, LexerTokenError, LexerTokenKind},
};

pub mod literal;
pub mod token;

pub struct Lexer {
    file: VecDeque<char>,
    tokens: Vec<LexerToken>,
    errors: Vec<(usize, LexerTokenError)>,
    file_pos: LineCol,
}

impl Lexer {
    pub fn from_string(contents: String) -> Self {
        Self {
            file: contents.chars().collect(),
            tokens: Vec::default(),
            errors: Vec::default(),
            file_pos: LineCol::zero(),
        }
    }

    pub fn lex(mut self) -> LexResult {
        loop {
            let token = match self.read_next_token() {
                Ok(token) => token,
                Err(token_err) => {
                    let (token, err) = token_err.unpack();
                    self.errors.push((self.tokens.len(), err));
                    token
                }
            };
            let eof = *token == LexerTokenKind::EOF;
            self.tokens.push(token);

            if eof {
                break;
            }
        }

        println!("{:?}", self.tokens);
        println!("{:?}", self.errors);

        LexResult {
            tokens: VecDeque::from(self.tokens),
            errors: self.errors,
        }
    }

    fn read_next_token(&mut self) -> Result<LexerToken, LexerErr> {
        let mut cur_token = if let Some(ch) = self.take_next_char() {
            self.file_pos.extend_with(ch.to_string());
            self.match_new(ch).unwrap() // TODO: Remove unwrap
        } else {
            return Ok(self.start_new_token(LexerTokenKind::EOF));
        };

        use LexerTokenKind as Token;

        while let Some(&ch) = self.peek_next_char() {
            let consume = match (cur_token.inner(), ch) {
                (Token::Whitespace(_), w) if Rules::whitespace(w) => true,
                (Token::Boolean(_), b) if Rules::boolean(b) => true,
                (Token::Identifer(_), i) if Rules::identifier(i) => true,

                (Token::Character(cs), '\\') if cs.len() == 1 => true,
                (Token::Character(_), _) => true,

                _ => false,
            };

            if consume {
                (*cur_token).push_to_inner(ch);
                self.take_next_char();
                self.file_pos.extend_with(ch.to_string());
                cur_token.span.extend_with(ch.to_string());
            } else {
                break;
            }
        }

        Ok(cur_token)
    }

    fn match_new(&self, ch: char) -> Result<LexerToken, ()> {
        match (ch, self.peek_next_char()) {
            (w, _) if Rules::whitespace(w) => {
                Ok(self.start_new_token(LexerTokenKind::Whitespace(w.to_string())))
            }
            (b, Some('t' | 'T' | 'f' | 'F')) if Rules::start_boolean(b) => {
                Ok(self.start_new_token(LexerTokenKind::Boolean(b.to_string())))
            }
            (c, Some('\\')) if Rules::start_character(c) => {
                Ok(self.start_new_token(LexerTokenKind::Character(c.to_string())))
            }
            (n, _) if Rules::start_numeric(n) => {
                Ok(self.start_new_token(LexerTokenKind::Numeric(NumericLiteral::new(n))))
            }
            (i, _) if Rules::start_identifier(i) => {
                Ok(self.start_new_token(LexerTokenKind::Identifer(i.to_string())))
            }
            (s, _) if Rules::start_string(s) => {
                Ok(self.start_new_token(LexerTokenKind::String(s.to_string())))
            }
            (s, _) if Rules::start_symbol(s) => {
                Ok(self.start_new_token(LexerTokenKind::Symbol(s.to_string())))
            }
            _ => Err(()),
        }
    }

    fn peek_next_char(&self) -> Option<&char> {
        self.file.front()
    }

    fn take_next_char(&mut self) -> Option<char> {
        self.file.pop_front()
    }

    fn start_new_token(&self, kind: LexerTokenKind) -> LexerToken {
        LexerToken {
            kind,
            span: Span::single(self.file_pos),
        }
    }
}

pub struct LexResult {
    pub tokens: TokenStream<LexerToken>,
    pub errors: Vec<(usize, LexerTokenError)>,
}

mod test {
    use crate::{
        lexer::token::{LexerTokenError, LexerTokenKind},
        token::TokenKind,
    };

    use super::Lexer;

    macro_rules! lex_test {
        ($test:ident, $s:expr, $tokens:expr) => {
            #[test]
            fn $test() {
                let mut result = Lexer::from_string($s.to_string()).lex();
                assert_eq!(result.tokens.len(), $tokens.len());
                assert_eq!(result.errors.len(), 0);
                let tokens_same = result.tokens.drain(..).zip($tokens).all(|(l, r)| (*l) == r);
                assert!(tokens_same);
            }
        };

        ($test:ident, $s:expr, $tokens:expr, $errors:expr) => {
            #[test]
            fn $test() {
                let mut result = Lexer::from_string($s.to_string()).lex();
                assert_eq!(result.tokens.len(), $tokens.len());
                assert_eq!(result.errors.len(), $errors.len());
                let tokens_same = result.tokens.drain(..).zip($tokens).all(|(l, r)| (*l) == r);
                assert!(tokens_same);
                let errors_same = result.errors.drain(..).zip($errors).all(
                    |(l, r): ((usize, LexerTokenError), (usize, LexerTokenError))| {
                        l.0 == r.0 && l.1 == r.1
                    },
                );
                assert!(errors_same);
            }
        };
    }

    lex_test!(empty_file, "", [LexerTokenKind::EOF]);

    lex_test!(
        whitespace,
        " \n ",
        [
            LexerTokenKind::Whitespace(" \n ".into()),
            LexerTokenKind::EOF
        ]
    );

    lex_test!(
        identifer,
        "alpha",
        [
            LexerTokenKind::Identifer("alpha".into()),
            LexerTokenKind::EOF
        ]
    );

    lex_test!(
        boolean,
        "#f #t",
        [
            LexerTokenKind::Boolean("#f".into()),
            LexerTokenKind::Whitespace(" ".into()),
            LexerTokenKind::Boolean("#t".into()),
            LexerTokenKind::EOF
        ]
    );

    lex_test!(
        character,
        "#\\*",
        [
            LexerTokenKind::Character("#\\*".into()),
            LexerTokenKind::EOF
        ]
    );
}
