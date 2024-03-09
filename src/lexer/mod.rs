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

        let mut err = None;

        use LexerTokenKind as Token;
        #[derive(Clone, Copy, PartialEq, Eq)]
        enum State {
            Break,
            Consume,
            ConsumeAndBreak,
        }

        loop {
            let ch = self.peek_next_char().map(|c| *c);
            let state = match (cur_token.inner(), ch) {
                (Token::Whitespace(_), Some(w)) if Rules::whitespace(w) => State::Consume,
                (Token::Boolean(_), Some(b)) if Rules::boolean(b) => State::Consume,
                (Token::Identifer(_), Some(i)) if Rules::identifier(i) => State::Consume,

                (Token::Character(cs), Some('\\')) if cs.len() == 1 => State::Consume,
                (Token::Character(cs), Some(c)) if cs.len() == 2 && Rules::character(c) => State::Consume,

                (Token::String(_), None) => {
                    err = Some(LexerTokenError::EOFInStringLiteral);
                    State::Break
                }
                (Token::String(s), Some(c)) if s.chars().last().unwrap() == '\\' => {
                    dbg!(c);
                    if !Rules::escaped_char(c) {
                        err = Some(LexerTokenError::EscapeCharacterExpected);
                    }
                    State::Consume
                }
                (Token::String(_), Some('"')) => State::ConsumeAndBreak,
                (Token::String(_), _) => State::Consume,

                _ => State::Break,
            };

            if let (Some(ch), State::Consume | State::ConsumeAndBreak) = (ch, state) {
                (*cur_token).push_to_inner(ch);
                self.take_next_char();
                self.file_pos.extend_with(ch.to_string());
                cur_token.span.extend_with(ch.to_string());
            }

            if let State::Break | State::ConsumeAndBreak = state {
                break;
            }
        }

        if let Some(err) = err {
            Err(cur_token.with_error(err))
        } else {
            Ok(cur_token)
        }
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
            (s, _) if Rules::start_string(s) => {
                Ok(self.start_new_token(LexerTokenKind::String(s.to_string())))
            }
            (i, _) if Rules::start_identifier(i) => {
                Ok(self.start_new_token(LexerTokenKind::Identifer(i.to_string())))
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
                result
                    .tokens
                    .drain(..)
                    .zip($tokens)
                    .for_each(|(l, r)| assert_eq!((*l), r));
            }
        };

        ($test:ident, $s:expr, $tokens:expr, $errors:expr) => {
            #[test]
            fn $test() {
                let mut result = Lexer::from_string($s.to_string()).lex();
                assert_eq!(result.tokens.len(), $tokens.len());
                assert_eq!(result.errors.len(), $errors.len());
                result
                    .tokens
                    .drain(..)
                    .zip($tokens)
                    .for_each(|(l, r)| assert_eq!((*l), r));
                result.errors.drain(..).zip($errors).for_each(
                    |(l, r): ((usize, LexerTokenError), (usize, LexerTokenError))| {
                        assert_eq!(l.0, r.0);
                        assert_eq!(l.1, r.1);
                    },
                );
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

    lex_test!(
        string,
        "\"hello world\"",
        [
            LexerTokenKind::String("\"hello world\"".into()),
            LexerTokenKind::EOF
        ]
    );

    lex_test!(
        escape_character,
        "\"\\n\"",
        [
            LexerTokenKind::String("\"\\n\"".into()),
            LexerTokenKind::EOF
        ]
    );

    lex_test!(
        escape_character_error,
        "\"\\ \"",
        [LexerTokenKind::String("\"\\ \"".into()), LexerTokenKind::EOF],
        [(0, LexerTokenError::EscapeCharacterExpected)]
    );

    lex_test!(
        eof_in_string_error,
        "\"uwu :3",
        [LexerTokenKind::String("\"uwu :3".into()), LexerTokenKind::EOF],
        [(0, LexerTokenError::EOFInStringLiteral)]
    );
}
