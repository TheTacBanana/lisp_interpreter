use std::collections::VecDeque;

use crate::{
    rules::Rules,
    token::span::{LineCol, Span},
};

use self::{
    literal::NumericLiteral,
    token::{LexerErr, LexerError, LexerToken, LexerTokenErrorKind, LexerTokenKind},
};

pub mod literal;
pub mod token;

pub struct Lexer {
    file_id: usize,
    whole_file: String,
    file: VecDeque<char>,
    tokens: Vec<LexerToken>,
    errors: Vec<LexerError>,
    file_pos: LineCol,
}

impl Lexer {
    pub fn new(file_id: usize, contents: &str) -> Self {
        Self {
            file_id,
            whole_file: contents.to_string(),
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
                    self.errors.push(LexerError::new(err, token.span));
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
            file: self.whole_file,
            tokens: self.tokens,
            errors: self.errors,
        }
    }

    fn read_next_token(&mut self) -> Result<LexerToken, LexerErr> {
        let mut cur_token = if let Some(ch) = self.take_next_char() {
            let temp = self.match_new(ch).unwrap(); // TODO: This could fail
            self.file_pos.extend_with(ch.to_string());
            temp
        } else {
            return Ok(self.start_new_token(LexerTokenKind::EOF));
        };

        let mut err = None;

        use LexerTokenKind as Token;
        use NumericLiteral as NL;
        #[derive(Clone, Copy, PartialEq, Eq)]
        enum State {
            Break,
            Consume,
            ConsumeAndBreak,
        }

        loop {
            let ch = self.peek_next_char().copied();
            let state =
                match (cur_token.inner(), ch) {
                    (Token::String(_), None) => {
                        err = Some(LexerTokenErrorKind::EOFInStringLiteral);
                        State::Break
                    }
                    (Token::String(s), Some(c)) if s.ends_with('\\') => {
                        if !Rules::escaped_char(c) {
                            err = Some(LexerTokenErrorKind::EscapeCharacterExpected);
                        }
                        State::Consume
                    }
                    (Token::String(_), Some('"')) => State::ConsumeAndBreak,
                    (Token::String(_), Some(c))
                        if Rules::line_break(c) && self.file.iter().nth(1).is_none() =>
                    {
                        err = Some(LexerTokenErrorKind::EOFInStringLiteral);
                        State::Break
                    }
                    (Token::String(_), Some(_)) => State::Consume,

                    (Token::Symbol(_), Some(s)) if Rules::symbol(s) => State::Consume,

                    (Token::Whitespace(_), Some(w)) if Rules::whitespace(w) => State::Consume,

                    (Token::Comment(_), Some(n)) if Rules::line_break(n) => State::ConsumeAndBreak,
                    (Token::Comment(_), Some(_)) => State::Consume,

                    (_, Some(c)) if Rules::delimiter(c) => {
                        State::Break
                    }

                    (Token::Boolean(_), Some(b)) if Rules::boolean(b) => State::Consume,

                    (Token::Identifer(_), Some(i)) if Rules::identifier(i) => State::Consume,

                    (Token::Character(cs), Some('\\')) if cs.len() == 1 => State::Consume,
                    (Token::Character(cs), Some(c)) if cs.len() == 2 && Rules::character(c) => {
                        State::Consume
                    }

                    (
                        Token::Numeric(NL::Bin(s) | NL::Oct(s) | NL::Dec(s) | NL::Hex(s)),
                        Some('b' | 'o' | 'd' | 'x' | 'B' | 'O' | 'D' | 'X'),
                    ) if s.len() == 1 && s == "#" => {
                        match ch.unwrap() {
                            'b' | 'B' => cur_token
                                .map_inner(|t| LexerTokenKind::Numeric(NL::Bin(t.to_string()))),
                            'o' | 'O' => cur_token
                                .map_inner(|t| LexerTokenKind::Numeric(NL::Oct(t.to_string()))),
                            'd' | 'D' => cur_token
                                .map_inner(|t| LexerTokenKind::Numeric(NL::Dec(t.to_string()))),
                            'x' | 'X' => cur_token
                                .map_inner(|t| LexerTokenKind::Numeric(NL::Hex(t.to_string()))),
                            _ => (),
                        }
                        State::Consume
                    }

                    (Token::Numeric(NL::Float(_)), Some('0'..='9')) => State::Consume,
                    (Token::Numeric(NL::Float(s) | NL::Dec(s)), Some('.')) => {
                        if s.contains('.') {
                            err = Some(LexerTokenErrorKind::MultiplePointsInFloat)
                        }
                        cur_token.map_inner(|t| LexerTokenKind::Numeric(NL::Float(t.to_string())));

                        State::Consume
                    }
                    (Token::Numeric(NL::Float(_)), Some(_)) => {
                        err = Some(LexerTokenErrorKind::InvalidInNumericLiteral);
                        State::Consume
                    },

                    (Token::Numeric(NL::Bin(_)), Some('0' | '1')) => State::Consume,
                    (Token::Numeric(NL::Bin(_)), Some(_)) => {
                        err = Some(LexerTokenErrorKind::InvalidInNAryLiteral(2));
                        State::Consume
                    },

                    (Token::Numeric(NL::Oct(_)), Some('0'..='7')) => State::Consume,
                    (Token::Numeric(NL::Oct(_)), Some(_)) => {
                        err = Some(LexerTokenErrorKind::InvalidInNAryLiteral(8));
                        State::Consume
                    },

                    (Token::Numeric(NL::Dec(_)), Some('0'..='9')) => State::Consume,
                    (Token::Numeric(NL::Dec(_)), Some('#')) => {
                        err = Some(LexerTokenErrorKind::InvalidInNumericLiteral);
                        State::Consume
                    },
                    (Token::Numeric(NL::Dec(_)), Some(_)) => {
                        err = Some(LexerTokenErrorKind::InvalidInNAryLiteral(10));
                        State::Consume
                    },

                    (Token::Numeric(NL::Hex(_)), Some('0'..='9' | 'a'..='f' | 'A'..='F')) => {
                        State::Consume
                    }
                    (Token::Numeric(NL::Hex(_)), Some(_)) => {
                        err = Some(LexerTokenErrorKind::InvalidInNAryLiteral(16));
                        State::Consume
                    },

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
            (n, a) if Rules::start_numeric(n, a.copied()) => Ok(self.start_new_token(
                LexerTokenKind::Numeric(NumericLiteral::new(n, a.copied())),
            )),
            (s, _) if Rules::start_string(s) => {
                Ok(self.start_new_token(LexerTokenKind::String(s.to_string())))
            }
            (s, _) if Rules::start_symbol(s) => {
                Ok(self.start_new_token(LexerTokenKind::Symbol(s.to_string())))
            }
            (s, _) if Rules::start_comment(s) => {
                Ok(self.start_new_token(LexerTokenKind::Comment(s.to_string())))
            }
            (i, _) if Rules::start_identifier(i) => {
                Ok(self.start_new_token(LexerTokenKind::Identifer(i.to_string())))
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
            span: Span::single(self.file_id, self.file_pos),
        }
    }
}

#[derive(Debug)]
pub struct LexResult {
    pub file: String,
    pub tokens: Vec<LexerToken>,
    pub errors: Vec<LexerError>,
}

#[cfg(test)]
mod test {
    use crate::lexer::{
        literal::NumericLiteral,
        token::{LexerTokenErrorKind, LexerTokenKind},
    };

    use super::Lexer;

    macro_rules! lex_test {
        ($test:ident, $s:expr, $tokens:expr) => {
            #[test]
            fn $test() {
                let mut result = Lexer::new(0, $s).lex();
                println!("{:?}", result.tokens);
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
                let mut result = Lexer::new(0, $s).lex();
                assert_eq!(result.tokens.len(), $tokens.len());
                assert_eq!(result.errors.len(), $errors.len());
                result
                    .tokens
                    .drain(..)
                    .zip($tokens)
                    .for_each(|(l, r)| assert_eq!((*l), r));
                result
                    .errors
                    .drain(..)
                    .map(|x| x.kind)
                    .zip($errors)
                    .for_each(|(l, r): (LexerTokenErrorKind, LexerTokenErrorKind)| {
                        assert_eq!(l, r);
                    });
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
        "#f #T",
        [
            LexerTokenKind::Boolean("#f".into()),
            LexerTokenKind::Whitespace(" ".into()),
            LexerTokenKind::Boolean("#T".into()),
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
        [
            LexerTokenKind::String("\"\\ \"".into()),
            LexerTokenKind::EOF
        ],
        [LexerTokenErrorKind::EscapeCharacterExpected]
    );

    lex_test!(
        eof_in_string_error,
        "\"uwu :3",
        [
            LexerTokenKind::String("\"uwu :3".into()),
            LexerTokenKind::EOF
        ],
        [LexerTokenErrorKind::EOFInStringLiteral]
    );

    lex_test!(
        bin_number,
        "#b010101",
        [
            LexerTokenKind::Numeric(NumericLiteral::Bin("#b010101".into())),
            LexerTokenKind::EOF
        ]
    );

    lex_test!(
        oct_number,
        "#o01234567",
        [
            LexerTokenKind::Numeric(NumericLiteral::Oct("#o01234567".into())),
            LexerTokenKind::EOF
        ]
    );

    lex_test!(
        dec_number,
        "12394 #d12394",
        [
            LexerTokenKind::Numeric(NumericLiteral::Dec("12394".into())),
            LexerTokenKind::Whitespace(" ".into()),
            LexerTokenKind::Numeric(NumericLiteral::Dec("#d12394".into())),
            LexerTokenKind::EOF
        ]
    );

    lex_test!(
        hex_number,
        "#x0123456789abcdefABCDEF",
        [
            LexerTokenKind::Numeric(NumericLiteral::Hex("#x0123456789abcdefABCDEF".into())),
            LexerTokenKind::EOF
        ]
    );

    lex_test!(
        float_number,
        "200.394",
        [
            LexerTokenKind::Numeric(NumericLiteral::Float("200.394".into())),
            LexerTokenKind::EOF
        ]
    );

    lex_test!(
        float_number_error,
        "200.394.334",
        [
            LexerTokenKind::Numeric(NumericLiteral::Float("200.394.334".into())),
            LexerTokenKind::EOF
        ],
        [LexerTokenErrorKind::MultiplePointsInFloat]
    );

    lex_test!(
        comment,
        "; this is a comment",
        [
            LexerTokenKind::Comment("; this is a comment".into()),
            LexerTokenKind::EOF
        ]
    );
}
