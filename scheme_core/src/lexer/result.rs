use crate::{error::{ErrorWriter}, token::stream::TokenStream};

use super::token::{LexerToken, LexerTokenError, LexerTokenKind};

#[derive(Debug)]
pub struct LexResult {
    pub file: String,
    pub tokens: TokenStream<LexerToken>,
    pub errors: Vec<(usize, LexerTokenError)>,
}

// impl LexResult {
//     pub fn error_writer(&self) -> Option<ErrorWriter<LexerTokenError>> {
//         if self.errors.len() == 0 {
//             return None
//         }

//         let lines = self.file.lines().collect::<Vec<_>>();

//         let mut formatted_errors = Vec::new();
//         for e in self.errors.iter() {
//             let primary_token = self.tokens.get(e.0).unwrap();
//             let span = primary_token.span;
//             let whole_line = lines.get(span.start.line).unwrap().to_string();

//             formatted_errors.push(
//                 IndividualError {
//                     whole_line,
//                     span,
//                     error: e.1,
//                 }
//             )
//         }

//         Some(
//             ErrorWriter {
//                 errors: formatted_errors,
//             }
//         )
//     }
// }