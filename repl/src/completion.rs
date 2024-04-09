use core::rules::Rules;
use std::sync::Arc;

use interpreter::InterpreterContext;
use rustyline::{
    completion::Completer,
    Helper, Highlighter, Hinter, Validator,
};

#[derive(Helper, Validator, Highlighter, Hinter)]
pub struct ReplCompleter(pub Arc<InterpreterContext>);

impl Completer for ReplCompleter {
    type Candidate = String;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        let idents = self.0.ident_mapping.read().unwrap();

        let chars = line[..pos].chars().collect::<Vec<_>>();
        if chars.is_empty() {
            return Ok((0, Vec::with_capacity(0)));
        }

        let found = chars.iter().enumerate().rev().find_map(|(i, c)| {
            if Rules::delimiter(*c) {
                Some(i + 1)
            } else {
                None
            }
        });

        let found = found.unwrap_or(0);

        let mut starts_with = idents
            .keys()
            .filter(|key| {
                let s = &line[found..];
                !s.is_empty() && key.starts_with(s) && *key != s
            })
            .map(|s| s.clone())
            .collect::<Vec<_>>();

        starts_with.sort();

        Ok((found, starts_with))
    }

    fn update(
        &self,
        line: &mut rustyline::line_buffer::LineBuffer,
        start: usize,
        elected: &str,
        cl: &mut rustyline::Changeset,
    ) {
        let end = line.pos();
        line.replace(start..end, elected, cl);
    }
}
