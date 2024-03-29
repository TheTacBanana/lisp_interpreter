use std::{collections::BTreeMap, path::PathBuf};

use crate::token::span::Span;

pub struct ErrorWriter {
    pub loaded_paths: BTreeMap<PathBuf, usize>,
    pub id_to_path: BTreeMap<usize, PathBuf>,
    pub files: BTreeMap<usize, Vec<String>>,
}

impl ErrorWriter {
    pub fn empty() -> Self {
        Self {
            loaded_paths: BTreeMap::new(),
            id_to_path: BTreeMap::new(),
            files: BTreeMap::new(),
        }
    }

    pub fn new(path: PathBuf, main: String) -> Self {
        let mut s = Self {
            files: BTreeMap::default(),
            loaded_paths: BTreeMap::default(),
            id_to_path: BTreeMap::default(),
        };
        s.add_file(path, main);
        s
    }

    pub fn already_loaded(&self, path: &PathBuf) -> bool {
        self.loaded_paths.contains_key(path)
    }

    pub fn add_file(&mut self, path: PathBuf, contents: String) -> usize {
        let id = self.files.len();
        self.loaded_paths.insert(path.clone(), id);
        self.id_to_path.insert(id, path);
        self.files.insert(
            id,
            contents.lines().map(|x| x.into()).collect::<Vec<String>>(),
        );
        id
    }

    pub fn load_string(&mut self, contents: String) -> usize {
        let id = self.files.len();
        self.files.insert(
            id,
            contents.lines().map(|x| x.into()).collect::<Vec<String>>(),
        );
        id
    }

    pub fn report_errors(&self, errors: Vec<impl FormattedError>) -> Result<(), ()> {
        if errors.is_empty() {
            return Ok(());
        }
        for e in errors {
            e.fmt_err(self).unwrap();
        }

        Err(())
    }

    pub fn get_line(&self, file: usize, l: usize) -> Option<&String> {
        self.files.get(&file).and_then(|f| f.get(l))
    }

    pub fn get_line_length(&self, file: usize, l: usize) -> Option<usize> {
        self.files
            .get(&file)
            .and_then(|f| f.get(l).map(|l| l.len()))
    }

    pub fn link_file(&self, span: Span) -> Option<String> {
        let path = self.id_to_path.get(&span.file_id)?;
        Some(format!(
            "{}:{}:{}",
            path.to_str().unwrap(),
            span.start.line + 1,
            span.start.col + 1
        ))
    }

    pub fn span_to_lines(&self, span: Span) -> Vec<Span> {
        let mut lines = span.lines().collect::<Vec<_>>();

        if lines.len() == 1 {
            vec![span]
        } else {
            let mut lines = lines.drain(..);
            let mut spans = Vec::new();

            let first = lines.next().unwrap();
            spans.push(Span::from_to_on(
                span.file_id,
                span.start.col,
                self.get_line_length(span.file_id, first).unwrap(),
                first,
            ));

            for _ in 0..(lines.len() - 1) {
                let line = lines.next().unwrap();
                spans.push(Span::from_to_on(
                    span.file_id,
                    0,
                    self.get_line_length(span.file_id, line).unwrap(),
                    line,
                ))
            }

            let last = lines.next().unwrap();
            spans.push(Span::from_to_on(span.file_id, 0, span.end.col, last));

            spans
        }
    }

    pub fn underline_span(span: Span) -> String {
        let padding = 0..span.start.col;
        let width = span.start.col..=span.end.col;
        let padding = padding.fold(String::new(), |l, _r| l + " ");

        width.fold(padding, |l, _r| l + "^")
    }
}

pub trait FormattedError {
    fn fmt_err(&self, ew: &ErrorWriter) -> std::fmt::Result {
        println!();
        println!("error: {}", self.message());

        let Some(span) = self.span() else {
            return Ok(());
        };
        if let Some(file_location) = ew.link_file(span) {
            println!(" --> {}", file_location)
        }

        let mut line_spans = ew.span_to_lines(span);
        let numbered_line_spans = line_spans
            .drain(..)
            .map(|l| (l.start.line.to_string(), l))
            .collect::<Vec<_>>();
        let max_num_length = numbered_line_spans
            .iter()
            .max_by_key(|l| l.0.len())
            .unwrap()
            .0
            .len();

        for (num, span) in numbered_line_spans.iter() {
            println!(
                " {} | {}",
                format!("{: >1$}", num, max_num_length),
                ew.get_line(span.file_id, span.start.line).unwrap()
            );

            println!(" {} | {}", format!("{: >1$}", "", max_num_length), ErrorWriter::underline_span(*span));
        }
        Ok(())
    }

    fn message(&self) -> String;

    fn span(&self) -> Option<Span>;
}
