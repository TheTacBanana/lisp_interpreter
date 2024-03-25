use std::path::PathBuf;

use crate::parser::ast::AST;

#[derive(Debug, Clone)]
pub struct SchemeFile {
    pub file_id: usize,
    pub path: PathBuf,
    pub ast: Vec<AST>
}
