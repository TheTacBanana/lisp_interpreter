use crate::parser::ast::AST;


pub struct SchemeFile {
    file_id: usize,
    ast: Vec<AST>
}

impl SchemeFile {
    pub fn new(file_id: usize, ast: Vec<AST>) -> Self {
        Self {
            file_id,
            ast,
        }
    }
}