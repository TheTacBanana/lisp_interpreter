use error::ErrorWriter;
use lexer::Lexer;
use parser::{ast::AST, Parser};

pub mod rules;
pub mod lexer;
pub mod parser;
pub mod token;
pub mod error;
pub mod literal;

pub struct LexerParser;

impl LexerParser {
    pub fn from_string(file_id: usize, contents: String, error_writer: &ErrorWriter) -> Result<Vec<AST>, ()> {
        let lexer_result = Lexer::new(file_id, contents.as_str()).lex();
        let lexer_error =  error_writer.report_errors(lexer_result.errors).is_err();

        let Some(parser) = Parser::new(lexer_result.tokens) else {
            return Err(())
        };

        let parser_result = parser.parse();
        if error_writer.report_errors(parser_result.errors).is_err() || lexer_error {
            Err(())
        } else {
            Ok(parser_result.ast)
        }
    }

}