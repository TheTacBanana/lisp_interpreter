use std::{fs::File, io::Read, path::PathBuf};

use anyhow::{Ok, Result};
use scheme_core::{
    error::ErrorWriter,
    file::SchemeFile,
    lexer::{self, LexResult, Lexer},
    parser::{ast::AST, Parser},
};
use scheme_interpreter::InterpreterContext;

pub fn main() -> Result<()> {
    let mut file_names = std::env::args().skip(1).collect::<Vec<_>>();
    if file_names.len() == 0 {
        println!("Error: File not specified");
        return Ok(());
    }

    let mut open_files = Vec::new();
    for file_name in file_names.drain(..) {
        let mut file = File::open(&file_name)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        open_files.push((PathBuf::from(file_name), contents))
    }

    let error_writer = ErrorWriter::new(
        open_files
            .iter()
            .map(|(_, contents)| contents.as_str())
            .collect(),
    );
    let mut parse_errors = false;
    let mut parsed_files = Vec::new();
    for (id, (path, file_contents)) in open_files.iter().enumerate() {
        // Lex the file
        let lexer_result = Lexer::new(id, file_contents.as_str()).lex();
        let _ = error_writer.report_errors(lexer_result.errors);

        // Create the parser
        let Some(parser) = Parser::new(lexer_result.tokens) else {
            //TODO:
            continue;
        };

        // Parse the
        let parser_result = parser.parse();
        if error_writer.report_errors(parser_result.errors).is_err() {
            parse_errors = true;
        } else {
            parsed_files.push(SchemeFile {
                file_id: id,
                path: path.clone(),
                // name,
                ast: parser_result.ast,
            });
        }
    }

    if parse_errors {
        return Ok(());
    }

    let mut interpreter = InterpreterContext::from_files(parsed_files);
    interpreter.with_std();
    interpreter.start(&error_writer)?;
    Ok(())

    // for ast in parser_result.ast {
    //     if let Err(err) = interpreter.interpret(&ast) {
    //         let _ = error_writer.report_errors(vec![err]);
    //         break;
    //     }
    // }
}
