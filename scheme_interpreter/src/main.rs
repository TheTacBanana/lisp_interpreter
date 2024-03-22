use std::{fs::File, io::Read};

use anyhow::{Ok, Result};
use scheme_core::{error::ErrorWriter, lexer::Lexer, parser::Parser};
use scheme_interpreter::InterpreterContext;

pub fn main() -> Result<()> {
    let files = std::env::args().skip(1).collect::<Vec<_>>();
    if files.len() == 0 {
        println!("Error: File not specified");
        return Ok(());
    }

    let mut interpreter = InterpreterContext::new();
    interpreter.with_std();

    let mut file = File::open(&files[0])?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let error_writer = ErrorWriter::from_string(&contents);

    let lexer_result = Lexer::from_string(contents.clone()).lex();
    if error_writer.report_errors(lexer_result.errors).is_err() {
        return Ok(());
    }

    let parser_result = Parser::new(lexer_result.tokens).parse();
    if error_writer.report_errors(parser_result.errors).is_err() {
        return Ok(());
    }

    for ast in parser_result.ast {
        if let Err(err) = interpreter.interpret(&ast) {
            let _ = error_writer.report_errors(vec![err]);
            break;
        }
    }

    Ok(())
}
