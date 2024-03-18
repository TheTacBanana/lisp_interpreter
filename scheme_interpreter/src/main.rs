use std::{fs::File, io::Read};

use anyhow::{Ok, Result};
use scheme_core::{lexer::Lexer, parser::Parser};
use scheme_interpreter::InterpreterContext;

pub fn main() -> Result<()> {
    let files = std::env::args().skip(1).collect::<Vec<_>>();
    if files.len() == 0 {
        println!("Error: File not specified");
        return Ok(());
    }

    let mut interpreter = InterpreterContext::new();

    let mut file = File::open(&files[0])?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let lexer_result = Lexer::from_string(contents.clone()).lex();
    if let Some(writer) = lexer_result.error_writer() {
        writer.write();
        return Ok(());
    }

    let parser_result = Parser::new(lexer_result.tokens).parse();
    if let Some(writer) = parser_result.error_writer(&contents) {
        writer.write();
        return Ok(());
    }

    for ast in parser_result.ast {
        interpreter.interpret(&ast)?;
    }

    Ok(())
}
