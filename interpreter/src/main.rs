use std::{fs::File, io::Read};

use anyhow::{Ok, Result};
use core::{
    error::ErrorWriter,
    LexerParser,
};
use interpreter::InterpreterContext;

pub fn main() -> Result<()> {
    let file_names = std::env::args().skip(1).collect::<Vec<_>>();
    if file_names.is_empty() {
        println!("Error: File not specified");
        return Ok(());
    }

    let mut file = File::open(&file_names[0])?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let error_writer = ErrorWriter::new((&file_names[0]).into(), contents.clone());
    let ast = LexerParser::from_string(0, contents, &error_writer).unwrap();

    let context = InterpreterContext::new(error_writer);
    context.start(ast);

    Ok(())
}
