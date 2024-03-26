use std::io::Write;

use scheme_core::{
    error::ErrorWriter, file, lexer::{self, Lexer}, parser::Parser, LexerParser
};
use scheme_interpreter::{
    deref::InterpreterDeref,
    object::{ObjectRef, StackObject},
    InterpreterContext,
};

fn main() {
    let verbose = std::env::args().any(|s| s.to_uppercase() == "V");
    if verbose {
        println!("Scheme REPL (Verbose):");
    } else {
        println!("Scheme REPL:");
    }

    let mut context = InterpreterContext::new(ErrorWriter::empty());
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        let mut str_in = String::new();
        std::io::stdin()
            .read_line(&mut str_in)
            .expect("Failed to take input");

        let file_id = context.error_writer.load_string(str_in.clone());
        let Ok(ast) = LexerParser::from_string(file_id, str_in, &context.error_writer) else { continue; };

        context.start(ast);
        if let Ok(p) = context.pop_data() {
            let obj = p.deref(&context).unwrap();
            println!("{}", obj);
        }
    }
}
