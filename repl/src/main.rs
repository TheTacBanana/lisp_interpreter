use std::io::Write;

use core::{
    error::ErrorWriter, LexerParser
};
use interpreter::{
    deref::InterpreterDeref,
    InterpreterContext,
};

fn main() {
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
