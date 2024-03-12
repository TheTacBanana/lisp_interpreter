use std::io::Write;

use scheme_core::{lexer::{self, Lexer}, parser::Parser};

fn main() {
    println!("Scheme REPL (Read Evalutate Print Loop:");

    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        let mut str_in = String::new();
        std::io::stdin().read_line(&mut str_in).expect("Failed to take input");

        let lexer_result = Lexer::from_string(str_in).lex();

        if let Some(writer) = lexer_result.error_writer() {
            writer.write();
            continue;
        } else {
            let tokens = lexer_result.tokens.iter().map(|t| t.inner()).collect::<Vec<_>>();
            println!("{:?}", tokens);
        }

        let parser = Parser::new(lexer_result.tokens).parse();
    }
}