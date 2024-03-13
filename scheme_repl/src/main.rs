use std::io::Write;

use scheme_core::{lexer::{self, Lexer}, parser::Parser};

fn main() {
    let verbose = std::env::args().any(|s| s.to_uppercase() == "v");
    println!("Scheme REPL (Read Evalutate Print Loop:");

    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        let mut str_in = String::new();
        std::io::stdin().read_line(&mut str_in).expect("Failed to take input");

        let lexer_result = Lexer::from_string(str_in.clone()).lex();
        if let Some(writer) = lexer_result.error_writer() {
            writer.write();
            continue;
        } else if verbose{
            println!("{:?}", lexer_result.tokens.iter().map(|t| t.inner()).collect::<Vec<_>>());
        }

        let parser_result = Parser::new(lexer_result.tokens).parse();
        if let Some(writer) = parser_result.error_writer(&str_in) {
            writer.write();
            continue;
        } else if verbose{
            println!("{:?}", parser_result.ast);
        }
    }
}