use std::io::Write;

use scheme_core::{
    lexer::{self, Lexer},
    parser::Parser,
};
use scheme_interpreter::Interpreter;

fn main() {
    let verbose = std::env::args().any(|s| s.to_uppercase() == "V");
    if verbose {
        println!("Scheme REPL (Verbose):");
    } else {
        println!("Scheme REPL:");
    }

    let mut interpreter = Interpreter::new();
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        let mut str_in = String::new();
        std::io::stdin()
            .read_line(&mut str_in)
            .expect("Failed to take input");

        let lexer_result = Lexer::from_string(str_in.clone()).lex();
        if let Some(writer) = lexer_result.error_writer() {
            writer.write();
            continue;
        } else if verbose {
            println!(
                "{:?}",
                lexer_result
                    .tokens
                    .iter()
                    .map(|t| t.inner())
                    .collect::<Vec<_>>()
            );
        }

        let parser_result = Parser::new(lexer_result.tokens).parse();
        if let Some(writer) = parser_result.error_writer(&str_in) {
            writer.write();
            continue;
        } else if verbose {
            println!("{:?}", parser_result.ast);
        }

        for ast in parser_result.ast {
            interpreter.run_ast(ast);
        }
    }
}
