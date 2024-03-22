use std::io::Write;

use scheme_core::{
    error::ErrorWriter,
    lexer::{self, Lexer},
    parser::Parser,
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

    let mut interpreter = InterpreterContext::new();
    interpreter.with_std();
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        let mut str_in = String::new();
        std::io::stdin()
            .read_line(&mut str_in)
            .expect("Failed to take input");

        let error_writer = ErrorWriter::from_string(&str_in);
        let lexer_result = Lexer::from_string(str_in.clone()).lex();
        let _ = error_writer.report_errors(lexer_result.errors);
        if verbose {
            println!(
                "{:?}",
                lexer_result
                    .tokens
                    .iter()
                    .map(|t| t.inner())
                    .collect::<Vec<_>>()
            );
        }

        let Some(parser) = Parser::new(lexer_result.tokens) else {
            continue;
        };
        let parser_result = parser.parse();
        if error_writer.report_errors(parser_result.errors).is_err() {
            continue;
        } else if verbose {
            println!("{:?}", parser_result.ast);
        }

        for ast in parser_result.ast {
            if let Err(err) = interpreter.interpret(&ast) {
                interpreter.stack_trace();
                interpreter.heap_dump();
                let _ = error_writer.report_errors(vec![err]);
            }

            if let Ok(p) = interpreter.pop_data() {
                let obj = p.deref(&interpreter).unwrap();
                println!("{}", obj);
            }
        }
    }
}
