use std::io::Write;

use scheme_core::{
    error::ErrorWriter, lexer::{self, Lexer}, parser::Parser
};
use scheme_interpreter::{deref::InterpreterDeref, object::{ObjectRef, StackObject}, InterpreterContext};

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

        let mut error_writer = ErrorWriter::from_string(&str_in);
        let lexer_result = Lexer::from_string(str_in.clone()).lex();
        // if let Some(writer) = lexer_result.error_writer() {
            // writer.write();
            // continue;
        // } else if verbose {
        //     println!(
        //         "{:?}",
        //         lexer_result
        //             .tokens
        //             .iter()
        //             .map(|t| t.inner())
        //             .collect::<Vec<_>>()
        //     );
        // }

        let parser_result = Parser::new(lexer_result.tokens).parse();
        // if let Some(writer) = parser_result.error_writer(&str_in) {
        //     writer.write();
        //     continue;
        // } else if verbose {
        //     println!("{:?}", parser_result.ast);
        // }

        for ast in parser_result.ast {
            if let Err(err) = interpreter.interpret(&ast) {
                interpreter.stack_trace();
                interpreter.heap_dump();
                error_writer.report_errors(vec![err]);
                // println!("{err}");
            }

            if let Ok(p) = interpreter.pop_data() {
                let obj = p.deref(&interpreter).unwrap();
                println!("{}", obj);
            }
        }
    }
}
