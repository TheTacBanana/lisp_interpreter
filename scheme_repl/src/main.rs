use scheme_core::lexer::Lexer;

fn main() {
    println!("Scheme REPL (Read Evalutate Print Loop:");

    loop {
        let mut str_in = String::new();
        std::io::stdin().read_line(&mut str_in).expect("Failed to take input");

        let lexer_result = Lexer::from_string(str_in).lex();

        if let Some(writer) = lexer_result.error_writer() {
            writer.write();
            continue;
        }
    }
}