use anyhow::Result;
use completion::ReplCompleter;
use core::{error::ErrorWriter, LexerParser};
use interpreter::{deref::InterpreterDeref, print::InterpreterPrint, InterpreterContext};
use std::sync::Arc;

use rustyline::{config::Configurer, error::ReadlineError, CompletionType, Editor};

pub mod completion;

fn main() -> Result<()> {
    let context = Arc::new(InterpreterContext::new(ErrorWriter::empty()));

    let mut editor = Editor::new()?;
    editor.set_helper(Some(ReplCompleter(context.clone())));
    editor.set_completion_type(CompletionType::List);

    loop {
        let readline = editor.readline(">> ");
        match readline {
            Ok(line) => {
                editor.add_history_entry(line.as_str())?;

                let file_id = context
                    .error_writer
                    .write()
                    .unwrap()
                    .load_string(line.clone());
                let Ok(ast) =
                    LexerParser::from_string(file_id, line, &context.error_writer.read().unwrap())
                else {
                    continue;
                };

                context.start(ast);
                if let Ok(p) = context.stack.pop_data() {
                    let obj = p.deref(&context)?;
                    println!("{}", obj.interpreter_fmt(&context));
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    Ok(())
}
