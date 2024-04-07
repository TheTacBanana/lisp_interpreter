# Lisp Interpreter

Lisp Interpreter written in Rust with minimal dependencies

Has a REPL with history and completion, as well as a standalone interpreter

`cargo run -r -p repl` or `cargo run -r -p interpreter ./examples/fib.scm`

Implements garbage collection and tail call optimization

Not feature complete but in a functional state, mainly lacking a more complete standard library