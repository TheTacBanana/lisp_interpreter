use scheme_core::parser::{ast::AST, token::Literal};

#[derive(Clone)]
pub enum Symbol {
    Value(Literal),
    // Tokens(AST),
    FunctionCall(FunctionCall),
    Bottom,
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::Value(ast) => write!(f, "{ast}")?,
            Symbol::FunctionCall(FunctionCall::Native(_)) => write!(f, "NativeFn")?,
            // Symbol::FunctionCall(FunctionCall::Defined(_)) => write!(f, "UserFn")?,
            Symbol::Bottom => write!(f, "⊥")?,
            _ => ()
        }
        Ok(())
    }
}

#[derive(Clone)]
pub enum FunctionCall {
    /// Native function call
    Native(fn(Vec<Symbol>) -> Symbol),
    // Defined(Vec<AST>, AST)
}