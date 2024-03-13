use scheme_core::parser::ast::AST;

pub enum Symbol {
    Value(AST),
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

// #[derive(Clone)]
pub enum FunctionCall {
    /// Native function call
    Native(Box<dyn 'static + Fn(&Vec<AST>) -> Option<AST>> ),
    // Defined(Vec<AST>, AST)
}