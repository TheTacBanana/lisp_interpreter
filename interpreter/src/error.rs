use core::{error::FormattedError, token::span::Span};
use std::error::Error;

#[derive(Debug)]
pub struct InterpreterError {
    span: Option<Span>,
    kind: InterpreterErrorKind,
}

impl std::fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {}", self.span, self.kind)
    }
}

impl Error for InterpreterError {}

impl InterpreterError {
    pub fn new(kind: InterpreterErrorKind) -> Self {
        Self { span: None, kind }
    }

    pub fn optional_span(kind: InterpreterErrorKind, span: Option<Span>) -> Self {
        Self { span, kind }
    }

    pub fn spanned(kind: InterpreterErrorKind, span: Span) -> Self {
        Self {
            span: Some(span),
            kind,
        }
    }

    pub fn add_if_not_spanned(mut self, span: Span) -> Self {
        self.span.get_or_insert(span);
        self
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InterpreterErrorKind {
    // Identifier Errors
    CantResolveIdentifier(String),
    IsNotParamName(String),
    CannotCall(String),
    ExpectedNParams { expected: usize, received: usize },

    // Failed Operation
    ExpectedList,
    NullDeref,
    CannotAllocateNull, // TODO:
    PointerDoesNotExist, // TODO:
    FailedOperation, // TODO:
    CannotCompare(String, String),

    // Stack Related
    EmptyStack,
    EmptyDataStack,
    StackIndexOutOfRange,

    // Import Errors
    EmptyImport,
    InvalidInImport,
    ImportNotFound(String),
    ErrorInParsingImport,

    // Definition Syntax
    InvalidFuncParamNames,

}

impl FormattedError for InterpreterError {
    fn message(&self) -> String {
        self.kind.to_string()
    }

    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl std::fmt::Display for InterpreterErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let temp;
        let s = match self {
            InterpreterErrorKind::NullDeref => "Null Pointer Dereferenced",
            InterpreterErrorKind::CannotCall(s) => {
                temp = format!("Cannot call '{s}', it is not a function");
                &temp
            }
            InterpreterErrorKind::IsNotParamName(s) => {
                temp = format!("'{s}' is not an ident");
                &temp
            },
            InterpreterErrorKind::CantResolveIdentifier(s) => {
                temp = format!("{s} is not a known identifier");
                &temp
            }
            InterpreterErrorKind::EmptyStack => "Stack is empty, cannot pop Stack frame",
            InterpreterErrorKind::EmptyDataStack => "Data Stack is empty, cannot pop Data Stack",
            InterpreterErrorKind::StackIndexOutOfRange => "Pointer exceeds limit of stack",
            InterpreterErrorKind::PointerDoesNotExist => "Pointer does not.", //TODO:
            InterpreterErrorKind::FailedOperation => "Operation arguments not valid",
            InterpreterErrorKind::CannotAllocateNull => {
                "Cannot allocate whatever the fuck this is to the heap" //TODO:
            }
            InterpreterErrorKind::ExpectedList => "Operation expected a List",
            InterpreterErrorKind::InvalidFuncParamNames => "Invalid Param names",
            InterpreterErrorKind::ExpectedNParams { expected, received } => {
                temp = format!("Operation expected {expected} parameters received {received}");
                &temp
            }
            InterpreterErrorKind::EmptyImport => "Import is empty",
            InterpreterErrorKind::InvalidInImport => "Invalid in import",
            InterpreterErrorKind::ImportNotFound(s) => {
                temp = format!("Import '{s}' cannot be found");
                &temp
            }
            InterpreterErrorKind::ErrorInParsingImport => "Parse error in import",
            InterpreterErrorKind::CannotCompare(l, r) => {
                temp = format!("Cannot compare '{}' and '{}'", l, r);
                &temp
            },
        };
        write!(f, "{s}")
    }
}

impl Error for InterpreterErrorKind {}
