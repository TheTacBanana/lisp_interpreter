use core::error::LispError;
use std::ops::RangeFrom;

pub type InterpreterError = LispError<InterpreterErrorKind>;

#[derive(Debug, Clone, PartialEq)]
pub enum InterpreterErrorKind {
    // Identifier Errors
    CantResolveIdentifier(String),
    IsNotParamName(String),
    CannotCall(String),
    ExpectedNOrMoreParams(RangeFrom<usize>, usize),
    ExpectedNParams(usize, usize),

    // Failed Operation
    ExpectedList,
    ExpectedString,
    NullDeref,
    CannotAllocateNull, // TODO:
    PointerDoesNotExist, // TODO:
    CannotCompare(String, String),
    CannotPerformOperation(String, String, String),
    CannotConvertType(String, String),

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

    // Let Errors
    InvalidLetStatement,
    InvalidLetBindingForm,
    InvalidLetBindingName,

    // File IO Errors
    CannotOpenFile(String),
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
            InterpreterErrorKind::CannotAllocateNull => {
                "Cannot allocate whatever the fuck this is to the heap" //TODO:
            }
            InterpreterErrorKind::ExpectedList => "Operation expected a List",
            InterpreterErrorKind::InvalidFuncParamNames => "Invalid Param names",
            InterpreterErrorKind::ExpectedNOrMoreParams(expected, received) => {
                temp = format!("Operation expected {expected:?} parameters received {received}");
                &temp
            }
            InterpreterErrorKind::ExpectedNParams(expected, received) => {
                temp = format!("Operation expected {expected:?} parameters received {received}");
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
            InterpreterErrorKind::InvalidLetStatement => "let statement must be in the form `let ((ident value) ..) (block)`",
            InterpreterErrorKind::InvalidLetBindingForm => "let binding must be in the form `(ident value)`",
            InterpreterErrorKind::InvalidLetBindingName => "Invalid identifier name in let binding",
            InterpreterErrorKind::CannotPerformOperation(op, l, r) => {
                temp = format!("Cannot perform '{op}' between '{l}' and '{r}'");
                &temp
            },
            InterpreterErrorKind::ExpectedString => "Operations expected a String",
            InterpreterErrorKind::CannotOpenFile(file_name) => {
                temp = format!("Cannot open file '{file_name}'");
                &temp
            },
            InterpreterErrorKind::CannotConvertType(from, to) => {
                temp = format!("Cannot convert from '{from}' to '{to}'");
                &temp
            },
        };
        write!(f, "{s}")
    }
}
