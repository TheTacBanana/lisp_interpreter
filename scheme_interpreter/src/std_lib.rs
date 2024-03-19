use scheme_core::parser::{ast::AST, token::Literal};

use crate::{
    alloc::InterpreterStackAlloc,
    deref::InterpreterDeref,
    object::{ObjectRef, UnallocatedObject},
    InterpreterContext, InterpreterError, InterpreterResult,
};

pub fn if_macro(interpreter: &mut InterpreterContext, mut ast: Vec<&AST>) -> InterpreterResult<*const AST> {
    assert!(ast.len() == 3);

    let mut drain = ast.drain(..);
    let cond = drain.next().unwrap();

    interpreter.interpret(&cond)?;
    let cond = interpreter.pop_data()?;
    let result = match cond.deref(interpreter)? {
        ObjectRef::Value(Literal::Boolean(false)) => false,
        _ => true,
    };

    if result {
        Ok(drain.next().unwrap())
    } else {
        Ok(drain.skip(1).next().unwrap())
    }
}

pub fn add(interpreter: &mut InterpreterContext, n: usize) -> InterpreterResult<()> {
    let mut objs = Vec::new();
    for _ in 0..n {
        objs.push(interpreter.pop_data()?);
    }

    let drain = objs.drain(..);
    let out = drain.fold(None, |out, obj| {
        match (out, obj.deref(interpreter).unwrap()) {
            (None, v) => Some(v.clone_to_unallocated()),
            (
                Some(UnallocatedObject::Value(Literal::Numeric(l))),
                ObjectRef::Value(Literal::Numeric(r)),
            ) => Some(UnallocatedObject::Value(Literal::Numeric(l + r))),
            _ => panic!(),
        }
    });

    let stack_obj = out
        .ok_or(InterpreterError::FailedOperation)?
        .stack_alloc(interpreter)?;

    interpreter.push_data(stack_obj);

    return Ok(());
}

// pub fn lambda(interpreter: &mut InterpreterContext, n: usize) -> InterpreterResult<()> {

// }
