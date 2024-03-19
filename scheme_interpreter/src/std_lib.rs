use std::ops::Deref;

use scheme_core::parser::{ast::AST, token::Literal};

use crate::{
    alloc::{InterpreterHeapAlloc, InterpreterStackAlloc},
    deref::InterpreterDeref,
    func::Func,
    object::{HeapObject, ObjectRef, UnallocatedObject},
    InterpreterContext, InterpreterError, InterpreterResult,
};

pub fn if_macro(
    interpreter: &mut InterpreterContext,
    mut ast: Vec<&AST>,
) -> InterpreterResult<*const AST> {
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

pub fn lambda(interpreter: &mut InterpreterContext, mut ast: Vec<&AST>) -> InterpreterResult<()> {
    let mut ast = ast.drain(..);
    let param_names = match ast.next().unwrap() {
        AST::Identifier(ident) => {
            vec![ident.clone()]
        }
        AST::Operation(op_name, op_params) => {
            let AST::Identifier(op_name) = &**op_name else {
                panic!("Expected Identifier {op_name}")
            };

            let mut param_names = vec![op_name.clone()];
            for p in op_params.iter() {
                match p {
                    AST::Identifier(ident) => param_names.push(ident.clone()),
                    e => panic!("Expected Identifier received {e}"),
                }
            }

            param_names
        }
        e => return Err(InterpreterError::InvalidOperator((*e).clone())),
    };

    let obj = UnallocatedObject::Func(Func::Defined(
        None,
        param_names,
        ast.next().unwrap().clone(),
    )).stack_alloc(interpreter)?;

    interpreter.push_data(obj);

    assert!(ast.len() == 0);

    Ok(())
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
