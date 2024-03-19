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

pub fn define(interpreter: &mut InterpreterContext, mut ast: Vec<&AST>) -> InterpreterResult<()> {
    let mut ast = ast.drain(..);
    match ast.next().unwrap() {
        // Define a value
        AST::Identifier(ident) => {
            interpreter.interpret(&ast.next().unwrap())?;
            let p = interpreter.pop_data()?;
            p.heap_alloc_named(ident, interpreter)?;
        }
        // Define a function
        AST::Operation(op_name, op_params) => {
            let AST::Identifier(op_name) = &**op_name else {
                panic!("Expected Identifier {op_name}")
            };

            let mut param_names = Vec::new();
            for p in op_params.iter() {
                match p {
                    AST::Identifier(ident) => param_names.push(ident.clone()),
                    e => panic!("Expected Identifier received {e}"),
                }
            }

            HeapObject::Func(Func::Defined(
                Some(op_name.clone()),
                param_names,
                ast.next().unwrap().clone(),
            ))
            .heap_alloc_named(&op_name, interpreter)?;
        }
        e => return Err(InterpreterError::InvalidOperator((*e).clone())),
    };
    assert!(ast.len() == 0);
    Ok(())
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
    ))
    .stack_alloc(interpreter)?;

    interpreter.push_data(obj);

    assert!(ast.len() == 0);

    Ok(())
}

pub fn write(interpreter: &mut InterpreterContext, n: usize) -> InterpreterResult<()> {
    let mut data = Vec::new();
    for _ in 0..n {
        data.push(interpreter.pop_data()?);
    }
    data.reverse();
    for d in data {
        print!("{} ", d.deref(interpreter)?);
    }
    println!();

    Ok(())
}

macro_rules! bin_op {
    ($name:ident, $l:ident, $r:ident, $calc:expr) => {
        pub fn $name(interpreter: &mut InterpreterContext, n: usize) -> InterpreterResult<()> {
            let mut objs = Vec::new();
            for _ in 0..n {
                objs.push(interpreter.pop_data()?);
            }
            objs.reverse();

            let drain = objs.drain(..);
            let out = drain.fold(None, |out, obj| {
                match (out, obj.deref(interpreter).unwrap()) {
                    (None, v) => Some(v.clone_to_unallocated()),
                    (
                        Some(UnallocatedObject::Value(Literal::Numeric($l))),
                        ObjectRef::Value(Literal::Numeric($r)),
                    ) => Some(UnallocatedObject::Value(Literal::Numeric($calc))),
                    _ => panic!(),
                }
            });

            let stack_obj = out
                .ok_or(InterpreterError::FailedOperation)?
                .stack_alloc(interpreter)?;

            interpreter.push_data(stack_obj);

            return Ok(());
        }
    };
}

bin_op!(add, l, r, l + r);
bin_op!(sub, l, r, l - r);
bin_op!(mul, l, r, l * r);
bin_op!(div, l, r, l / r);
