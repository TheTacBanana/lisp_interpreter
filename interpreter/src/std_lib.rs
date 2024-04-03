use std::ops::Deref;
use std::{env, fs::File, io::Read};

use core::{literal::Literal, parser::ast::AST, LexerParser};

use core::token::span::TotalSpan;

use crate::comparison::InterpreterComparison;
use crate::object::UnallocatedObject;
use crate::print::InterpreterPrint;
use crate::{
    alloc::{InterpreterHeapAlloc, InterpreterStackAlloc},
    deref::InterpreterDeref,
    func::Func,
    object::{HeapObject, ObjectPointer, ObjectRef, StackObject},
    InterpreterContext, InterpreterError, InterpreterErrorKind, InterpreterResult,
};

pub fn stack_trace(interpreter: &InterpreterContext, _n: usize) -> InterpreterResult<()> {
    interpreter.stack.stack_trace();
    Ok(())
}

pub fn heap_dump(interpreter: &InterpreterContext, _n: usize) -> InterpreterResult<()> {
    interpreter.heap.dump(&interpreter);
    Ok(())
}

pub fn import(interpreter: &InterpreterContext, mut ast: Vec<&AST>) -> InterpreterResult<()> {
    if ast.is_empty() {
        return Err(InterpreterError::new(InterpreterErrorKind::EmptyImport));
    }
    let cur_file_id = ast[0].span().file_id;
    let total_span = ast.total_span().unwrap();

    let mut path = Vec::new();
    for ident in ast.drain(..) {
        let name = match ident {
            AST::Identifier(name, _) => name,
            e => {
                return Err(InterpreterError::spanned(
                    InterpreterErrorKind::InvalidInImport,
                    e.span(),
                ))?
            }
        };
        path.push(name.clone());
    }

    let mut file_path = interpreter
        .error_writer
        .read()
        .unwrap()
        .id_to_path
        .get(&cur_file_id)
        .map(|p| {
            let mut p = p.clone();
            p.pop();
            p
        })
        .unwrap_or(env::current_dir().unwrap())
        .clone();

    file_path = path.iter().fold(file_path, |l, r| l.join(r));
    file_path.set_extension("scm");

    if interpreter
        .error_writer
        .read()
        .unwrap()
        .already_loaded(&file_path)
    {
        return Ok(());
    }

    let mut file = File::open(file_path.clone()).map_err(|_| {
        InterpreterError::spanned(
            InterpreterErrorKind::ImportNotFound(file_path.to_str().unwrap().to_string()),
            total_span,
        )
    })?;
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();

    let id = interpreter
        .error_writer
        .write().unwrap()
        .add_file(file_path, contents.clone());

    let ast = LexerParser::from_string(id, contents, &interpreter.error_writer.read().unwrap()).map_err(|_| {
        InterpreterError::spanned(InterpreterErrorKind::ErrorInParsingImport, total_span)
    })?;

    for node in ast {
        interpreter.interpret(&node)?;
    }

    Ok(())
}

pub fn if_macro(
    interpreter: &InterpreterContext,
    mut ast: Vec<&AST>,
) -> InterpreterResult<*const AST> {
    if ast.len() != 3 {
        Err(InterpreterError::spanned(
            InterpreterErrorKind::ExpectedNParams {
                expected: 3,
                received: ast.len(),
            },
            {
                if let Some(span) = ast
                    .iter()
                    .skip(3)
                    .map(|l| l.span())
                    .reduce(|l, r| l.max_span(r))
                {
                    span
                } else {
                    ast.last().unwrap().span()
                }
            },
        ))?
    }

    let mut drain = ast.drain(..);
    let cond = drain.next().unwrap();

    interpreter.interpret(cond)?;
    let cond = interpreter.stack.pop_data()?;
    let result = match cond.deref(interpreter)? {
        ObjectRef::Value(Literal::Boolean(false)) => false,
        _ => true,
    };

    if result {
        Ok(drain.next().unwrap())
    } else {
        Ok(drain.nth(1).unwrap())
    }
}

pub fn define(interpreter: &InterpreterContext, mut ast: Vec<&AST>) -> InterpreterResult<()> {
    if ast.len() > 2 || ast.is_empty() {
        return Err(InterpreterError::new(
            InterpreterErrorKind::ExpectedNParams {
                expected: 2,
                received: ast.len(),
            },
        ));
    }

    let mut ast = ast.drain(..);
    match ast.next().unwrap() {
        // Define a value
        AST::Identifier(ident, _) => {
            interpreter.interpret(ast.next().unwrap())?;
            let p = interpreter.stack.pop_data()?;
            p.heap_alloc_named(ident, interpreter)?;
        }
        // Define a function
        AST::Operation(op_name, op_params, _) => {
            let AST::Identifier(op_name, _) = &**op_name else {
                return Err(InterpreterError::new(InterpreterErrorKind::CannotCall(
                    op_name.to_string(),
                )));
            };

            let mut param_names = Vec::new();
            for p in op_params.iter() {
                match p {
                    AST::Identifier(ident, _) => param_names.push(ident.clone()),
                    e => {
                        return Err(InterpreterError::spanned(
                            InterpreterErrorKind::InvalidFuncParamNames,
                            e.span(),
                        ))
                    }
                }
            }

            HeapObject::Func(Func::Defined(
                Some(op_name.clone()),
                param_names,
                ast.next().unwrap().clone(),
            ))
            .heap_alloc_named(op_name, interpreter)?;
        }
        e => {
            return Err(InterpreterError::new(InterpreterErrorKind::CannotCall(
                e.to_string(),
            )))
        }
    };
    Ok(())
}

pub fn lambda(interpreter: &InterpreterContext, mut ast: Vec<&AST>) -> InterpreterResult<()> {
    if ast.len() > 2 || ast.is_empty() {
        return Err(InterpreterError::new(
            InterpreterErrorKind::ExpectedNParams {
                expected: 2,
                received: ast.len(),
            },
        ));
    }

    let mut ast = ast.drain(..);
    let param_names = match ast.next().unwrap() {
        AST::Identifier(ident, _) => {
            vec![ident.clone()]
        }
        AST::Operation(op_name, op_params, _) => {
            let build_err = |ast: &AST| -> Result<(), InterpreterError> {
                Err(InterpreterError::spanned(
                    InterpreterErrorKind::IsNotParamName(ast.to_string()),
                    ast.span(),
                ))
            };

            let AST::Identifier(op_name, _) = &**op_name else {
                return build_err(op_name);
            };

            let mut param_names = vec![op_name.clone()];
            for p in op_params.iter() {
                match p {
                    AST::Identifier(ident, _) => param_names.push(ident.clone()),
                    e => build_err(e)?,
                }
            }

            param_names
        }
        AST::EmptyList(_) => Vec::new(),
        e => {
            return Err(InterpreterError::new(InterpreterErrorKind::CannotCall(
                e.to_string(),
            )))
        }
    };

    let obj = HeapObject::Func(Func::Defined(
        None,
        param_names,
        ast.next().unwrap().clone(),
    ))
    .stack_alloc(interpreter)?;

    interpreter.stack.push_data(obj);

    Ok(())
}

pub fn write(interpreter: &InterpreterContext, n: usize) -> InterpreterResult<()> {
    let mut data = Vec::new();
    for _ in 0..n {
        data.push(interpreter.stack.pop_data()?);
    }
    data.reverse();
    for d in data {
        print!("{}", d.interpreter_fmt(interpreter));
    }
    println!();

    Ok(())
}

macro_rules! bin_op {
    ($name:ident, $l:ident, $r:ident, $calc:expr) => {
        pub fn $name(interpreter: &InterpreterContext, n: usize) -> InterpreterResult<()> {
            let mut objs = Vec::new();
            for _ in 0..n {
                objs.push(interpreter.stack.pop_data()?);
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
                .ok_or(InterpreterError::new(InterpreterErrorKind::FailedOperation))?
                .stack_alloc(interpreter)?;

            interpreter.stack.push_data(stack_obj);

            return Ok(());
        }
    };
}

bin_op!(add, l, r, l + r);
bin_op!(sub, l, r, l - r);
bin_op!(mul, l, r, l * r);
bin_op!(div, l, r, l / r);

macro_rules! cmp_op {
    ($name:ident, $l:ident, $r:ident, $i:ident, $calc:expr) => {
        pub fn $name(interpreter: &InterpreterContext, n: usize) -> InterpreterResult<()> {
            let mut objs = Vec::new();
            for _ in 0..n {
                objs.push(interpreter.stack.pop_data()?);
            }
            objs.reverse();

            let drain = objs.windows(2);
            let out = drain.fold(Ok(true), |out, objs| match out {
                Ok(out) => Ok(out && {
                    let $l = &objs[0];
                    let $r = &objs[1];
                    let $i = &interpreter;
                    $calc
                }),
                e => e,
            });
            interpreter.stack.push_data(StackObject::Value(Literal::Boolean(out?)));

            Ok(())
        }
    };
}

cmp_op!(eq, l, r, i, l.object_eq(&r, i)?);
cmp_op!(lt, l, r, i, l.object_cmp(&r, i)?.is_lt());
cmp_op!(lteq, l, r, i, l.object_cmp(&r, i)?.is_le());
cmp_op!(gt, l, r, i, l.object_cmp(&r, i)?.is_gt());
cmp_op!(gteq, l, r, i, l.object_cmp(&r, i)?.is_ge());

pub fn empty(interpreter: &InterpreterContext, n: usize) -> InterpreterResult<()> {
    if n != 1 {
        return Err(InterpreterError::new(
            InterpreterErrorKind::ExpectedNParams {
                expected: 1,
                received: n,
            },
        ));
    }

    let val = {
        let stack_object = interpreter.stack.pop_data()?;
        let list = stack_object.deref(interpreter)?;
        match list {
            ObjectRef::Null => true,
            _ => false,
        }
    };

    interpreter.stack.push_data(StackObject::Value(Literal::Boolean(val)));
    Ok(())
}

pub fn car(interpreter: &InterpreterContext, n: usize) -> InterpreterResult<()> {
    if n != 1 {
        return Err(InterpreterError::new(
            InterpreterErrorKind::ExpectedNParams {
                expected: 1,
                received: n,
            },
        ));
    }

    let p = {
        let stack_object = interpreter.stack.pop_data()?;
        let list = stack_object.deref(interpreter)?;
        let p = match list {
            ObjectRef::Object(o) => match &*o.deref() {
                HeapObject::List(h, _) => h.clone(),
                _ => Err(InterpreterError::new(InterpreterErrorKind::ExpectedList))?,
            },
            _ => Err(InterpreterError::new(InterpreterErrorKind::ExpectedList))?,
        };
        p
    };
    let p = p.stack_alloc(interpreter)?;
    interpreter.stack.push_data(p);
    Ok(())
}

pub fn cdr(interpreter: &InterpreterContext, n: usize) -> InterpreterResult<()> {
    if n != 1 {
        return Err(InterpreterError::new(
            InterpreterErrorKind::ExpectedNParams {
                expected: 1,
                received: n,
            },
        ));
    }

    let p = {
        let stack_object = interpreter.stack.pop_data()?;
        let list = stack_object.deref(interpreter)?;
        let p = match list {
            ObjectRef::Object(o) => match &*o.deref() {
                HeapObject::List(_, t) => t.clone(),
                _ => Err(InterpreterError::new(InterpreterErrorKind::ExpectedList))?,
            },
            _ => Err(InterpreterError::new(InterpreterErrorKind::ExpectedList))?,
        };
        p
    };
    let p = p.stack_alloc(interpreter)?;
    interpreter.stack.push_data(p);
    Ok(())
}

pub fn cons(interpreter: &InterpreterContext, n: usize) -> InterpreterResult<()> {
    if n != 2 {
        return Err(InterpreterError::new(
            InterpreterErrorKind::ExpectedNParams {
                expected: 1,
                received: n,
            },
        ));
    }

    let ptr = {
        let tail = interpreter.stack.pop_data()?;
        let head = interpreter.stack
            .pop_data()?
            .deref(interpreter)?
            .clone_to_unallocated();

        let obj = if let StackObject::Ref(ObjectPointer::Null) = tail {
            UnallocatedObject::List(head.heap_alloc(interpreter)?, ObjectPointer::Null)
        } else {
            UnallocatedObject::List(head.heap_alloc(interpreter)?, tail.heap_alloc(interpreter)?)
        };
        obj.stack_alloc(interpreter)?
    };

    interpreter.stack.push_data(ptr);

    Ok(())
}
