use core::error::LispError;
use core::literal::Numeric;
use std::mem;
use std::ops::Deref;
use std::{env, fs::File, io::Read};

use core::{literal::Literal, parser::ast::AST, LexerParser};

use core::token::span::TotalSpan;

use dylib::DynamicLibrary;

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
        .write()
        .unwrap()
        .add_file(file_path, contents.clone());

    let ast = LexerParser::from_string(id, contents, &interpreter.error_writer.read().unwrap())
        .map_err(|_| {
            InterpreterError::spanned(InterpreterErrorKind::ErrorInParsingImport, total_span)
        })?;

    for node in ast {
        interpreter.interpret(&node)?;
    }

    Ok(())
}

pub fn let_(interpreter: &InterpreterContext, mut ast: Vec<&AST>) -> InterpreterResult<usize> {
    if ast.len() != 2 {
        return Err(InterpreterError::optional_span(
            InterpreterErrorKind::InvalidLetStatement,
            ast.total_span(),
        ));
    }

    let _ = ast.pop().unwrap();
    let bindings = ast.pop().unwrap();

    let AST::Operation(first, others, _) = bindings else {
        return Err(InterpreterError::spanned(
            InterpreterErrorKind::InvalidLetBindingForm,
            bindings.span(),
        ));
    };
    let mut binding_list = vec![first.deref()];
    binding_list.extend(others);

    let mut named_bindings = binding_list
        .drain(..)
        .map(|b| match b {
            AST::Operation(name, value, op_span) => {
                let bind_name = match &**name {
                    AST::Identifier(name, _) => name,
                    e => {
                        return Err(InterpreterError::spanned(
                            InterpreterErrorKind::InvalidLetBindingName,
                            e.span(),
                        ))
                    }
                };

                if value.len() > 1 {
                    return Err(InterpreterError::spanned(
                        InterpreterErrorKind::InvalidLetBindingForm,
                        *op_span,
                    ));
                }

                interpreter.interpret(&value[0])?;
                let value = interpreter.stack.pop_data()?;

                Ok((bind_name.clone(), value.heap_alloc(interpreter)?))
            }
            e => {
                return Err(InterpreterError::spanned(
                    InterpreterErrorKind::InvalidLetBindingForm,
                    e.span(),
                ))
            }
        })
        .collect::<Vec<_>>();

    if let Some(Err(err)) = named_bindings.iter().find(|e| e.is_err()) {
        return Err(err.clone());
    }

    {
        let mut top_frame = interpreter.stack.top_frame()?;
        for (name, obj) in named_bindings.drain(..).map(|x| x.unwrap()) {
            top_frame.insert_local(&name, obj);
        }
    }

    Ok(1)
}

pub fn if_macro(interpreter: &InterpreterContext, mut ast: Vec<&AST>) -> InterpreterResult<usize> {
    if ast.len() != 3 {
        Err(InterpreterError::spanned(
            InterpreterErrorKind::ExpectedNParams(3, ast.len()),
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
        Ok(1)
    } else {
        Ok(2)
    }
}

pub fn define(interpreter: &InterpreterContext, mut ast: Vec<&AST>) -> InterpreterResult<()> {
    if ast.len() > 2 || ast.is_empty() {
        return Err(InterpreterError::new(
            InterpreterErrorKind::ExpectedNParams(2, ast.len()),
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
            InterpreterErrorKind::ExpectedNParams(2, ast.len()),
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

pub fn link_external(interpreter: &InterpreterContext, n: usize) -> InterpreterResult<()> {
    let mut data = Vec::new();
    for _ in 0..n {
        data.push(interpreter.stack.pop_data()?);
    }
    data.reverse();

    let file_name = {
        let ObjectRef::Object(obj) = data[0].deref(interpreter)? else {
            return Err(InterpreterError::new(InterpreterErrorKind::ExpectedString));
        };

        let HeapObject::String(file_name) = &*obj.deref() else {
            return Err(InterpreterError::new(InterpreterErrorKind::ExpectedString));
        };

        file_name.clone()
    };

    let ld_path = std::path::Path::new(&file_name);
    {
        let ObjectRef::Object(obj) = data[1].deref(interpreter)? else {
            return Err(InterpreterError::new(InterpreterErrorKind::ExpectedString));
        };

        let HeapObject::String(symbol) = &*obj.deref() else {
            return Err(InterpreterError::new(InterpreterErrorKind::ExpectedString));
        };

        let lib = match DynamicLibrary::open(Some(ld_path)) {
            Err(error) => return Err(LispError::new(InterpreterErrorKind::CannotLoadLib(file_name))),
            Ok(lib) => lib,
        };

        let func: extern "C" fn() = unsafe {
            match lib.symbol(&symbol) {
                Err(error) => return Err(LispError::new(InterpreterErrorKind::CannotCall(symbol.clone()))),
                Ok(cosine) => mem::transmute::<*mut u8, _>(cosine),
            }
        };

        func();
    };

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
    ($name:ident, $l:ident, $r:ident, $calc:expr, $op:expr) => {
        pub fn $name(interpreter: &InterpreterContext, n: usize) -> InterpreterResult<()> {
            let mut objs = Vec::new();
            for _ in 0..n {
                objs.push(interpreter.stack.pop_data()?);
            }
            objs.reverse();

            let drain = objs.drain(..);
            let out = drain.fold(Ok(None), |out, obj| {
                match (out, obj.deref(interpreter).unwrap()) {
                    (Ok(None), v) => Ok(Some(v.clone_to_unallocated())),
                    (
                        Ok(Some(UnallocatedObject::Value(Literal::Numeric($l)))),
                        ObjectRef::Value(Literal::Numeric($r)),
                    ) => Ok(Some(UnallocatedObject::Value(Literal::Numeric($calc)))),
                    (
                        Ok(Some(UnallocatedObject::Value(Literal::Numeric($l)))),
                        ObjectRef::Object(obj),
                    ) => match obj.deref() {
                        HeapObject::Value(Literal::Numeric(r)) => {
                            let $r = *r;
                            Ok(Some(UnallocatedObject::Value(Literal::Numeric($calc))))
                        }
                        $r => Err(InterpreterError::new(
                            InterpreterErrorKind::CannotPerformOperation(
                                format!("{}", $op),
                                $l.interpreter_fmt(interpreter),
                                $r.interpreter_fmt(interpreter),
                            ),
                        )),
                    },
                    (Ok(Some($l)), $r) => Err(InterpreterError::new(
                        InterpreterErrorKind::CannotPerformOperation(
                            format!("{}", $op),
                            $l.interpreter_fmt(interpreter),
                            $r.interpreter_fmt(interpreter),
                        ),
                    )),
                    (e @ Err(_), _) => e,
                }
            })?;

            let stack_obj = out
                .ok_or(InterpreterError::new(
                    InterpreterErrorKind::ExpectedNOrMoreParams(1usize.., 0),
                ))?
                .stack_alloc(interpreter)?;

            interpreter.stack.push_data(stack_obj);

            return Ok(());
        }
    };
}

bin_op!(add, l, r, l + r, "+");
bin_op!(sub, l, r, l - r, "-");
bin_op!(mul, l, r, l * r, "*");
bin_op!(div, l, r, l / r, "/");

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
            interpreter
                .stack
                .push_data(StackObject::Value(Literal::Boolean(out?)));

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
            InterpreterErrorKind::ExpectedNParams(1, n),
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

    interpreter
        .stack
        .push_data(StackObject::Value(Literal::Boolean(val)));
    Ok(())
}

pub fn car(interpreter: &InterpreterContext, n: usize) -> InterpreterResult<()> {
    if n != 1 {
        return Err(InterpreterError::new(
            InterpreterErrorKind::ExpectedNParams(1, n),
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
            InterpreterErrorKind::ExpectedNParams(1, n),
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
            InterpreterErrorKind::ExpectedNParams(2, n),
        ));
    }

    let ptr = {
        let tail = interpreter.stack.pop_data()?;
        let head = interpreter
            .stack
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

pub fn file_to_string(interpreter: &InterpreterContext, n: usize) -> InterpreterResult<()> {
    if n != 1 {
        return Err(InterpreterError::new(
            InterpreterErrorKind::ExpectedNParams(2, n),
        ));
    }

    let file_name = interpreter.stack.pop_data()?;

    let contents = {
        let ObjectRef::Object(obj) = file_name.deref(interpreter)? else {
            return Err(InterpreterError::new(InterpreterErrorKind::ExpectedString));
        };

        let HeapObject::String(file_name) = &*obj.deref() else {
            return Err(InterpreterError::new(InterpreterErrorKind::ExpectedString));
        };

        let contents = std::fs::read_to_string(file_name).map_err(|_| {
            InterpreterError::new(InterpreterErrorKind::CannotOpenFile(file_name.clone()))
        })?;

        contents
    };

    let obj = UnallocatedObject::String(contents).stack_alloc(interpreter)?;
    interpreter.stack.push_data(obj);

    Ok(())
}

pub fn string_to_chars(interpreter: &InterpreterContext, n: usize) -> InterpreterResult<()> {
    if n != 1 {
        return Err(InterpreterError::new(
            InterpreterErrorKind::ExpectedNParams(2, n),
        ));
    }

    let string = {
        let s = interpreter.stack.pop_data()?;

        let ObjectRef::Object(obj) = s.deref(interpreter)? else {
            return Err(InterpreterError::new(InterpreterErrorKind::ExpectedString));
        };

        let HeapObject::String(string) = &*obj.deref() else {
            return Err(InterpreterError::new(InterpreterErrorKind::ExpectedString));
        };

        string.clone()
    };

    let chars = string.chars();

    let head = chars
        .rev()
        .fold(Ok(ObjectPointer::Null), |tail, next| match tail {
            Ok(tail_pointer) => {
                let head =
                    UnallocatedObject::Value(Literal::Character(next)).heap_alloc(interpreter)?;
                UnallocatedObject::List(head, tail_pointer).heap_alloc(interpreter)
            }
            e => e,
        })?;

    interpreter.stack.push_data(head.stack_alloc(interpreter)?);

    Ok(())
}

macro_rules! string_parse {
    ($name:ident,$ty:ident,$out:ident,$out_alloc:expr,$ty_name:expr) => {
        pub fn $name(interpreter: &InterpreterContext, n: usize) -> InterpreterResult<()> {
            if n != 1 {
                return Err(InterpreterError::new(
                    InterpreterErrorKind::ExpectedNParams(2, n),
                ));
            }

            let string = {
                let s = interpreter.stack.pop_data()?;

                let ObjectRef::Object(obj) = s.deref(interpreter)? else {
                    return Err(InterpreterError::new(InterpreterErrorKind::ExpectedString));
                };

                let HeapObject::String(string) = &*obj.deref() else {
                    return Err(InterpreterError::new(InterpreterErrorKind::ExpectedString));
                };

                string.clone()
            };

            let $out = string.parse::<$ty>().map_err(|_| {
                InterpreterError::new(InterpreterErrorKind::CannotConvertType(
                    "String".into(),
                    $ty_name.into(),
                ))
            })?;

            interpreter.stack.push_data($out_alloc);

            Ok(())
        }
    };
}

string_parse!(
    string_to_int,
    i32,
    v,
    StackObject::Value(Literal::Numeric(Numeric::Int(v))),
    "Int"
);

string_parse!(
    string_to_float,
    f32,
    v,
    StackObject::Value(Literal::Numeric(Numeric::Float(v))),
    "Int"
);
