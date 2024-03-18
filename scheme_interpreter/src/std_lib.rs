use scheme_core::parser::token::{Literal, Numeric};

use crate::{object::{Object, ObjectPointer}, InterpreterContext, InterpreterResult};



pub fn add(interpreter: &mut InterpreterContext, n: usize) -> InterpreterResult<()> {
    let mut pointers = Vec::new();
    for _ in 0..n {
        pointers.push(interpreter.pop_data()?);
    }

    let mut values = Vec::new();
    for p in pointers {
        values.push(interpreter.deref_pointer(p.clone())?)
    }

    let drain = values.drain(..);
    let mut temp = Literal::Numeric(Numeric::Int(0));
    for val in drain {
        temp = match (temp, val) {
            (Literal::Numeric(l), Object::Value(Literal::Numeric(r))) => Literal::Numeric(l + *r),
            e => panic!("{e:?}")
        };
    }

    let p = interpreter.allocate_object(None, Object::Value(temp));
    interpreter.push_data(p);

    return Ok(())
}