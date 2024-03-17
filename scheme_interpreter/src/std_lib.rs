use scheme_core::parser::token::Literal;

use crate::{object::{Object, ObjectPointer}, InterpreterContext, InterpreterResult};



pub fn add(interpreter: &mut InterpreterContext, n: usize) -> InterpreterResult<()> {
    let mut pointers = Vec::new();
    for _ in 0..n {
        pointers.push(interpreter.pop_data()?);
    }

    let mut values = Vec::new();
    for p in pointers {
        values.push(interpreter.deref_pointer(&p)?)
    }

    let mut drain = values.drain(..);
    let mut temp = drain.next().unwrap();
    for val in drain {
        let temp = match (temp, val) {
            (Object::Value(Literal::Numeric(l)), Object::Value(Literal::Numeric(r))) => Object::Value(Literal::Numeric(*l + *r)),
            e => panic!("{e:?}")
        };
    }

    interpreter.push_data(ObjectPointer::Object(*temp));

    return Ok(())
}