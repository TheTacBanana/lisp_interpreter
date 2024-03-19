use scheme_core::parser::token::{Literal, Numeric};

use crate::{object::StackObject, InterpreterContext, InterpreterResult};



pub fn add(interpreter: &mut InterpreterContext, n: usize) -> InterpreterResult<()> {
    let mut stack_objects = Vec::new();
    for _ in 0..n {
        stack_objects.push(interpreter.pop_data()?);
    }

    let drain = stack_objects.drain(..);
    let mut temp = Literal::Numeric(Numeric::Int(0));
    for val in drain {
        temp = match (temp, val) {
            (Literal::Numeric(l), StackObject::Value(Literal::Numeric(r))) => Literal::Numeric(l + r),
            e => panic!("{e:?}")
        };
    }

    interpreter.push_data(StackObject::Value(temp));

    return Ok(())
}