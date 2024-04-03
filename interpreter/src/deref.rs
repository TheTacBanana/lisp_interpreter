use std::ops::Deref;

use crate::{
    object::{ObjectPointer, ObjectRef, StackObject},
    InterpreterContext, InterpreterError, InterpreterErrorKind, InterpreterResult,
};

pub trait InterpreterDeref {
    fn deref<'a, 'b>(&'b self, interpreter: &'a InterpreterContext)
        -> InterpreterResult<ObjectRef<'a>>;
}

impl InterpreterDeref for ObjectPointer {
    fn deref<'a, 'b>(
        &'b self,
        interpreter: &'a InterpreterContext,
    ) -> InterpreterResult<ObjectRef<'a>> {
        match self {
            ObjectPointer::Null => Ok(ObjectRef::Null),
            ObjectPointer::Heap(p) => {
                interpreter
                    .heap
                    .get_heap_object(*p.deref())
                    .ok_or(InterpreterError::new(
                        InterpreterErrorKind::PointerDoesNotExist,
                    ))
            }
            ObjectPointer::Stack(frame, index) => {
                interpreter.stack.get_stack_object(*frame, *index, &interpreter)
            }
        }
    }
}

impl InterpreterDeref for StackObject {
    fn deref<'a, 'b>(
        &'b self,
        interpreter: &'a InterpreterContext,
    ) -> InterpreterResult<ObjectRef<'a>> {
        match self {
            StackObject::Value(v) => Ok(ObjectRef::Value(*v)),
            StackObject::Ref(p) => p.deref(interpreter),
        }
    }
}
