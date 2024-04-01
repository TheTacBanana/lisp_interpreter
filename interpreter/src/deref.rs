use std::ops::Deref;

use crate::{
    object::{ObjectPointer, ObjectRef, StackObject}, InterpreterContext, InterpreterError, InterpreterErrorKind, InterpreterResult
};

pub trait InterpreterDeref {
    fn deref<'a>(&'a self, interpreter: &'a InterpreterContext)
        -> InterpreterResult<ObjectRef<'a>>;
}

impl InterpreterDeref for ObjectPointer {
    fn deref<'a>(
        &'a self,
        interpreter: &'a InterpreterContext,
    ) -> InterpreterResult<ObjectRef<'a>> {
        println!("{self}");
        match self {
            ObjectPointer::Null => Ok(ObjectRef::Null),
            ObjectPointer::Heap(p) => {
                interpreter
                    .heap
                    .get_heap_object(*p.deref())
                    .ok_or(InterpreterError::new(InterpreterErrorKind::PointerDoesNotExist))
            }
            ObjectPointer::Stack(frame, index) => {
                let frame = interpreter
                    .frame_stack
                    .get(*frame)
                    .ok_or(InterpreterError::new(InterpreterErrorKind::StackIndexOutOfRange))?;
                let obj = frame
                    .get_local_by_index(*index)
                    .ok_or(InterpreterError::new(InterpreterErrorKind::PointerDoesNotExist))?;
                obj.deref(interpreter)
            }
        }
    }
}

impl InterpreterDeref for StackObject {
    fn deref<'a>(
        &'a self,
        interpreter: &'a InterpreterContext,
    ) -> InterpreterResult<ObjectRef<'a>> {
        println!("{self:?}");
        match self {
            StackObject::Value(v) => Ok(ObjectRef::Value(*v)),
            StackObject::Ref(p) => p.deref(interpreter),
        }
    }
}
