use std::ops::Deref;

use crate::{
    object::{HeapObject, ObjectPointer, ObjectRef, StackObject}, InterpreterContext, InterpreterError, InterpreterErrorKind, InterpreterResult
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
        match self {
            ObjectPointer::Null => Ok(ObjectRef::Null), //TODO:
            ObjectPointer::Heap(p) => {
                let obj = interpreter
                    .heap
                    .get(*p.deref())
                    .and_then(|x| x.as_ref())
                    .ok_or(InterpreterError::new(InterpreterErrorKind::PointerDoesNotExist))?;
                obj.0.deref(interpreter)
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
        match self {
            StackObject::Value(v) => Ok(ObjectRef::Value(*v)),
            StackObject::Ref(p) => p.deref(interpreter),
        }
    }
}

impl InterpreterDeref for HeapObject {
    fn deref<'a>(
        &'a self,
        interpreter: &'a InterpreterContext,
    ) -> InterpreterResult<ObjectRef<'a>> {
        match self {
            HeapObject::Value(v) => Ok(ObjectRef::Value(*v)),
            HeapObject::String(s) => Ok(ObjectRef::String(s)),
            HeapObject::Func(f) => Ok(ObjectRef::Func(f)),
            HeapObject::List(x, xs) => Ok(ObjectRef::List(Box::new(x.deref(interpreter)?), {
                if *xs == ObjectPointer::Null {
                    Box::new(ObjectRef::Null)
                } else {
                    Box::new(xs.deref(interpreter)?)
                }
            })),
        }
    }
}
