use crate::{
    object::{HeapObject, ObjectPointer, ObjectRef, StackObject},
    InterpreterContext, InterpreterError, InterpreterResult,
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
            ObjectPointer::Null => Err(InterpreterError::NullDeref),
            ObjectPointer::Heap(p) => {
                let obj = interpreter.heap.get(*p).and_then(|x| x.as_ref()).ok_or(InterpreterError::PointerDoesNotExist)?;
                obj.deref(interpreter)
            },
            ObjectPointer::Stack(frame, index) => {
                let frame = interpreter.frame_stack.get(*frame).ok_or(InterpreterError::StackIndexOutOfRange)?;
                let obj = frame.get_local_by_index(*index).ok_or(InterpreterError::PointerDoesNotExist)?;
                obj.deref(interpreter)
            },
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
            HeapObject::List(x, xs) => Ok(ObjectRef::List(
                Box::new(x.deref(interpreter)?),
                Box::new(xs.deref(interpreter)?),
            )),
        }
    }
}
