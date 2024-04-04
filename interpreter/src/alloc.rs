use crate::{
    object::{HeapObject, ObjectPointer, StackObject, UnallocatedObject},
    InterpreterContext, InterpreterError, InterpreterErrorKind, InterpreterResult,
};

pub trait InterpreterStackAlloc: Sized {
    fn stack_alloc(self, interpreter: &InterpreterContext) -> InterpreterResult<StackObject>;
}

impl InterpreterStackAlloc for ObjectPointer {
    fn stack_alloc(self, _interpreter: &InterpreterContext) -> InterpreterResult<StackObject> {
        Ok(StackObject::Ref(self))
    }
}

impl InterpreterStackAlloc for UnallocatedObject {
    fn stack_alloc(self, interpreter: &InterpreterContext) -> InterpreterResult<StackObject> {
        match self {
            UnallocatedObject::Value(v) => Ok(StackObject::Value(v)),
            o => Ok(StackObject::Ref(o.heap_alloc(interpreter)?)),
        }
    }
}

impl InterpreterStackAlloc for HeapObject {
    fn stack_alloc(self, interpreter: &InterpreterContext) -> InterpreterResult<StackObject> {
        Ok(StackObject::Ref(self.heap_alloc(interpreter)?))
    }
}

pub trait InterpreterHeapAlloc: Sized {
    fn heap_alloc_named(
        self,
        ident: &str,
        interpreter: &InterpreterContext,
    ) -> InterpreterResult<ObjectPointer> {
        let p = self.heap_alloc(interpreter)?;
        interpreter
            .ident_mapping
            .write()
            .unwrap()
            .insert(ident.to_string(), p.clone());
        Ok(p)
    }

    fn heap_alloc(self, interpreter: &InterpreterContext) -> InterpreterResult<ObjectPointer>;
}

impl InterpreterHeapAlloc for HeapObject {
    fn heap_alloc(self, interpreter: &InterpreterContext) -> InterpreterResult<ObjectPointer> {
        Ok(interpreter.heap.alloc_heap_object(self))
    }
}

impl InterpreterHeapAlloc for StackObject {
    fn heap_alloc(self, interpreter: &InterpreterContext) -> InterpreterResult<ObjectPointer> {
        match self {
            StackObject::Value(v) => HeapObject::Value(v).heap_alloc(interpreter),
            StackObject::Ref(ObjectPointer::Stack(f, p)) => {
                interpreter
                    .stack
                    .get_stack_object(f, p)
            }
            StackObject::Ref(o) => Ok(o),
        }
    }
}

impl InterpreterHeapAlloc for UnallocatedObject {
    fn heap_alloc(self, interpreter: &InterpreterContext) -> InterpreterResult<ObjectPointer> {
        match self {
            UnallocatedObject::Func(f) => HeapObject::Func(f),
            UnallocatedObject::String(s) => HeapObject::String(s),
            UnallocatedObject::List(head, tail) => HeapObject::List(head, tail),
            UnallocatedObject::Value(v) => HeapObject::Value(v),
            UnallocatedObject::Null => {
                return Err(InterpreterError::new(
                    InterpreterErrorKind::CannotAllocateNull,
                ))
            }
        }
        .heap_alloc(interpreter)
    }
}
