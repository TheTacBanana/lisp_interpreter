use std::ops::Deref;

use crate::{
    deref::InterpreterDeref,
    object::{HeapObject, ObjectPointer, ObjectRef, StackObject},
    InterpreterContext, InterpreterResult,
};

pub trait InterpreterComparison {
    fn object_eq(&self, rhs: &Self, interpreter: &InterpreterContext) -> InterpreterResult<bool>;
}

impl InterpreterComparison for StackObject {
    fn object_eq(&self, rhs: &Self, interpreter: &InterpreterContext) -> InterpreterResult<bool> {
        Ok(self == rhs
            || InterpreterComparison::object_eq(
                &self.deref(interpreter)?,
                &rhs.deref(interpreter)?,
                interpreter,
            )?)
    }
}

impl InterpreterComparison for ObjectPointer {
    fn object_eq(&self, rhs: &Self, interpreter: &InterpreterContext) -> InterpreterResult<bool> {
        Ok(self == rhs
            || InterpreterComparison::object_eq(
                &self.deref(interpreter)?,
                &rhs.deref(interpreter)?,
                interpreter,
            )?)
    }
}

impl InterpreterComparison for HeapObject {
    fn object_eq(&self, rhs: &Self, interpreter: &InterpreterContext) -> InterpreterResult<bool> {
        match (self, rhs) {
            (HeapObject::Value(l), HeapObject::Value(r)) => Ok(l == r),
            (HeapObject::String(l), HeapObject::String(r)) => Ok(l == r),
            (HeapObject::Func(l), HeapObject::Func(r)) => Ok(l == r),
            (HeapObject::List(l, ls), HeapObject::List(r, rs)) => {
                Ok(l.object_eq(r, interpreter)? && ls.object_eq(rs, interpreter)?)
            }
            _ => Ok(false),
        }
    }
}

impl InterpreterComparison for ObjectRef<'_> {
    fn object_eq(&self, rhs: &Self, interpreter: &InterpreterContext) -> InterpreterResult<bool> {
        match (self, rhs) {
            (ObjectRef::Null, ObjectRef::Null) => Ok(true),
            (ObjectRef::Value(l), ObjectRef::Value(r)) => Ok(l == r),
            (ObjectRef::Object(l), ObjectRef::Object(r)) => {
                let l = &*l.deref();
                let r = &*r.deref();
                l.object_eq(r, interpreter)
            }
            _ => Ok(false),
        }
    }
}
