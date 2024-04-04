use std::{cmp::Ordering, ops::Deref};

use crate::{
    deref::InterpreterDeref,
    error::{InterpreterError, InterpreterErrorKind},
    object::{HeapObject, ObjectPointer, ObjectRef, StackObject},
    print::InterpreterPrint,
    InterpreterContext, InterpreterResult,
};

pub trait InterpreterComparison<T> {
    fn object_eq(&self, rhs: &T, interpreter: &InterpreterContext) -> InterpreterResult<bool>;

    fn object_cmp(&self, rhs: &T, interpreter: &InterpreterContext) -> InterpreterResult<Ordering>;
}

impl InterpreterComparison<Self> for StackObject {
    fn object_eq(&self, rhs: &Self, interpreter: &InterpreterContext) -> InterpreterResult<bool> {
        Ok(self == rhs
            || InterpreterComparison::object_eq(
                &self.deref(interpreter)?,
                &rhs.deref(interpreter)?,
                interpreter,
            )?)
    }

    fn object_cmp(
        &self,
        rhs: &Self,
        interpreter: &InterpreterContext,
    ) -> InterpreterResult<Ordering> {
        InterpreterComparison::object_cmp(
            &self.deref(interpreter)?,
            &rhs.deref(interpreter)?,
            interpreter,
        )
    }
}

impl InterpreterComparison<Self> for ObjectPointer {
    fn object_eq(&self, rhs: &Self, interpreter: &InterpreterContext) -> InterpreterResult<bool> {
        Ok(self == rhs
            || InterpreterComparison::object_eq(
                &self.deref(interpreter)?,
                &rhs.deref(interpreter)?,
                interpreter,
            )?)
    }

    fn object_cmp(
        &self,
        rhs: &Self,
        interpreter: &InterpreterContext,
    ) -> InterpreterResult<Ordering> {
        InterpreterComparison::object_cmp(
            &self.deref(interpreter)?,
            &rhs.deref(interpreter)?,
            interpreter,
        )
    }
}

impl InterpreterComparison<Self> for HeapObject {
    fn object_eq(&self, rhs: &Self, interpreter: &InterpreterContext) -> InterpreterResult<bool> {
        match (self, rhs) {
            // (HeapObject::Value(l), HeapObject::Value(r)) => Ok(l == r),
            (HeapObject::String(l), HeapObject::String(r)) => Ok(l == r),
            (HeapObject::Func(l), HeapObject::Func(r)) => Ok(l == r),
            (HeapObject::List(l, ls), HeapObject::List(r, rs)) => {
                Ok(l.object_eq(r, interpreter)? && ls.object_eq(rs, interpreter)?)
            }
            _ => Ok(false),
        }
    }

    fn object_cmp(
        &self,
        rhs: &Self,
        interpreter: &InterpreterContext,
    ) -> InterpreterResult<Ordering> {
        match (self, rhs) {
            (HeapObject::Value(l), HeapObject::Value(r)) => Ok(l.cmp(r)),
            (l, r) => Err(InterpreterError::new(InterpreterErrorKind::CannotCompare(
                dbg!(l).interpreter_fmt(interpreter),
                dbg!(r).interpreter_fmt(interpreter),
            ))),
        }
    }
}

impl InterpreterComparison<Self> for ObjectRef<'_> {
    fn object_eq(&self, rhs: &Self, interpreter: &InterpreterContext) -> InterpreterResult<bool> {
        match (self, rhs) {
            (ObjectRef::Null, ObjectRef::Null) => Ok(true),
            (ObjectRef::Value(l), ObjectRef::Value(r)) => Ok(l == r),
            (ObjectRef::Object(l), ObjectRef::Object(r)) => {
                let l = &*l.deref();
                let r = &*r.deref();
                l.object_eq(r, interpreter)
            }
            (ObjectRef::Object(l), r @ ObjectRef::Value(_))
            | (r @ ObjectRef::Value(_), ObjectRef::Object(l)) => {
                r.object_eq(&*l.deref(), interpreter)
            }
            _ => Ok(false),
        }
    }

    fn object_cmp(
        &self,
        rhs: &Self,
        interpreter: &InterpreterContext,
    ) -> InterpreterResult<Ordering> {
        match (self, rhs) {
            (ObjectRef::Value(l), ObjectRef::Value(r)) => Ok(l.cmp(r)),
            (ObjectRef::Object(l), ObjectRef::Object(r)) => {
                let l = &*l.deref();
                let r = &*r.deref();
                l.object_cmp(r, interpreter)
            }
            (ObjectRef::Object(l), r @ ObjectRef::Value(_))
            | (r @ ObjectRef::Value(_), ObjectRef::Object(l)) => {
                r.object_cmp(&*l.deref(), interpreter)
            }
            (l, r) => Err(InterpreterError::new(InterpreterErrorKind::CannotCompare(
                l.interpreter_fmt(interpreter),
                r.interpreter_fmt(interpreter),
            ))),
        }
    }
}

impl InterpreterComparison<HeapObject> for ObjectRef<'_> {
    fn object_eq(
        &self,
        rhs: &HeapObject,
        interpreter: &InterpreterContext,
    ) -> InterpreterResult<bool> {
        match (self, rhs) {
            (ObjectRef::Value(l), HeapObject::Value(r)) => Ok(l == r),
            _ => Ok(false),
        }
    }

    fn object_cmp(
        &self,
        rhs: &HeapObject,
        interpreter: &InterpreterContext,
    ) -> InterpreterResult<Ordering> {
        match (self, rhs) {
            (ObjectRef::Value(l), HeapObject::Value(r)) => Ok(l.cmp(r)),
            (l, r) => Err(InterpreterError::new(InterpreterErrorKind::CannotCompare(
                l.interpreter_fmt(interpreter),
                r.interpreter_fmt(interpreter),
            ))),
        }
    }
}
