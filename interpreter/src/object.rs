use std::{
    fmt::{Debug, Display},
    ops::Deref,
    sync::{Arc, MappedRwLockReadGuard, MappedRwLockWriteGuard},
};

use core::literal::Literal;

use crate::func::Func;

#[derive(Debug, Clone, PartialEq)]
pub enum StackObject {
    Value(Literal),
    Ref(ObjectPointer),
}

impl std::fmt::Display for StackObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackObject::Value(v) => write!(f, "{v}"),
            StackObject::Ref(p) => write!(f, "{p}"),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ObjectPointer {
    #[default]
    Null,
    Stack(usize, usize),
    Heap(Arc<usize>),
}

impl std::fmt::Display for ObjectPointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectPointer::Null => write!(f, "()"),
            ObjectPointer::Heap(p) => write!(f, "@{p}"),
            ObjectPointer::Stack(s, p) => write!(f, "@{s}:{p}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum HeapObject {
    Value(Literal),
    String(String),
    List(ObjectPointer, ObjectPointer),
    Func(Func),
}

impl std::fmt::Display for HeapObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HeapObject::Value(v) => write!(f, "{v}"),
            HeapObject::String(s) => write!(f, "\"{s}\""),
            HeapObject::List(h, t) => write!(f, "{h}:{t}"),
            HeapObject::Func(_fn) => write!(f, "{_fn}"),
        }
    }
}

#[derive(Debug)]
pub enum ObjectRef<'a> {
    Null,
    Value(Literal),
    Object(MappedRwLockReadGuard<'a, HeapObject>),
}

impl<'a> Into<ObjectRef<'a>> for MappedRwLockReadGuard<'a, HeapObject> {
    fn into(self) -> ObjectRef<'a> {
        ObjectRef::Object(self)
    }
}

impl std::fmt::Display for ObjectRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectRef::Object(o) => write!(f, "{}", o.deref()),
            ObjectRef::Null => write!(f, "()"),
            ObjectRef::Value(v) => write!(f, "{v}"),
        }
    }
}
