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
            o => write!(f, "{o}"),
            // HeapObject::String(_) => todo!(),
            // HeapObject::List(_, _) => todo!(),
            // HeapObject::Func(_) => todo!(),
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
            o => write!(f, "{}", o),
        }
    }
}

// #[derive(Debug)]
// pub enum ObjectRef<'a> {
//     Null,
//     Value(Literal),
//     Func(InterpreterObjectRef<'a, Func>),
//     String(InterpreterObjectRef<'a, String>),
//     List(ObjectPointer, ObjectPointer),
// }

// impl std::fmt::Display for ObjectRef<'_> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             ObjectRef::List(head, tail) => write!(f, "{head} {tail}"),
//             ObjectRef::Value(v) => write!(f, "{v}"),
//             ObjectRef::Func(fid) => write!(f, "{fid}"),
//             ObjectRef::String(s) => write!(f, "{s}"),
//             ObjectRef::Null => write!(f, "()"),
//         }
//     }
// }
