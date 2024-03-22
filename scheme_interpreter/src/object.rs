use scheme_core::literal::Literal;

use crate::func::Func;

#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum ObjectPointer {
    #[default]
    Null,
    Stack(usize, usize),
    Heap(usize),
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
            HeapObject::Value(_) => todo!(),
            HeapObject::String(_) => todo!(),
            HeapObject::List(_, _) => todo!(),
            HeapObject::Func(_) => todo!(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ObjectRef<'a> {
    Null,
    Value(Literal),
    Func(&'a Func),
    String(&'a String),
    List(Box<ObjectRef<'a>>, Box<ObjectRef<'a>>),
}

impl std::fmt::Display for ObjectRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectRef::List(head, tail) => write!(f, "{head} {tail}"),
            ObjectRef::Value(v) => write!(f, "{v}"),
            ObjectRef::Func(fid) => write!(f, "{fid}"),
            ObjectRef::String(s) => write!(f, "{s}"),
            ObjectRef::Null => write!(f, "()"),
        }
    }
}

impl ObjectRef<'_> {
    pub fn clone_to_unallocated(&self) -> UnallocatedObject {
        match self {
            ObjectRef::Value(v) => UnallocatedObject::Value(*v),
            ObjectRef::Func(f) => UnallocatedObject::Func((*f).clone()),
            ObjectRef::String(s) => UnallocatedObject::String((*s).clone()),
            ObjectRef::List(x, xs) => UnallocatedObject::List(
                Box::new((*x).clone_to_unallocated()),
                Box::new((*xs).clone_to_unallocated()),
            ),
            ObjectRef::Null => UnallocatedObject::Null,
        }
    }
}

#[derive(Debug)]
pub enum UnallocatedObject {
    Value(Literal),
    Func(Func),
    String(String),
    List(Box<UnallocatedObject>, Box<UnallocatedObject>),
    Null,
}
