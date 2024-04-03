use crate::{
    object::{HeapObject, ObjectPointer, ObjectRef, StackObject},
    InterpreterContext,
};

pub trait InterpreterPrint {
    fn interpreter_fmt(&self, i: &InterpreterContext) -> String;
}

impl InterpreterPrint for StackObject {
    fn interpreter_fmt(&self, i: &InterpreterContext) -> String {
        match self {
            StackObject::Value(v) => format!("{v}"),
            StackObject::Ref(r) => format!("{}", r.interpreter_fmt(i)),
        }
    }
}

impl InterpreterPrint for ObjectPointer {
    fn interpreter_fmt(&self, i: &InterpreterContext) -> String {
        match self {
            ObjectPointer::Null => format!("()"),
            ObjectPointer::Stack(f, p) => i
                .stack
                .frame
                .read()
                .unwrap()
                .get(*f)
                .unwrap()
                .get_local_by_index(*p)
                .unwrap()
                .interpreter_fmt(i),
            ObjectPointer::Heap(p) => i.heap.get_heap_object(**p).unwrap().interpreter_fmt(i),
        }
    }
}

impl InterpreterPrint for HeapObject {
    fn interpreter_fmt(&self, i: &InterpreterContext) -> String {
        match self {
            HeapObject::Value(v) => format!("{v}"),
            HeapObject::Func(f) => format!("{f}"),
            HeapObject::String(s) => format!("\"{s}\""),
            HeapObject::List(h, t) => {
                format!("{}:{}", h.interpreter_fmt(i), t.interpreter_fmt(i))
            }
        }
    }
}

impl InterpreterPrint for ObjectRef<'_> {
    fn interpreter_fmt(&self, i: &InterpreterContext) -> String {
        match self {
            ObjectRef::Null => format!("()"),
            ObjectRef::Value(v) => format!("{v}"),
            ObjectRef::Object(obj) => obj.interpreter_fmt(i),
        }
    }
}
