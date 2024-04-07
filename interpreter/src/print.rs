use core::literal::Numeric;

use crate::{
    frame::Frame,
    object::{HeapObject, ObjectPointer, ObjectRef, StackObject, UnallocatedObject},
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
            ObjectPointer::Heap(p) => i.heap.get_heap_ref(**p).unwrap().interpreter_fmt(i),
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

impl InterpreterPrint for UnallocatedObject {
    fn interpreter_fmt(&self, i: &InterpreterContext) -> String {
        match self {
            UnallocatedObject::Value(v) => format!("{v}"),
            UnallocatedObject::Func(f) => format!("{f}"),
            UnallocatedObject::String(s) => format!("\"{s}\""),
            UnallocatedObject::List(h, t) =>
                format!("{}:{}", h.interpreter_fmt(i), t.interpreter_fmt(i)),
            UnallocatedObject::Null => format!("()"),
        }
    }
}

impl InterpreterPrint for Numeric {
    fn interpreter_fmt(&self, _i: &InterpreterContext) -> String {
        format!("{self}")
    }
}

impl InterpreterPrint for Frame {
    fn interpreter_fmt(&self, int: &InterpreterContext) -> String {
        let mut s = format!("Frame[{}]: {}\n", self.stack_index, self.name);

        for (ident, i) in self.ident_mapping.iter() {
            s.push_str(&format!(
                "{ident:?}: {:?}, ",
                self.locals
                    .get(*i)
                    .unwrap()
                    .clone()
                    .unwrap()
                    .interpreter_fmt(int)
            ));
        }
        s
    }
}
