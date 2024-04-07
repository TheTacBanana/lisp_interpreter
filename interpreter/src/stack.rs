use std::{
    ops::{Deref, DerefMut},
    sync::{
        MappedRwLockWriteGuard, RwLock, RwLockWriteGuard,
    },
};

use crate::{
    deref::InterpreterDeref, error::{InterpreterError, InterpreterErrorKind}, frame::Frame, object::{ObjectPointer, ObjectRef, StackObject}, InterpreterContext, InterpreterResult
};

pub struct InterpreterStack {
    pub frame: RwLock<Vec<Frame>>,
    pub data: RwLock<Vec<StackObject>>,
}

impl InterpreterStack {
    pub fn new() -> Self {
        Self {
            frame: RwLock::new(Vec::new()),
            data: RwLock::new(Vec::new()),
        }
    }

    pub fn stack_trace(&self) {
        println!("Stack Trace:");
        for s in self.frame.read().unwrap().iter() {
            println!("{s}")
        }
    }

    pub fn get_stack_object<'a>(
        &'a self,
        frame: usize,
        index: usize,
    ) -> InterpreterResult<ObjectPointer> {
        let err = InterpreterError::new(InterpreterErrorKind::StackIndexOutOfRange);

        let frames = self.frame.read().unwrap();
        let frame = frames.get(frame).ok_or(err.clone())?;
        frame.get_local_by_index(index).ok_or(err)
    }

    pub fn get_stack_ref<'a>(
        &'a self,
        frame: usize,
        index: usize,
        i: &'a InterpreterContext,
    ) -> InterpreterResult<ObjectRef<'a>> {
        let err = InterpreterError::new(InterpreterErrorKind::StackIndexOutOfRange);

        let frames = self.frame.read().unwrap();
        let frame = frames.get(frame).ok_or(err.clone())?;
        let obj = frame.get_local_by_index(index).ok_or(err)?;
        obj.deref(i)
    }

    pub fn push_frame(&self, frame: Frame) {
        self.frame.write().unwrap().push(frame);
    }

    pub fn pop_frame(&self) -> InterpreterResult<Frame> {
        self.frame
            .write()
            .unwrap()
            .pop()
            .ok_or(InterpreterError::new(InterpreterErrorKind::EmptyStack))
    }

    pub fn top_frame<'a>(&'a self) -> InterpreterResult<FrameRef<'a>> {
        RwLockWriteGuard::try_map(self.frame.write().unwrap(), |l| l.last_mut())
            .map(|x| x.into())
            .map_err(|_| InterpreterError::new(InterpreterErrorKind::EmptyStack))
    }

    pub fn push_data(&self, obj: StackObject) {
        self.data.write().unwrap().push(obj)
    }

    pub fn pop_data(&self) -> InterpreterResult<StackObject> {
        self.data
            .write()
            .unwrap()
            .pop()
            .ok_or(InterpreterError::new(InterpreterErrorKind::EmptyDataStack))
    }
}

pub struct FrameRef<'a>(MappedRwLockWriteGuard<'a, Frame>);

impl<'a> Into<FrameRef<'a>> for MappedRwLockWriteGuard<'a, Frame> {
    fn into(self) -> FrameRef<'a> {
        FrameRef(self)
    }
}

impl Deref for FrameRef<'_> {
    type Target = Frame;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

impl DerefMut for FrameRef<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.deref_mut()
    }
}

// pub struct StackObjectRef<'a>(MappedRwLockReadGuard<'a, StackObject>);

// impl<'a> Into<StackObjectRef<'a>> for MappedRwLockReadGuard<'a, StackObject> {
//     fn into(self) -> StackObjectRef<'a> {
//         StackObjectRef(self)
//     }
// }

// impl Deref for StackObjectRef<'_> {
//     type Target = StackObject;

//     fn deref(&self) -> &Self::Target {
//         self.0.deref()
//     }
// }
