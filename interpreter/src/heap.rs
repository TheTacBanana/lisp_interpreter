use core::literal::Literal;
use std::{
    borrow::Borrow,
    cell::RefCell,
    fmt::Display,
    ops::Deref,
    sync::{Arc, MappedRwLockReadGuard, RwLock, RwLockReadGuard},
    thread::JoinHandle,
    time::Duration,
};

use crate::object::{HeapObject, ObjectPointer, ObjectRef};

pub struct InterpreterHeap {
    pub free_slots: RwLock<Vec<usize>>,
    pub store: RwLock<Vec<Option<(HeapObject, Arc<usize>)>>>,
}

impl InterpreterHeap {
    pub fn new() -> (Arc<Self>, GarbageCollector) {
        let heap = Arc::new(Self {
            free_slots: RwLock::new(Vec::new()),
            store: RwLock::new(Vec::new()),
        });
        let gc = GarbageCollector::new(heap.clone());
        (heap, gc)
    }

    pub fn dump(&self) {
        println!("Heap Dump:");
        for (i, object) in self
            .store
            .read()
            .unwrap()
            .iter()
            .enumerate()
            .filter_map(|(i, o)| o.as_ref().map(|o| (i, o)))
        {
            if let (refs, Some(item)) = (Arc::strong_count(&object.1), self.get_heap_object(i)) {
                //TODO:
                println!("[{i}] {:?} {refs}", item)
            } else {
                println!("[{i}] Deref Failed {object:?}")
            }
        }
        println!()
    }

    pub fn get_heap_object<'a>(&self, index: usize) -> Option<ObjectRef<'_>> {
        let lock = self.store.read().unwrap();
        if lock.get(index).is_some_and(|x| x.is_some()) {
            Some(RwLockReadGuard::map(lock, |lock| &(&lock[index]).as_ref().unwrap().0).into())
        } else {
            None
        }
    }

    pub fn alloc_heap_object(&self, obj: HeapObject) -> ObjectPointer {
        let store_len = self.store.read().unwrap().len();
        let id = if self.free_slots.read().unwrap().len() > 0 {
            self.free_slots.write().unwrap().pop().unwrap()
        } else {
            store_len
        };

        println!("{id}");


        let mut store_write = self.store.write().unwrap();
        if id >= store_len {
            let extend = ((store_len)..=id).map(|_| None);
            store_write.extend(extend);
        }

        let mut heap_index = store_write.get_mut(id);
        let arc = Arc::new(id);
        let temp = ObjectPointer::Heap(arc.clone());
        heap_index.as_mut().unwrap().insert((obj, arc));
        temp
    }
}
pub struct GarbageCollector {
    delay: Duration,
    heap: Arc<InterpreterHeap>,
}

impl GarbageCollector {
    pub fn new(heap: Arc<InterpreterHeap>) -> Self {
        Self {
            heap,
            delay: Duration::from_secs_f32(1.0),
        }
    }

    pub fn spawn_thread(self) -> JoinHandle<()> {
        std::thread::spawn(|| self.run())
    }

    pub fn run(self) {
        loop {
            std::thread::sleep(self.delay);

            // println!("hi")

            // let get_strong_count = |i: usize, heap_ref: &mut InterpreterHeap| -> Option<usize> {
            //     heap_ref
            //         .store
            //         .get(i)
            //         .and_then(|x| x.as_ref())
            //         .map(|(_, arc)| Arc::strong_count(&arc))
            // };
            // println!("{:?}", self.heap);

            // let heap_ref = unsafe { self.heap.0.as_mut().unwrap() };

            // println!("hi {:?}", self.heap.read().unwrap().test);

            // let mut queue = (0..heap_ref.store.len())
            //     .filter_map(|i| dbg!(get_strong_count(i, heap_ref)))
            //     .filter(|&c| c <= 1)
            //     .collect::<Vec<_>>();

            // println!("hi");

            // while !queue.is_empty() {
            //     let popped = queue.pop().unwrap();
            //     println!("{popped}");
            //     if get_strong_count(popped, heap_ref).is_some_and(|i| i > 1) {
            //         continue;
            //     }

            //     if let Some(HeapObject::List(ObjectPointer::Heap(h), ObjectPointer::Heap(t))) =
            //         heap_ref.get_heap_object(popped)
            //     {
            //         queue.push(*h.deref());
            //         queue.push(*t.deref());
            //     }

            //     heap_ref.free_slots.push(popped);
            //     heap_ref.store[popped] = None
            // }
        }
    }
}
