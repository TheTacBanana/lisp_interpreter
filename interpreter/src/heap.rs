use std::{
    ops::Deref,
    sync::{Arc, RwLock, RwLockReadGuard},
    thread::JoinHandle,
    time::Duration,
};

use crate::{object::{HeapObject, ObjectPointer, ObjectRef}, print::InterpreterPrint, InterpreterContext};

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

    pub fn dump(&self, context: &InterpreterContext) {
        println!("Heap Dump:");
        for (i, object) in self
            .store
            .read()
            .unwrap()
            .iter()
            .enumerate()
            .filter_map(|(i, o)| o.as_ref().map(|o| (i, o)))
        {
            if let (refs, Some(item)) = (Arc::strong_count(&object.1), self.get_heap_ref(i)) {
                //TODO:
                println!("[{i}] {} {refs}", item.interpreter_fmt(context))
            } else {
                println!("[{i}] Deref Failed {object:?}")
            }
        }
        println!()
    }

    pub fn get_heap_ref<'a>(&self, index: usize) -> Option<ObjectRef<'_>> {
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

        let mut store_write = self.store.write().unwrap();
        if id >= store_len {
            let extend = ((store_len)..=id).map(|_| None);
            store_write.extend(extend);
        }

        let mut heap_index = store_write.get_mut(id);
        let arc = Arc::new(id);
        let temp = ObjectPointer::Heap(arc.clone());
        let _ = heap_index.as_mut().unwrap().insert((obj, arc));
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
            delay: Duration::from_secs_f32(5.0),
        }
    }

    pub fn spawn_thread(self) -> JoinHandle<()> {
        std::thread::spawn(|| self.run())
    }

    pub fn run(self) {
        loop {
            std::thread::sleep(self.delay);

            let to_free = {
                let mut queue = {
                    let heap_ref = self.heap.store.read().unwrap();
                    let get_strong_count = |i: usize| -> Option<usize> {
                        heap_ref
                            .get(i)
                            .and_then(|x| x.as_ref())
                            .map(|(_, arc)| Arc::strong_count(&arc))
                    };

                    let queue = (0..heap_ref.len())
                        .filter(|i| get_strong_count(*i).filter(|c| *c <= 1).is_some())
                        .collect::<Vec<_>>();

                    queue
                };

                let mut to_free = Vec::new();

                while !queue.is_empty() {
                    let popped = queue.pop().unwrap();
                    match self.heap.get_heap_ref(popped).unwrap() {
                        ObjectRef::Object(o) => match &*o {
                            HeapObject::List(ObjectPointer::Heap(h), ObjectPointer::Heap(t)) => {
                                queue.push(*h.deref());
                                queue.push(*t.deref());
                            }
                            HeapObject::List(ObjectPointer::Heap(h), ObjectPointer::Null) => {
                                queue.push(*h.deref());
                            }
                            _ => (),
                        },
                        _ => (),
                    }

                    to_free.push(popped)
                }
                to_free
            };

            if !to_free.is_empty() {
                let mut write_lock = self.heap.store.write().unwrap();
                let mut free_vec = self.heap.free_slots.write().unwrap();

                for index in to_free.iter() {
                    write_lock[*index] = None;
                    free_vec.push(*index);
                }

                let last = write_lock.iter().enumerate().rev().find_map(|(i, o)| {
                    if o.is_some() {
                        Some(i)
                    } else {
                        None
                    }
                });

                if let Some(last) = last {
                    write_lock.truncate(last + 1);
                    *free_vec = free_vec.drain(..).filter(|i| *i < (last + 1)).collect()
                }
            }
        }
    }
}
