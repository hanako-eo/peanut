use std::collections::HashSet;

use super::{context::Context, ptr::GarbageBox};

pub struct GarbageCollector<T: Eq> {
    heap: HashSet<GarbageBox<T>>,
}

impl<T: Eq> GarbageCollector<T> {
    pub fn new() -> Self {
        Self {
            heap: HashSet::new(),
        }
    }

    pub fn create_context(&mut self) -> Context<T> {
        Context::new(self)
    }

    pub fn push(&mut self, ref_value: GarbageBox<T>) {
        self.heap.insert(ref_value);
    }

    pub fn pop(&mut self, ref_value: GarbageBox<T>) {
        self.heap.remove(&ref_value);
    }

    pub fn try_pop(&mut self, ref_value: GarbageBox<T>) -> Option<()> {
        eprintln!("{}", ref_value.strong_count());
        if ref_value.strong_count() == 1 {
            self.heap.remove(&ref_value);
            Some(())
        } else {
            None
        }
    }
}
