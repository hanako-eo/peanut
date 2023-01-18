use std::{cell::RefCell, collections::HashSet, rc::Rc};

use super::ptr::{GarbageBox, WeakRef};

#[derive(Debug)]
pub struct GarbageCollector<T: Eq> {
    pub heap: HashSet<WeakRef<T>>,
}

impl<T: Eq> GarbageCollector<T> {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            heap: HashSet::new(),
        }))
    }

    pub fn push(&mut self, ref_value: GarbageBox<T>) {
        self.heap.insert(ref_value.into());
    }

    pub fn pop(&mut self, ref_value: GarbageBox<T>) {
        self.heap.remove(&ref_value.into());
    }

    pub fn try_pop(&mut self, ref_value: GarbageBox<T>) -> Option<()> {
        if ref_value.strong_count() == 1 {
            self.pop(ref_value);
            Some(())
        } else {
            None
        }
    }
}
