use std::collections::HashMap;

use super::{
    gc::GarbageCollector,
    ptr::{GarbageBox, WeakRef},
};

pub struct Context<'a, T: Eq> {
    parent: WeakRef<Context<'a, T>>,
    heap: HashMap<&'a str, GarbageBox<T>>,
    garbage: &'a mut GarbageCollector<T>,
}

impl<'a, T: Eq> Context<'a, T> {
    pub fn new(garbage: &'a mut GarbageCollector<T>) -> Self {
        Self {
            parent: WeakRef::empty(),
            heap: HashMap::new(),
            garbage,
        }
    }

    pub fn with_parent(
        parent: WeakRef<Context<'a, T>>,
        garbage: &'a mut GarbageCollector<T>,
    ) -> Self {
        Self {
            parent,
            heap: HashMap::new(),
            garbage,
        }
    }

    pub fn create(&mut self, key: &'a str, value: T) -> GarbageBox<T> {
        let inner: GarbageBox<T>;

        if let Some(mut gbox) = self.get(key) {
            *gbox.get_mut() = value;
            inner = gbox;
        } else {
            let value_ref = GarbageBox::new(value);
            inner = GarbageBox::clone(&value_ref);

            self.heap.insert(key, GarbageBox::clone(&value_ref));
            self.garbage.push(value_ref);
        }

        inner
    }

    pub fn delete(&mut self, key: &str) {
        let gbox = self.get(key).unwrap();
        self.heap.remove(key);
        self.garbage.try_pop(gbox);
    }

    pub fn get(&self, key: &'a str) -> Option<GarbageBox<T>> {
        self.heap.get(key).map(|r| unsafe { r.clone_uncheckable() })
    }
}
