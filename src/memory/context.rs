use std::{
    cell::RefCell,
    collections::HashMap,
    rc::{Rc, Weak},
};

use super::{gc::GarbageCollector, ptr::GarbageBox};

pub struct Context<'a, T: Eq> {
    parent: Weak<RefCell<Context<'a, T>>>,
    heap: HashMap<&'a str, GarbageBox<T>>,
    garbage: Rc<RefCell<GarbageCollector<T>>>,
}

impl<'a, T: Eq> Context<'a, T> {
    pub fn new(garbage: Rc<RefCell<GarbageCollector<T>>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent: Weak::new(),
            heap: HashMap::new(),
            garbage,
        }))
    }

    pub fn with_parent(
        parent: Rc<RefCell<Context<'a, T>>>,
        garbage: Rc<RefCell<GarbageCollector<T>>>,
    ) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent: Rc::downgrade(&parent),
            heap: HashMap::new(),
            garbage,
        }))
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
            {
                let mut garbage = self.garbage.borrow_mut();
                garbage.push(value_ref);
            }
        }

        inner
    }

    pub fn delete(&mut self, key: &'a str) {
        let gbox = self.get(key).unwrap();
        self.heap.remove(key);
        {
            let mut garbage = self.garbage.borrow_mut();
            garbage.try_pop(gbox);
        }
    }

    pub fn free(&mut self, key: &'a str) {
        let gbox = self.get(key).unwrap();
        self.heap.remove(key);
        {
            let mut garbage = self.garbage.borrow_mut();
            garbage.pop(gbox);
        }
    }

    pub fn get(&self, key: &'a str) -> Option<GarbageBox<T>> {
        let value = self.heap.get(key).map(|r| r.clone());
        match (value, self.parent.upgrade()) {
            (Some(value), _) => Some(value),
            (None, Some(parent)) => {
                let parent = parent.borrow();
                parent.get(key)
            }
            (None, None) => None,
        }
    }
}

impl<'a, T: Eq> Drop for Context<'a, T> {
    fn drop(&mut self) {
        let mut garbage = self.garbage.borrow_mut();
        for (_, gbox) in &self.heap {
            garbage.try_pop(unsafe { gbox.clone_uncheckable() });
        }
    }
}
