use core::{cell::Cell, marker::PhantomData, ops::Deref, ptr::NonNull};
use std::{fmt::Debug, hash::Hash, ops::DerefMut};

#[derive(Eq, PartialEq)]
struct GBoxInner<T> {
    value: T,
    refcount: Cell<usize>,
}

#[derive(Eq, PartialEq)]
pub struct GarbageBox<T: ?Eq> {
    inner: NonNull<GBoxInner<T>>,
    _marker: PhantomData<GBoxInner<T>>,
}

impl<T> GarbageBox<T> {
    pub fn new(value: T) -> GarbageBox<T> {
        let inner = Box::new(GBoxInner {
            value,
            refcount: Cell::new(1),
        });

        Self {
            // SAFETY: Box does not give us a null pointer.
            inner: unsafe { NonNull::new_unchecked(Box::into_raw(inner)) },
            _marker: PhantomData,
        }
    }

    pub fn downgrade(&self) -> WeakRef<T> {
        WeakRef::from_ptr(self.inner.as_ptr())
    }

    pub fn get(&self) -> &T {
        &unsafe { self.inner.as_ref() }.value
    }

    pub fn get_mut(&mut self) -> &mut T {
        &mut unsafe { self.inner.as_mut() }.value
    }

    pub fn strong_count(&self) -> usize {
        unsafe { self.inner.as_ref() }.refcount.get()
    }

    pub unsafe fn clone_uncheckable(&self) -> Self {
        Self {
            inner: self.inner,
            _marker: PhantomData,
        }
    }
}

impl<T> Hash for GarbageBox<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state)
    }
}

impl<T: Debug> Debug for GarbageBox<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ref<{:?}>", self.get())
    }
}

impl<T> Deref for GarbageBox<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.get()
    }
}

impl<T> DerefMut for GarbageBox<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.get_mut()
    }
}

impl<T> Clone for GarbageBox<T> {
    fn clone(&self) -> Self {
        let inner = unsafe { self.inner.as_ref() };
        let c = inner.refcount.get();
        inner.refcount.set(c + 1);

        Self {
            inner: self.inner,
            _marker: PhantomData,
        }
    }
}

impl<T> Drop for GarbageBox<T> {
    fn drop(&mut self) {
        let inner = unsafe { self.inner.as_ref() };
        let c = inner.refcount.get();
        // inner.refcount.set(c - 1);
        if c == 0 {
            drop(inner);
            // SAFETY: we are the only referance on the GarbageBox left, and we are being dropped.
            // therefore, after us, there will be no Box, and no references to T.
            let _ = unsafe { Box::from_raw(self.inner.as_ptr()) };
        } else {
            inner.refcount.set(c - 1);
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct WeakRef<T: ?Eq> {
    inner: Option<NonNull<GBoxInner<T>>>,
}

impl<T> WeakRef<T> {
    fn from_ptr(raw: *mut GBoxInner<T>) -> Self {
        let inner = unsafe { NonNull::new_unchecked(raw) };

        Self { inner: Some(inner) }
    }

    pub fn get(&self) -> Option<&T> {
        let Some(inner) = self.inner else {
            return None;
        };
        let ptr = unsafe { inner.as_ref() };

        if ptr.refcount.get() == 0 {
            return None;
        }

        Some(&ptr.value)
    }
}

impl<T> Hash for WeakRef<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state)
    }
}
impl<T> From<GarbageBox<T>> for WeakRef<T> {
    fn from(gbox: GarbageBox<T>) -> Self {
        GarbageBox::downgrade(&gbox)
    }
}
