use crate::runtime::{Context, Exception};

use crate::runtime::Function;
use std::any::Any;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;
use ustr::Ustr;

/// An opaque, ref-counted handle to a native object with a dynamic type tag.
/// The payload is erased (`dyn Any`) and shared via `Rc<RefCell<...>>`.
/// Equality is pointer identity + matching tag; cloning clones the handle, not the payload.
#[derive(Clone, Debug)]
pub struct Handle {
    tag: Ustr,
    inner: Rc<RefCell<dyn Any>>,
}

impl Handle {
    pub fn new<T: 'static>(tag: Ustr, value: T) -> Self {
        Self {
            tag,
            inner: Rc::new(RefCell::new(value)),
        }
    }

    pub fn tag(&self) -> Ustr {
        self.tag
    }

    pub fn ptr(&self) -> usize {
        Rc::as_ptr(&self.inner) as *const () as usize
    }

    pub fn borrow<T: 'static>(
        &self,
        expected_tag: Ustr,
        ctx: &Context,
    ) -> Result<Ref<'_, T>, Exception> {
        if self.tag != expected_tag {
            return Err(Exception::new(
                "TypeError",
                format!(
                    "expected handle of type '{}', found '{}'",
                    expected_tag, self.tag
                ),
            )
            .with_backtrace(ctx.backtrace()));
        }

        Ref::filter_map(self.inner.borrow(), |any| any.downcast_ref::<T>()).map_err(|_| {
            Exception::new(
                "TypeError",
                format!(
                    "handle type '{}' has unexpected payload (internal type mismatch)",
                    expected_tag
                ),
            )
            .with_backtrace(ctx.backtrace())
        })
    }

    pub fn borrow_mut<T: 'static>(
        &self,
        expected_tag: Ustr,
        ctx: &Context,
    ) -> Result<RefMut<'_, T>, Exception> {
        if self.tag != expected_tag {
            return Err(Exception::new(
                "TypeError",
                format!(
                    "expected handle of type '{}', found '{}'",
                    expected_tag, self.tag
                ),
            )
            .with_backtrace(ctx.backtrace()));
        }

        RefMut::filter_map(self.inner.borrow_mut(), |any| any.downcast_mut::<T>()).map_err(|_| {
            Exception::new(
                "TypeError",
                format!(
                    "handle type '{}' has unexpected payload (internal type mismatch)",
                    expected_tag
                ),
            )
            .with_backtrace(ctx.backtrace())
        })
    }
}

impl PartialEq for Handle {
    fn eq(&self, other: &Self) -> bool {
        self.tag == other.tag && Rc::ptr_eq(&self.inner, &other.inner)
    }
}

impl Eq for Handle {}

// MARK: HandleMethodRegistry

#[derive(Default)]
pub struct HandleMethodRegistry {
    methods: HashMap<Ustr, HashMap<Ustr, Function>>,
}

impl HandleMethodRegistry {
    pub fn new() -> Self {
        Self {
            methods: HashMap::new(),
        }
    }

    pub fn register(&mut self, tag: impl Into<Ustr>, name: impl Into<Ustr>, func: Function) {
        self.methods
            .entry(tag.into())
            .or_default()
            .insert(name.into(), func);
    }

    pub fn get(&self, tag: Ustr, name: Ustr) -> Option<Function> {
        self.methods.get(&tag).and_then(|m| m.get(&name).cloned())
    }
}
