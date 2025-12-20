use super::{iterator::Iterable, Context, Exception, Ty, Value};
use crate::print::{PrettyPrint, PrettyString};

use smallvec::SmallVec;
use std::cell::{self, RefCell};
use std::rc::{Rc, Weak};

// MARK: VRef

#[derive(Clone, Debug)]
pub struct VRef<T: Clone> {
    inner: Rc<RefCell<T>>,
    is_const: bool,
}

impl<T: Clone> VRef<T> {
    pub fn new(value: T) -> Self {
        VRef {
            inner: Rc::new(RefCell::new(value)),
            is_const: false,
        }
    }

    pub fn new_const(value: T) -> Self {
        VRef {
            inner: Rc::new(RefCell::new(value)),
            is_const: true,
        }
    }

    pub fn get(&self) -> T {
        self.inner.borrow().clone()
    }

    pub fn set(&self, value: T) {
        if self.is_const {
            panic!("attempted to set a const ref");
        }
        *self.inner.borrow_mut() = value;
    }

    pub fn borrow(&self) -> cell::Ref<T> {
        self.inner.borrow()
    }

    pub fn borrow_mut(&self) -> cell::RefMut<T> {
        if self.is_const {
            panic!("attempted to modify a const ref");
        }
        self.inner.borrow_mut()
    }

    pub fn strong_count(&self) -> usize {
        Rc::strong_count(&self.inner)
    }

    pub fn ptr(&self) -> usize {
        Rc::as_ptr(&self.inner) as usize
    }

    pub fn downgrade(&self) -> Weak<RefCell<T>> {
        Rc::downgrade(&self.inner)
    }
}

// MARK: ValueRef

#[derive(Clone, Debug)]
pub struct ValueRef {
    inner: Rc<RefCell<Value>>,
    is_const: bool,
}

impl ValueRef {
    pub fn new(value: Value) -> Self {
        ValueRef {
            inner: Rc::new(RefCell::new(value)),
            is_const: false,
        }
    }

    pub fn new_const(value: Value) -> Self {
        ValueRef {
            inner: Rc::new(RefCell::new(value)),
            is_const: true,
        }
    }

    pub fn ty(&self) -> Ty {
        Value::ty(&self.inner.borrow())
    }

    pub fn is_mut(&self) -> bool {
        !self.is_const
    }

    pub fn is_const(&self) -> bool {
        self.is_const
    }

    pub fn get(&self) -> Value {
        (*self.inner.borrow()).clone()
    }

    pub fn set(&self, value: Value) {
        if !self.is_const {
            *self.inner.borrow_mut() = value;
        } else {
            panic!("attempted to modify a const value");
        }
    }

    pub fn borrow(&self) -> cell::Ref<Value> {
        self.inner.borrow()
    }

    pub fn borrow_mut(&self) -> cell::RefMut<Value> {
        if !self.is_const {
            self.inner.borrow_mut()
        } else {
            panic!("attempted to modify a const value");
        }
    }

    pub fn unwrap_value(self) -> Value {
        if self.is_const {
            panic!("attempted to unwrap a const value");
        }
        Rc::try_unwrap(self.inner).unwrap().into_inner()
    }

    pub fn into_value(self) -> Value {
        Value::Ref(self)
    }

    pub fn try_into_tuple(self, ctx: &Context) -> Result<SmallVec<[Box<Value>; 3]>, Exception> {
        let inner_ref = self.borrow();
        inner_ref.clone().try_into_tuple(ctx)
    }

    pub fn try_into_list(self, ctx: &Context) -> Result<super::List, Exception> {
        let inner_ref = self.borrow();
        inner_ref.clone().try_into_list(ctx)
    }

    pub fn try_into_iter(self, ctx: &Context) -> Result<Box<dyn Iterable>, Exception> {
        let inner_ref = self.borrow();
        inner_ref.clone().try_into_iter(ctx)
    }
}
