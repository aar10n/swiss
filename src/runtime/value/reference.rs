use super::{Context, Exception, Ty, Value};
use crate::print::{PrettyPrint, PrettyString};

use smallvec::SmallVec;
use std::cell::{self, RefCell};
use std::rc::Rc;

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
        match &*inner_ref {
            Value::Tuple(t) => Ok(t.clone()),
            _ => Err(Exception::new(
                "TypeError",
                format!("expected tuple, got {}", self.ty().pretty_string(ctx)),
            )),
        }
    }

    pub fn try_into_list(self, ctx: &Context) -> Result<VRef<Vec<Value>>, Exception> {
        match self.inner.replace(Value::Empty) {
            Value::Ref(r) => {
                let value = r.into_value();
                match value {
                    Value::List(l) => Ok(l),
                    _ => Err(Exception::new(
                        "TypeError",
                        format!("expected list, got {}", self.ty().pretty_string(ctx)),
                    )),
                }
            }
            Value::List(l) => Ok(l.clone()),
            _ => Err(Exception::new(
                "TypeError",
                format!("expected list, got {}", self.ty().pretty_string(ctx)),
            )),
        }
    }
}
