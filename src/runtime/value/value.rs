use super::super::{Context, Exception};
use super::{Dim, Number, Quantity, Ty};

use crate::print::ansi::{NUMBER, RESET};
use crate::print::{PrettyPrint, PrettyString};

use smallvec::SmallVec;
use std::cell::RefCell;
use std::rc::Rc;

pub type VRef<T> = Rc<RefCell<T>>;

// MARK: Value

#[derive(Clone, Debug)]
pub enum Value {
    Tuple(SmallVec<[Box<Value>; 3]>),
    List(VRef<Vec<Value>>),
    Quantity(Quantity),
    String(String),
    Boolean(bool),
    Empty,
}

impl Value {
    pub fn is_quantity(&self) -> bool {
        matches!(self, Value::Quantity(_))
    }

    pub fn is_zero(&self) -> bool {
        match &self {
            Value::Tuple(t) => t.is_empty(),
            Value::List(l) => l.borrow().is_empty(),
            Value::Quantity(q) => q.is_zero(),
            Value::String(s) => s.is_empty(),
            Value::Boolean(b) => !b,
            Value::Empty => false,
        }
    }

    pub fn is_truthy(&self) -> bool {
        match &self {
            Value::Tuple(t) => {
                if t.is_empty() {
                    false
                } else {
                    t.iter().all(|v| v.is_truthy())
                }
            }
            Value::List(l) => {
                if l.borrow().is_empty() {
                    false
                } else {
                    l.borrow().iter().all(|v| v.is_truthy())
                }
            }
            Value::Quantity(q) => q.is_truthy(),
            Value::String(s) => !s.is_empty(),
            Value::Boolean(b) => *b,
            Value::Empty => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match &self {
            Value::Quantity(q) => q.is_float(),
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match &self {
            Value::Quantity(q) => q.is_int(),
            _ => false,
        }
    }

    pub fn ty(&self) -> Ty {
        match &self {
            Value::Tuple(t) => Ty::Tuple(t.iter().map(|v| Box::new(v.ty())).collect()),
            Value::List(_) => Ty::List,
            Value::Quantity(q) => {
                if !q.dim.is_none() {
                    Ty::Dim(q.dim.clone())
                } else if q.number.is_int() {
                    Ty::Int
                } else {
                    Ty::Float
                }
            }
            Value::String(_) => Ty::Str,
            Value::Boolean(_) => Ty::Bool,
            Value::Empty => Ty::Empty,
        }
    }

    pub fn into_tuple(self, ctx: &Context) -> Result<SmallVec<[Box<Value>; 3]>, Exception> {
        match self {
            Value::Tuple(t) => Ok(t),
            _ => Err(Exception::new(
                "TypeError",
                format!("expected tuple, got {}", self.ty().pretty_string(ctx)),
            )),
        }
    }

    pub fn into_list(self, ctx: &Context) -> Result<VRef<Vec<Value>>, Exception> {
        match self {
            Value::List(l) => Ok(l),
            _ => Err(Exception::new(
                "TypeError",
                format!("expected list, got {}", self.ty().pretty_string(ctx)),
            )),
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Value::Empty
    }
}

impl<T: Into<Quantity>> From<T> for Value {
    fn from(value: T) -> Self {
        Value::Quantity(value.into())
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::String(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Boolean(value)
    }
}

impl PrettyPrint<Context> for Value {
    fn pretty_print<Output: std::io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> std::io::Result<()> {
        match &self {
            Value::Tuple(t) => {
                write!(out, "(")?;
                for (i, v) in t.iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ")?;
                    }
                    v.pretty_print(out, ctx, level)?;
                }
                write!(out, ")")
            }
            Value::List(l) => {
                write!(out, "[")?;
                for (i, v) in l.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ")?;
                    }
                    v.pretty_print(out, ctx, level)?;
                }
                write!(out, "]")
            }
            Value::Quantity(q) => q.pretty_print(out, ctx, level),
            Value::String(s) => write!(out, "{:?}", s),
            Value::Boolean(b) => write!(out, "{}", b),
            Value::Empty => write!(out, "()"),
        }
    }
}
