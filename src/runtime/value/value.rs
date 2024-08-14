use super::{Dim, Exception, Number, Quantity, Ty};

use crate::print::ansi::{NUMBER, RESET};
use crate::print::{PrettyPrint, PrettyString};

use smallvec::SmallVec;
use std::cell::RefCell;
use std::rc::Rc;

pub type ValueRef = Rc<RefCell<Value>>;

impl Into<ValueRef> for Value {
    fn into(self) -> ValueRef {
        Rc::new(RefCell::new(self))
    }
}

// MARK: Value

#[derive(Clone, Debug)]
pub enum Value {
    Tuple(SmallVec<[Box<Value>; 3]>),
    List(Vec<ValueRef>),
    Quantity(Quantity),
    String(String),
    Boolean(bool),
}

impl Value {
    pub fn is_zero(&self) -> bool {
        match &self {
            Value::Tuple(t) => t.is_empty(),
            Value::List(l) => l.is_empty(),
            Value::Quantity(q) => q.is_zero(),
            Value::String(s) => s.is_empty(),
            Value::Boolean(b) => !b,
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
                if l.is_empty() {
                    false
                } else {
                    l.iter().all(|v| v.borrow().is_truthy())
                }
            }
            Value::Quantity(q) => q.is_truthy(),
            Value::String(s) => !s.is_empty(),
            Value::Boolean(b) => *b,
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
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Value::Quantity(Quantity::default())
    }
}

impl ToString for Value {
    fn to_string(&self) -> String {
        match &self {
            Value::Tuple(tys) => {
                let mut s = String::new();
                s.push_str("(");
                for (i, ty) in tys.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&ty.to_string());
                }
                s.push_str(")");
                s
            }
            Value::List(values) => {
                let mut s = String::new();
                s.push_str("[");
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&value.borrow().to_string());
                }
                s.push_str("]");
                s
            }
            Value::Quantity(q) => q.to_string(),
            Value::String(s) => s.clone(),
            Value::Boolean(b) => b.to_string(),
        }
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

impl<Ctx> PrettyPrint<Ctx> for Value {
    fn pretty_print<Output: std::io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
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
                for (i, v) in l.iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ")?;
                    }
                    v.borrow().pretty_print(out, ctx, level)?;
                }
                write!(out, "]")
            }
            Value::Quantity(q) => q.pretty_print(out, ctx, level),
            Value::String(s) => write!(out, "{:?}", s),
            Value::Boolean(b) => write!(out, "{}", b),
        }
    }
}
