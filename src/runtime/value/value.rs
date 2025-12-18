use super::super::{Context, Conversion, Exception, Function, IoHandle};
use super::{Dim, Number, Quantity, Ty};
pub use super::{VRef, ValueRef};

pub use crate::id::VarId;
use crate::print::ansi::{NUMBER, RESET};
use crate::print::{EvalPrint, PrettyPrint, PrettyString};

use smallvec::SmallVec;
use std::cell::RefCell;
use std::rc::Rc;
use ustr::Ustr;

pub enum LRValue {
    L(ValueRef),
    R(Value),
}

impl PrettyPrint<Context> for LRValue {
    fn pretty_print<Output: std::io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> std::io::Result<()> {
        match self {
            LRValue::L(v) => {
                write!(out, "ref<")?;
                v.borrow().pretty_print(out, ctx, level)?;
                write!(out, ">")
            }
            LRValue::R(v) => v.pretty_print(out, ctx, level),
        }
    }
}

// MARK: Value

#[derive(Clone, Debug)]
pub enum Value {
    Ref(ValueRef),
    List(VRef<Vec<Value>>),
    Object(VRef<Vec<(Ustr, Value)>>),
    Tuple(SmallVec<[Box<Value>; 3]>),
    Quantity(Quantity),
    String(String),
    Boolean(bool),
    Function(Function),
    Io(IoHandle),
    Unit(Ustr),
    Ty(Ty),
    Empty,
}

impl Value {
    pub fn list(values: Vec<Value>) -> Self {
        Value::List(VRef::new(values))
    }

    pub fn object(values: Vec<(Ustr, Value)>) -> Self {
        Value::Object(VRef::new(values))
    }

    pub fn is_ref(&self) -> bool {
        matches!(self, Value::Ref(_))
    }

    pub fn is_quantity(&self) -> bool {
        matches!(self, Value::Quantity(_))
    }

    pub fn is_zero(&self) -> bool {
        match &self {
            Value::Ref(r) => r.borrow().is_zero(),
            Value::List(l) => l.borrow().is_empty(),
            Value::Object(o) => o.borrow().is_empty(),
            Value::Tuple(t) => t.is_empty(),
            Value::Quantity(q) => q.is_zero(),
            Value::String(s) => s.is_empty(),
            Value::Boolean(b) => !b,
            Value::Function(_) => false,
            Value::Io(_) => false,
            Value::Unit(_) => false,
            Value::Ty(_) => false,
            Value::Empty => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match &self {
            Value::Ref(r) => r.borrow().is_float(),
            Value::Quantity(q) => q.is_float(),
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match &self {
            Value::Ref(r) => r.borrow().is_int(),
            Value::Quantity(q) => q.is_int(),
            _ => false,
        }
    }

    pub fn ty(&self) -> Ty {
        match &self {
            Value::Ref(r) => r.borrow().ty(),
            Value::List(_) => Ty::List,
            Value::Object(_) => Ty::Object,
            Value::Tuple(t) => Ty::Tuple(t.iter().map(|v| Box::new(v.ty())).collect()),
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
            Value::Function(_) => Ty::Function,
            Value::Io(_) => Ty::Io,
            Value::Unit(_) => Ty::Unit,
            Value::Ty(_) => Ty::Type,
            Value::Empty => Ty::Empty,
        }
    }

    pub fn into_ref(self) -> ValueRef {
        match self {
            Value::Ref(r) => r,
            _ => ValueRef::new(self),
        }
    }

    pub fn try_into_tuple(self, ctx: &Context) -> Result<SmallVec<[Box<Value>; 3]>, Exception> {
        match self {
            Value::Tuple(t) => Ok(t),
            _ => Err(Exception::new(
                "TypeError",
                format!("expected tuple, got {}", self.ty().pretty_string(ctx)),
            )),
        }
    }

    pub fn try_into_list(self, ctx: &Context) -> Result<VRef<Vec<Value>>, Exception> {
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

impl From<ValueRef> for Value {
    fn from(value: ValueRef) -> Self {
        Value::Ref(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::String(value)
    }
}

impl From<usize> for Value {
    fn from(value: usize) -> Self {
        Value::Quantity(Quantity::from(Number::from(value)))
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Boolean(value)
    }
}

impl From<()> for Value {
    fn from(value: ()) -> Self {
        Value::Empty
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
            Value::Ref(r) => {
                write!(out, "&")?;
                r.borrow().pretty_print(out, ctx, level)
            }
            Value::Object(o) => {
                write!(out, "{{")?;
                for (i, (key, value)) in o.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ")?;
                    }
                    write!(out, "\"{}\": ", key)?;
                    value.pretty_print(out, ctx, level)?;
                }
                write!(out, "}}")
            }
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
            Value::Function(f) => write!(out, "<fn {}>", f.name.raw),
            Value::Io(_) => write!(out, "<io>"),
            Value::Unit(u) => {
                // Prefer registered unit name; fall back to raw identifier.
                let name = ctx
                    .active_module()
                    .and_then(|m| m.units.get(*u))
                    .map(|unit| unit.name.raw.to_string())
                    .unwrap_or_else(|| u.to_string());
                write!(out, "{}", name)
            }
            Value::Ty(t) => write!(out, "{:?}", t),
            Value::Empty => write!(out, "()"),
        }
    }
}

impl EvalPrint<Context> for Value {
    fn display_print<Output: std::io::Write>(
        &self,
        out: &mut Output,
        ctx: &mut Context,
        level: usize,
    ) -> std::io::Result<()> {
        match &self {
            Value::Ref(r) => {
                write!(out, "&")?;
                r.borrow().display_print(out, ctx, level)
            }
            Value::Object(o) => {
                write!(out, "{{")?;
                for (i, (key, value)) in o.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ")?;
                    }
                    write!(out, "\"{}\": ", key)?;
                    value.display_print(out, ctx, level)?;
                }
                write!(out, "}}")
            }
            Value::Tuple(t) => {
                write!(out, "(")?;
                for (i, v) in t.iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ")?;
                    }
                    v.display_print(out, ctx, level)?;
                }
                write!(out, ")")
            }
            Value::List(l) => {
                write!(out, "[")?;
                for (i, v) in l.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ")?;
                    }
                    v.display_print(out, ctx, level)?;
                }
                write!(out, "]")
            }
            Value::Quantity(q) => q.display_print(out, ctx, level),
            Value::String(s) => write!(out, "{:?}", s),
            Value::Boolean(b) => write!(out, "{}", b),
            Value::Function(f) => write!(out, "<fn {}>", f.name.raw),
            Value::Io(_) => write!(out, "<io>"),
            Value::Unit(u) => {
                // Prefer display_name from unit impl if available.
                let name = if let Some(module) = ctx.active_module() {
                    if let Some(unit) = module.units.get(*u) {
                        let unit_name = unit.name.raw;
                        let conversion = unit.conversion.clone();
                        match conversion {
                            Conversion::Impl(unit_impl) => unit_impl
                                .display_name(ctx)
                                .ok()
                                .flatten()
                                .unwrap_or_else(|| unit_name.to_string()),
                            _ => unit_name.to_string(),
                        }
                    } else {
                        u.to_string()
                    }
                } else {
                    u.to_string()
                };
                write!(out, "{}", name)
            }
            Value::Ty(t) => write!(out, "{:?}", t),
            Value::Empty => write!(out, "()"),
        }
    }
}
