use super::super::{
    collector::{register_list, register_object},
    Context, Conversion, Exception, Function,
};
use super::iterator::{Iterable, ListIterator, ObjectIterator, StringIterator, TupleIterator};
use super::{Dim, Handle, Number, Quantity, Ty};
pub use super::{VRef, ValueRef};

pub use crate::id::VarId;
use crate::print::ansi::{NUMBER, RESET};
use crate::print::{EvalPrint, PrettyPrint, PrettyString};

use smallvec::SmallVec;
use std::cell::RefCell;
use std::collections::HashSet;
use ustr::Ustr;

thread_local! {
    static PRINT_GUARD: RefCell<HashSet<(usize, u8)>> = RefCell::new(HashSet::new());
}

struct PrintGuard {
    key: (usize, u8),
    inserted: bool,
}

impl PrintGuard {
    fn new(key: (usize, u8)) -> Self {
        let inserted = PRINT_GUARD.with(|guard| {
            let mut guard = guard.borrow_mut();
            if guard.contains(&key) {
                false
            } else {
                guard.insert(key);
                true
            }
        });
        Self { key, inserted }
    }

    fn is_cycle(&self) -> bool {
        !self.inserted
    }
}

impl Drop for PrintGuard {
    fn drop(&mut self) {
        if self.inserted {
            PRINT_GUARD.with(|guard| {
                guard.borrow_mut().remove(&self.key);
            });
        }
    }
}

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

// MARK: List

#[derive(Clone, Debug)]
pub struct List {
    buf: VRef<Vec<Value>>,
    start: usize,
    len: usize,
    uses_full_len: bool,
}

impl List {
    pub fn new(values: Vec<Value>) -> Self {
        let len = values.len();
        let buf = VRef::new(values);
        register_list(&buf);
        List {
            buf,
            start: 0,
            len,
            uses_full_len: true,
        }
    }

    pub fn len(&self) -> usize {
        if self.uses_full_len {
            self.buf.borrow().len()
        } else {
            self.len
        }
    }

    pub fn buf_ptr(&self) -> usize {
        self.buf.ptr()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn borrow_slice(&self) -> std::cell::Ref<[Value]> {
        let start = self.start;
        let uses_full = self.uses_full_len;
        let view_len = self.len;
        std::cell::Ref::map(self.buf.borrow(), move |data| {
            let end = if uses_full {
                data.len()
            } else {
                start + view_len
            };
            &data[start..end]
        })
    }

    pub fn get(&self, idx: usize) -> Option<Value> {
        let len = self.len();
        (idx < len).then(|| self.buf.borrow()[self.start + idx].clone())
    }

    pub fn slice(&self, start: usize, stop: usize) -> Self {
        debug_assert!(start <= stop && stop <= self.len());
        List {
            buf: self.buf.clone(),
            start: self.start + start,
            len: stop - start,
            uses_full_len: false,
        }
    }

    fn ensure_unique(&mut self) {
        let data_len = self.buf.borrow().len();
        let view_len = if self.uses_full_len {
            data_len
        } else {
            self.len
        };
        let covering_all = self.start == 0 && view_len == data_len;
        if !covering_all {
            let slice = self.borrow_slice().to_vec();
            let buf = VRef::new(slice);
            register_list(&buf);
            self.buf = buf;
            self.start = 0;
            self.len = self.buf.borrow().len();
            self.uses_full_len = true;
        } else {
            // Keep view aligned to the full buffer.
            self.len = data_len;
            self.uses_full_len = true;
        }
    }

    pub fn push(&mut self, value: Value) {
        self.ensure_unique();
        self.buf.borrow_mut().push(value);
        if !self.uses_full_len {
            self.len += 1;
        } else {
            self.len = self.buf.borrow().len();
        }
    }

    pub fn set(&mut self, idx: usize, value: Value) -> Option<()> {
        let len = self.len();
        if idx >= len {
            return None;
        }
        self.ensure_unique();
        let abs_idx = self.start + idx;
        self.buf.borrow_mut()[abs_idx] = value;
        Some(())
    }

    pub fn to_vec(&self) -> Vec<Value> {
        self.borrow_slice().to_vec()
    }
}

// MARK: Value

#[derive(Clone, Debug)]
pub enum Value {
    Ref(ValueRef),
    List(List),
    Object(VRef<Vec<(Ustr, Value)>>),
    Tuple(SmallVec<[Box<Value>; 3]>),
    Quantity(Quantity),
    String(String),
    Boolean(bool),
    Function(Function),
    Handle(Handle),
    Unit(Ustr),
    Ty(Ty),
    Empty,
}

impl Value {
    pub fn list(values: Vec<Value>) -> Self {
        Value::List(List::new(values))
    }

    pub fn object(values: Vec<(Ustr, Value)>) -> Self {
        let buf = VRef::new(values);
        register_object(&buf);
        Value::Object(buf)
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
            Value::List(l) => l.is_empty(),
            Value::Object(o) => o.borrow().is_empty(),
            Value::Tuple(t) => t.is_empty(),
            Value::Quantity(q) => q.is_zero(),
            Value::String(s) => s.is_empty(),
            Value::Boolean(b) => !b,
            Value::Function(_) => false,
            Value::Handle(_) => false,
            Value::Unit(_) => false,
            Value::Ty(_) => false,
            Value::Empty => true,
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
            Value::Handle(h) => Ty::Handle(h.tag()),
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
            )
            .with_backtrace(ctx.backtrace())),
        }
    }

    pub fn try_into_list(self, ctx: &Context) -> Result<List, Exception> {
        match self {
            Value::List(l) => Ok(l),
            _ => Err(Exception::new(
                "TypeError",
                format!("expected list, got {}", self.ty().pretty_string(ctx)),
            )
            .with_backtrace(ctx.backtrace())),
        }
    }

    pub fn try_into_iter(self, ctx: &Context) -> Result<Box<dyn Iterable>, Exception> {
        match self {
            Value::List(l) => Ok(Box::new(ListIterator::new(l))),
            Value::Tuple(t) => Ok(Box::new(TupleIterator::new(t))),
            Value::Object(o) => Ok(Box::new(ObjectIterator::new(o))),
            Value::String(s) => Ok(Box::new(StringIterator::new(s))),
            _ => Err(Exception::new(
                "TypeError",
                format!("expected iterable, got {}", self.ty().pretty_string(ctx)),
            )
            .with_backtrace(ctx.backtrace())),
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

impl From<Handle> for Value {
    fn from(value: Handle) -> Self {
        Value::Handle(value)
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
                let guard = PrintGuard::new((o.ptr(), 2));
                if guard.is_cycle() {
                    return write!(out, "{{...}}");
                }
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
                let guard = PrintGuard::new((l.buf_ptr(), 1));
                if guard.is_cycle() {
                    return write!(out, "[...]");
                }
                write!(out, "[")?;
                for (i, v) in l.borrow_slice().iter().enumerate() {
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
            Value::Handle(h) => write!(out, "<handle:{}>", h.tag()),
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
                let guard = PrintGuard::new((o.ptr(), 2));
                if guard.is_cycle() {
                    return write!(out, "{{...}}");
                }
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
                let guard = PrintGuard::new((l.buf_ptr(), 1));
                if guard.is_cycle() {
                    return write!(out, "[...]");
                }
                write!(out, "[")?;
                for (i, v) in l.borrow_slice().iter().enumerate() {
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
            Value::Handle(h) => write!(out, "<handle:{}>", h.tag()),
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
