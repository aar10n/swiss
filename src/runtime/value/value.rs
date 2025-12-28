use super::super::{
    collector::{register_list, register_object},
    Context, Conversion, Exception, Function,
};
use super::iterator::{
    Iterable, IterValue, ListIterator, ObjectIterator, StringIterator, TupleIterator,
};
use super::{Dim, Number, Quantity, SharedStr, Ty};
use super::super::UserTy;
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

    pub fn eq(ctx: &mut Context, lhs: &List, rhs: &List) -> Result<bool, Exception> {
        let len1 = lhs.len();
        let len2 = rhs.len();
        if len1 != len2 {
            return Ok(false);
        }
        for i in 0..len1 {
            let v1 = lhs.get(i).unwrap();
            let v2 = rhs.get(i).unwrap();
            if !Value::eq(ctx, &v1, &v2)? {
                return Ok(false);
            }
        }
        Ok(true)
    }
}

// MARK: Tuple

#[derive(Clone, Debug)]
pub struct Tuple {
    items: SmallVec<[Box<Value>; 3]>,
}

impl Tuple {
    pub fn new(items: SmallVec<[Box<Value>; 3]>) -> Self {
        Self { items }
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Box<Value>> {
        self.items.iter()
    }

    pub fn get(&self, idx: usize) -> Option<&Box<Value>> {
        self.items.get(idx)
    }

    pub fn into_items(self) -> SmallVec<[Box<Value>; 3]> {
        self.items
    }

    pub fn eq(ctx: &mut Context, lhs: &Tuple, rhs: &Tuple) -> Result<bool, Exception> {
        let len1 = lhs.len();
        let len2 = rhs.len();
        if len1 != len2 {
            return Ok(false);
        }
        for i in 0..len1 {
            let v1 = &lhs.items[i];
            let v2 = &rhs.items[i];
            if !Value::eq(ctx, v1, v2)? {
                return Ok(false);
            }
        }
        Ok(true)
    }
}

// MARK: Object

#[derive(Clone, Debug)]
pub struct Object {
    buf: VRef<Vec<(Ustr, Value)>>,
}

impl Object {
    pub fn new(values: Vec<(Ustr, Value)>) -> Self {
        let buf = VRef::new(values);
        register_object(&buf);
        Self { buf }
    }

    pub fn ptr(&self) -> usize {
        self.buf.ptr()
    }

    pub fn len(&self) -> usize {
        self.buf.borrow().len()
    }

    pub fn is_empty(&self) -> bool {
        self.buf.borrow().is_empty()
    }

    pub fn borrow(&self) -> std::cell::Ref<Vec<(Ustr, Value)>> {
        self.buf.borrow()
    }

    pub fn borrow_mut(&self) -> std::cell::RefMut<Vec<(Ustr, Value)>> {
        self.buf.borrow_mut()
    }

    pub fn eq(ctx: &mut Context, lhs: &Object, rhs: &Object) -> Result<bool, Exception> {
        let len1 = lhs.len();
        let len2 = rhs.len();
        if len1 != len2 {
            return Ok(false);
        }
        let binding = rhs.borrow();
        let map2: std::collections::HashMap<_, _> = binding.iter().map(|(k, v)| (*k, v)).collect();
        for (k1, v1) in lhs.borrow().iter() {
            match map2.get(k1) {
                Some(v2) => {
                    if !Value::eq(ctx, v1, v2)? {
                        return Ok(false);
                    }
                }
                None => return Ok(false),
            }
        }
        Ok(true)
    }
}

// MARK: Value

#[derive(Clone, Debug)]
pub enum Value {
    Ref(ValueRef),
    List(List),
    Object(Object),
    Tuple(Tuple),
    Iter(IterValue),
    Quantity(Quantity),
    String(SharedStr),
    Boolean(bool),
    Function(Function),
    UserType(UserTy),
    Unit(Ustr),
    Ty(Ty),
    Empty,
}

impl Value {
    pub fn list(values: Vec<Value>) -> Self {
        Value::List(List::new(values))
    }

    pub fn object(values: Vec<(Ustr, Value)>) -> Self {
        Value::Object(Object::new(values))
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
            Value::Object(o) => o.is_empty(),
            Value::Tuple(t) => t.is_empty(),
            Value::Iter(_) => false,
            Value::Quantity(q) => q.is_zero(),
            Value::String(s) => s.is_empty(),
            Value::Boolean(b) => !b,
            Value::Function(_) => false,
            Value::UserType(_) => false,
            Value::Unit(_) => false,
            Value::Ty(_) => false,
            Value::Empty => true,
        }
    }

    pub fn is_truthy(&self) -> bool {
        !self.is_zero()
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
            Value::Iter(_) => Ty::Iter,
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
            Value::UserType(user_ty) => Ty::UserType(user_ty.tag()),
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

    pub fn try_into_tuple(self, ctx: &Context) -> Result<Tuple, Exception> {
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
            Value::Iter(iter) => Ok(Box::new(iter.shared_iter())),
            _ => Err(Exception::new(
                "TypeError",
                format!("expected iterable, got {}", self.ty().pretty_string(ctx)),
            )
            .with_backtrace(ctx.backtrace())),
        }
    }

    pub fn eq(ctx: &mut Context, lhs: &Value, rhs: &Value) -> Result<bool, Exception> {
        match (lhs, rhs) {
            (Value::Ref(r1), Value::Ref(r2)) => Value::eq(ctx, &r1.borrow(), &r2.borrow()),
            (Value::Ref(r), v) | (v, Value::Ref(r)) => Value::eq(ctx, &r.borrow(), v),
            (Value::List(l1), Value::List(l2)) => List::eq(ctx, l1, l2),
            (Value::Tuple(t1), Value::Tuple(t2)) => Tuple::eq(ctx, t1, t2),
            (Value::Object(o1), Value::Object(o2)) => Object::eq(ctx, o1, o2),
            (Value::Quantity(q1), Value::Quantity(q2)) => {
                Quantity::safe_eq(ctx, q1.clone(), q2.clone())
            }
            (Value::String(s1), Value::String(s2)) => Ok(s1 == s2),
            (Value::Boolean(b1), Value::Boolean(b2)) => Ok(b1 == b2),
            (Value::Function(f1), Value::Function(f2)) => Ok(f1.unique_id() == f2.unique_id()),
            (Value::UserType(h1), Value::UserType(h2)) => Ok(h1.eq(h2)),
            (Value::Iter(_), Value::Iter(_)) => Ok(false),
            (Value::Unit(u1), Value::Unit(u2)) => Ok(u1 == u2),
            (Value::Ty(t1), Value::Ty(t2)) => Ok(t1 == t2),
            (Value::Empty, Value::Empty) => Ok(true),
            _ => Ok(false),
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
        Value::String(SharedStr::from(value))
    }
}

impl From<SharedStr> for Value {
    fn from(value: SharedStr) -> Self {
        Value::String(value)
    }
}

impl From<UserTy> for Value {
    fn from(value: UserTy) -> Self {
        Value::UserType(value)
    }
}

impl From<super::Handle> for Value {
    fn from(value: super::Handle) -> Self {
        Value::UserType(UserTy::Handle(value))
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
            Value::Iter(_) => write!(out, "<iter>"),
            Value::UserType(user_ty) => write!(out, "<handle:{}>", user_ty.tag()),
            Value::Unit(u) => {
                // Prefer registered unit name; fall back to raw identifier.
                let name = ctx
                    .active_module()
                    .and_then(|m| m.units.get(*u))
                    .map(|unit| unit.name.raw.to_string())
                    .unwrap_or_else(|| u.to_string());
                write!(out, "{}", name)
            }
            Value::Ty(t) => write!(out, "{}", t.pretty_string(ctx)),
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
            Value::Iter(_) => write!(out, "<iter>"),
            Value::UserType(user_ty) => write!(out, "<handle:{}>", user_ty.tag()),
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
            Value::Ty(t) => write!(out, "{}", t.to_string()),
            Value::Empty => write!(out, "()"),
        }
    }
}
