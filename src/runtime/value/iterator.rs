use super::{Context, Exception, List, Object, Tuple, Value};

use std::cell::RefCell;
use std::rc::Rc;

use smallvec::SmallVec;

// MARK: Iterable

pub trait Iterable {
    fn next(&mut self, ctx: &mut Context) -> Result<Option<Value>, Exception>;
    fn size_hint(&self) -> Option<usize> {
        None
    }
    fn collect_roots(&self, _roots: &mut Vec<Value>) {}
}

// MARK: IterValue

#[derive(Clone)]
pub struct IterValue {
    inner: Rc<RefCell<Box<dyn Iterable>>>,
}

impl IterValue {
    pub fn new(iter: Box<dyn Iterable>) -> Self {
        Self {
            inner: Rc::new(RefCell::new(iter)),
        }
    }

    pub fn shared_iter(&self) -> SharedIter {
        SharedIter {
            inner: self.inner.clone(),
        }
    }
}

impl std::fmt::Debug for IterValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IterValue").finish()
    }
}

pub struct SharedIter {
    inner: Rc<RefCell<Box<dyn Iterable>>>,
}

impl Iterable for SharedIter {
    fn next(&mut self, ctx: &mut Context) -> Result<Option<Value>, Exception> {
        self.inner.borrow_mut().next(ctx)
    }

    fn size_hint(&self) -> Option<usize> {
        self.inner.borrow().size_hint()
    }

    fn collect_roots(&self, roots: &mut Vec<Value>) {
        self.inner.borrow().collect_roots(roots);
    }
}

// MARK: ListIterator

pub struct ListIterator {
    list: List,
    index: usize,
}

impl ListIterator {
    pub fn new(list: List) -> Self {
        Self { list, index: 0 }
    }
}

impl Iterable for ListIterator {
    fn next(&mut self, ctx: &mut Context) -> Result<Option<Value>, Exception> {
        if self.index < self.list.len() {
            let value = self.list.get(self.index).map_or(
                Err(Exception::new(
                    "IndexError",
                    format!(
                        "Index {} out of bounds for list of length {}",
                        self.index,
                        self.list.len()
                    ),
                )
                .with_backtrace(ctx.backtrace())),
                |value| Ok(Some(value)),
            )?;

            self.index += 1;
            Ok(value)
        } else {
            Ok(None)
        }
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.list.len() - self.index)
    }

    fn collect_roots(&self, roots: &mut Vec<Value>) {
        roots.push(Value::List(self.list.clone()));
    }
}

// MARK: TupleIterator

pub struct TupleIterator {
    tuple: Tuple,
    index: usize,
}

impl TupleIterator {
    pub fn new(tuple: Tuple) -> Self {
        Self { tuple, index: 0 }
    }
}

impl Iterable for TupleIterator {
    fn next(&mut self, ctx: &mut Context) -> Result<Option<Value>, Exception> {
        if self.index < self.tuple.len() {
            let value = self.tuple.get(self.index).map_or(
                Err(Exception::new(
                    "IndexError",
                    format!(
                        "Index {} out of bounds for tuple of length {}",
                        self.index,
                        self.tuple.len()
                    ),
                )
                .with_backtrace(ctx.backtrace())),
                |value| Ok(Some((**value).clone())),
            )?;

            self.index += 1;
            Ok(value)
        } else {
            Ok(None)
        }
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.tuple.len() - self.index)
    }

    fn collect_roots(&self, roots: &mut Vec<Value>) {
        roots.push(Value::Tuple(self.tuple.clone()));
    }
}

// MARK: ObjectIterator

pub struct ObjectIterator {
    object: Object,
    index: usize,
}

impl ObjectIterator {
    pub fn new(object: Object) -> Self {
        Self { object, index: 0 }
    }
}

impl Iterable for ObjectIterator {
    fn next(&mut self, ctx: &mut Context) -> Result<Option<Value>, Exception> {
        let entries = self.object.borrow();
        if self.index < entries.len() {
            let (key, value) = entries.get(self.index).ok_or_else(|| {
                Exception::new(
                    "IndexError",
                    format!(
                        "Index {} out of bounds for object of length {}",
                        self.index,
                        entries.len()
                    ),
                )
                .with_backtrace(ctx.backtrace())
            })?;

            let tuple = Value::Tuple(Tuple::new(SmallVec::from_vec(vec![
                Box::new(Value::String(key.to_string())),
                Box::new(value.clone()),
            ])));

            self.index += 1;
            Ok(Some(tuple))
        } else {
            Ok(None)
        }
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.object.borrow().len().saturating_sub(self.index))
    }

    fn collect_roots(&self, roots: &mut Vec<Value>) {
        roots.push(Value::Object(self.object.clone()));
    }
}

// MARK: StringIterator

pub struct StringIterator {
    chars: Vec<char>,
    index: usize,
}

impl StringIterator {
    pub fn new(string: String) -> Self {
        Self {
            chars: string.chars().collect(),
            index: 0,
        }
    }
}

impl Iterable for StringIterator {
    fn next(&mut self, _ctx: &mut Context) -> Result<Option<Value>, Exception> {
        if self.index < self.chars.len() {
            let ch = self.chars[self.index];
            self.index += 1;
            Ok(Some(Value::String(ch.to_string())))
        } else {
            Ok(None)
        }
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.chars.len() - self.index)
    }
}

// MARK: RangeIterator

pub struct RangeIterator {
    start: i64,
    end: i64,
    current: i64,
    step: i64,
}

impl RangeIterator {
    pub fn new(start: i64, end: i64, step: i64) -> Self {
        Self {
            start,
            end,
            current: start,
            step,
        }
    }
}

impl Iterable for RangeIterator {
    fn next(&mut self, _ctx: &mut Context) -> Result<Option<Value>, Exception> {
        if (self.step > 0 && self.current >= self.end)
            || (self.step < 0 && self.current <= self.end)
        {
            Ok(None)
        } else {
            self.current += self.step;
            Ok(Some(Value::from(self.current)))
        }
    }

    fn size_hint(&self) -> Option<usize> {
        if (self.step > 0 && self.start >= self.end) || (self.step < 0 && self.start <= self.end) {
            Some(0)
        } else {
            let distance = (self.end - self.start).abs() as usize;
            let step = self.step.abs() as usize;
            Some((distance + step - 1) / step)
        }
    }
}
