use super::{Context, Exception, List, VRef, Value};

use smallvec::SmallVec;
use ustr::Ustr;

// MARK: Iterable

pub trait Iterable {
    fn next(&mut self, ctx: &mut Context) -> Result<Option<Value>, Exception>;
    fn size_hint(&self) -> Option<usize> {
        None
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
}

// MARK: TupleIterator

pub struct TupleIterator {
    tuple: SmallVec<[Box<Value>; 3]>,
    index: usize,
}

impl TupleIterator {
    pub fn new(tuple: SmallVec<[Box<Value>; 3]>) -> Self {
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
}

// MARK: ObjectIterator

pub struct ObjectIterator {
    object: VRef<Vec<(Ustr, Value)>>,
    index: usize,
}

impl ObjectIterator {
    pub fn new(object: VRef<Vec<(Ustr, Value)>>) -> Self {
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

            let tuple = Value::Tuple(SmallVec::from_vec(vec![
                Box::new(Value::String(key.to_string())),
                Box::new(value.clone()),
            ]));

            self.index += 1;
            Ok(Some(tuple))
        } else {
            Ok(None)
        }
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.object.borrow().len().saturating_sub(self.index))
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
