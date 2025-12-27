use crate::interp;
use crate::print::PrettyString;
use crate::runtime::value::{IterValue, Iterable};
use crate::runtime::{Context, Exception, Function, Module, Tuple, Value};

use smallvec::SmallVec;
use ustr::Ustr;

// MARK: MapIter

struct MapIter {
    base: Box<dyn Iterable>,
    func: Function,
}

impl Iterable for MapIter {
    fn next(&mut self, ctx: &mut Context) -> Result<Option<Value>, Exception> {
        match self.base.next(ctx)? {
            Some(value) => {
                let mapped = interp::call_function(ctx, &self.func, vec![value])?;
                Ok(Some(mapped))
            }
            None => Ok(None),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        self.base.size_hint()
    }

    fn collect_roots(&self, roots: &mut Vec<Value>) {
        roots.push(Value::Function(self.func.clone()));
        self.base.collect_roots(roots);
    }
}

// MARK: FilterIter

struct FilterIter {
    base: Box<dyn Iterable>,
    pred: Function,
}

impl Iterable for FilterIter {
    fn next(&mut self, ctx: &mut Context) -> Result<Option<Value>, Exception> {
        loop {
            match self.base.next(ctx)? {
                Some(value) => {
                    let result = interp::call_function(ctx, &self.pred, vec![value.clone()])?;
                    if result.is_truthy() {
                        return Ok(Some(value));
                    }
                }
                None => return Ok(None),
            }
        }
    }

    fn size_hint(&self) -> Option<usize> {
        self.base.size_hint()
    }

    fn collect_roots(&self, roots: &mut Vec<Value>) {
        roots.push(Value::Function(self.pred.clone()));
        self.base.collect_roots(roots);
    }
}

// MARK: EnumerateIter

struct EnumerateIter {
    base: Box<dyn Iterable>,
    index: usize,
}

impl Iterable for EnumerateIter {
    fn next(&mut self, ctx: &mut Context) -> Result<Option<Value>, Exception> {
        match self.base.next(ctx)? {
            Some(value) => {
                let idx = Value::from(self.index);
                self.index += 1;
                Ok(Some(Value::Tuple(Tuple::new(SmallVec::from_vec(vec![
                    Box::new(idx),
                    Box::new(value),
                ])))))
            }
            None => Ok(None),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        self.base.size_hint()
    }

    fn collect_roots(&self, roots: &mut Vec<Value>) {
        self.base.collect_roots(roots);
    }
}

// MARK: register

pub(super) fn register(ctx: &mut Context) {
    ctx.get_module_mut("builtin")
        .expect("builtin module should exist")
        .with_function(builtin_fn_v2!("len", |&ctx, v: any| {
            let len = match v {
                Value::String(s) => s.chars().count(),
                Value::List(l) => l.len(),
                Value::Tuple(t) => t.len(),
                Value::Object(o) => o.borrow().len(),
                _ => {
                    return Err(Exception::new(
                        "TypeError",
                        format!("cannot get length of type: {}", v.ty().plain_string(ctx)),
                    )
                    .with_backtrace(ctx.backtrace()))
                }
            };
            Ok(Value::from(len))
        }))
        .with_function(builtin_fn_v2!("reverse", |&ctx, v: any| {
            let result = match v {
                Value::List(list) => {
                    Value::list(list.borrow_slice().iter().cloned().rev().collect())
                }
                Value::Tuple(tuple) => {
                    let mut items: Vec<Value> = tuple.iter().map(|v| (**v).clone()).collect();
                    items.reverse();
                    Value::Tuple(Tuple::new(SmallVec::from_vec(
                        items.into_iter().map(Box::new).collect(),
                    )))
                }
                Value::String(s) => Value::String(s.chars().rev().collect()),
                other => {
                    return Err(Exception::new(
                        "TypeError",
                        format!(
                            "reverse expects list, tuple, or string, found {}",
                            other.ty().plain_string(ctx)
                        ),
                    )
                    .with_backtrace(ctx.backtrace()))
                }
            };
            Ok(result)
        }))
        .with_function(builtin_fn_v2!("delete", |&ctx, obj: any, key: str| {
            match obj {
                Value::Object(map) => {
                    let key_ustr = Ustr::from(&key);
                    let mut fields = map.borrow_mut();
                    if let Some(pos) = fields.iter().position(|(k, _)| *k == key_ustr) {
                        fields.remove(pos);
                    }
                    Ok(Value::Object(crate::runtime::Object::new(fields.clone())))
                }
                other => Err(Exception::new(
                    "TypeError",
                    format!(
                        "delete expects object, found {}",
                        other.ty().plain_string(ctx)
                    ),
                )
                .with_backtrace(ctx.backtrace())),
            }
        }))
        .with_function(builtin_fn_v2!("append", |&ctx, list: any, item: any| {
            match list {
                Value::List(v) => {
                    let mut v = v;
                    v.push(item);
                    Ok(Value::List(v))
                }
                other => Err(Exception::new(
                    "TypeError",
                    format!(
                        "append expects list, found {}",
                        other.ty().plain_string(ctx)
                    ),
                )
                .with_backtrace(ctx.backtrace())),
            }
        }));

    ctx.get_module_mut("builtin")
        .expect("builtin module should exist")
        .with_function(builtin_fn_v2!("map", |&ctx, xs: iter, f: fn| {
            let base = xs.try_into_iter(ctx)?;
            Ok(Value::Iter(IterValue::new(Box::new(MapIter {
                base,
                func: f,
            }))))
        }))
        .with_function(builtin_fn_v2!(
            "filter",
            |&ctx, xs: iter, pred: fn| {
                let base = xs.try_into_iter(ctx)?;
                Ok(Value::Iter(IterValue::new(Box::new(FilterIter {
                    base,
                    pred,
                }))))
            }
        ))
        .with_function(builtin_fn_v2!("enumerate", |&ctx, xs: iter| {
            let base = xs.try_into_iter(ctx)?;
            Ok(Value::Iter(IterValue::new(Box::new(EnumerateIter {
                base,
                index: 0,
            }))))
        }))
        .with_function(builtin_fn_v2!("next", |&ctx, xs: iter| {
            let mut iter = xs.try_into_iter(ctx)?;
            match iter.next(ctx)? {
                Some(value) => Ok(value),
                None => Ok(Value::Empty),
            }
        }))
        .with_function(builtin_fn_v2!(
            "reduce",
            |&ctx, xs: iter, f: fn, init: any?| {
                let mut iter = xs.try_into_iter(ctx)?;
                let mut acc = match init {
                    Some(value) => value,
                    None => match iter.next(ctx)? {
                        Some(value) => value,
                        None => return Ok(Value::Empty),
                    },
                };
                while let Some(value) = iter.next(ctx)? {
                    acc = interp::call_function(ctx, &f, vec![acc, value])?;
                }
                Ok(acc)
            }
        ));
}
