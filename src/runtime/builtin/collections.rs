use crate::interp::VRef;
use crate::print::PrettyString;
use crate::runtime::{Context, Exception, Module, Value};
use smallvec::SmallVec;
use ustr::Ustr;

pub(super) fn register(module: &mut Module) {
    module
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
                Value::List(list) => Value::list(list.borrow_slice().iter().cloned().rev().collect()),
                Value::Tuple(tuple) => {
                    let mut items: Vec<Value> = tuple.iter().map(|v| (**v).clone()).collect();
                    items.reverse();
                    Value::Tuple(SmallVec::from_vec(
                        items.into_iter().map(Box::new).collect(),
                    ))
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
        .with_function(builtin_fn_v2!("index", |&ctx, container: any, idx: any| {
            let idx_value = match idx {
                Value::Ref(r) => r.borrow().clone(),
                other => other,
            };

            let as_usize = |value: Value| -> Result<usize, Exception> {
                match value {
                    Value::Quantity(q) if q.is_dimless() => q
                        .number
                        .into_int(ctx)
                        .map_err(|e| e.with_backtrace(ctx.backtrace()))?
                        .to_usize()
                        .ok_or_else(|| {
                            Exception::new("IndexError", "index must be non-negative".to_string())
                                .with_backtrace(ctx.backtrace())
                        }),
                    other => Err(Exception::new(
                        "TypeError",
                        format!(
                            "index must be an integer, found {}",
                            other.ty().plain_string(ctx)
                        ),
                    )
                    .with_backtrace(ctx.backtrace())),
                }
            };

            let value = match container {
                Value::List(list) => {
                    let idx = as_usize(idx_value)?;
                    list.get(idx).ok_or_else(|| {
                        Exception::new("IndexError", format!("list index out of range: {}", idx))
                            .with_backtrace(ctx.backtrace())
                    })?
                }
                Value::Tuple(tuple) => {
                    let idx = as_usize(idx_value)?;
                    *tuple.get(idx).cloned().ok_or_else(|| {
                        Exception::new("IndexError", format!("tuple index out of range: {}", idx))
                            .with_backtrace(ctx.backtrace())
                    })?
                }
                Value::Object(object) => {
                    let key = match idx_value {
                        Value::String(s) => s,
                        Value::Ref(r) => match r.borrow().clone() {
                            Value::String(s) => s,
                            other => {
                                return Err(Exception::new(
                                    "TypeError",
                                    format!(
                                        "object indices must be strings, found {}",
                                        other.ty().plain_string(ctx)
                                    ),
                                )
                                .with_backtrace(ctx.backtrace()))
                            }
                        },
                        other => {
                            return Err(Exception::new(
                                "TypeError",
                                format!(
                                    "object indices must be strings, found {}",
                                    other.ty().plain_string(ctx)
                                ),
                            )
                            .with_backtrace(ctx.backtrace()))
                        }
                    };

                    let key_ustr = Ustr::from(&key);
                    object
                        .borrow()
                        .iter()
                        .find(|(k, _)| *k == key_ustr)
                        .map(|(_, v)| v.clone())
                        .ok_or_else(|| {
                            Exception::new("KeyError", format!("object key not found: \"{}\"", key))
                                .with_backtrace(ctx.backtrace())
                        })?
                }
                other => {
                    return Err(Exception::new(
                        "TypeError",
                        format!("cannot index into type: {}", other.ty().plain_string(ctx)),
                    )
                    .with_backtrace(ctx.backtrace()))
                }
            };

            Ok(value)
        }))
        .with_function(builtin_fn_v2!("delete", |&ctx, obj: any, key: str| {
            match obj {
                Value::Object(map) => {
                    let key_ustr = Ustr::from(&key);
                    let mut fields = map.borrow_mut();
                    if let Some(pos) = fields.iter().position(|(k, _)| *k == key_ustr) {
                        fields.remove(pos);
                    }
                    Ok(Value::Object(VRef::new(fields.clone())))
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
}
