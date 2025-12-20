use crate::interp::VRef;
use crate::print::PrettyString;
use crate::runtime::{Context, Exception, Module, Value};
use smallvec::SmallVec;
use ustr::Ustr;

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
