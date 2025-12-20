use crate::interp;
use crate::print::PrettyString;
use crate::runtime::{Context, Exception, Module, Quantity, Value};

use ustr::Ustr;

pub(super) fn register(ctx: &mut Context) {
    ctx.get_module_mut("builtin")
        .expect("builtin module should exist")
        .with_function(builtin_fn_v2!("pos", |&ctx, x: num| Ok(x)))
        .with_function(builtin_fn_v2!("neg", |&ctx, x: num| Ok(-x)))
        .with_function(builtin_fn_v2!("add", |&ctx, x: num, y: num| {
            Quantity::safe_add(ctx, x, y)
        }))
        .with_function(builtin_fn_v2!("sub", |&ctx, x: num, y: num| {
            Quantity::safe_sub(ctx, x, y)
        }))
        .with_function(builtin_fn_v2!("mul", |&ctx, x: num, y: num| {
            Quantity::safe_mul(ctx, x, y)
        }))
        .with_function(builtin_fn_v2!("div", |&ctx, x: num, y: num| {
            Quantity::safe_div(ctx, x, y)
        }))
        .with_function(builtin_fn_v2!("mod", |&ctx, x: num, y: num| {
            Quantity::safe_mod(ctx, x, y)
        }))
        .with_function(builtin_fn_v2!("add_assign", |&ctx, a: &num, b: num| {
            *a = Quantity::safe_add(ctx, a.clone(), b)?;
            Ok(a.clone())
        }))
        .with_function(builtin_fn_v2!("sub_assign", |&ctx, a: &num, b: num| {
            *a = Quantity::safe_sub(ctx, a.clone(), b)?;
            Ok(a.clone())
        }))
        .with_function(builtin_fn_v2!("mul_assign", |&ctx, a: &num, b: num| {
            *a = Quantity::safe_mul(ctx, a.clone(), b)?;
            Ok(a.clone())
        }))
        .with_function(builtin_fn_v2!("div_assign", |&ctx, a: &num, b: num| {
            *a = Quantity::safe_div(ctx, a.clone(), b)?;
            Ok(a.clone())
        }))
        .with_function(builtin_fn_v2!("eq", |&ctx, x: num, y: num| {
            Quantity::safe_eq(ctx, x, y)
        }))
        .with_function(builtin_fn_v2!("ne", |&ctx, x: num, y: num| {
            Quantity::safe_ne(ctx, x, y)
        }))
        .with_function(builtin_fn_v2!("lt", |&ctx, x: num, y: num| {
            Quantity::safe_lt(ctx, x, y)
        }))
        .with_function(builtin_fn_v2!("le", |&ctx, x: num, y: num| {
            Quantity::safe_le(ctx, x, y)
        }))
        .with_function(builtin_fn_v2!("gt", |&ctx, x: num, y: num| {
            Quantity::safe_gt(ctx, x, y)
        }))
        .with_function(builtin_fn_v2!("ge", |&ctx, x: num, y: num| {
            Quantity::safe_ge(ctx, x, y)
        }))
        .with_function(builtin_fn_v2!("not", |&ctx, x: num| {
            Quantity::safe_not(ctx, x)
        }))
        .with_function(builtin_fn_v2!("and", |&ctx, x: num, y: num| {
            Quantity::safe_and(ctx, x, y)
        }))
        .with_function(builtin_fn_v2!("or", |&ctx, x: num, y: num| {
            Quantity::safe_or(ctx, x, y)
        }))
        .with_function(builtin_fn_v2!("bit_not", |&ctx, x: num| {
            Quantity::safe_bit_not(ctx, x)
        }))
        .with_function(builtin_fn_v2!("bit_or", |&ctx, x: num, y: num| {
            Quantity::safe_bit_or(ctx, x, y)
        }))
        .with_function(builtin_fn_v2!("bit_and", |&ctx, x: num, y: num| {
            Quantity::safe_bit_and(ctx, x, y)
        }))
        .with_function(builtin_fn_v2!("bit_xor", |&ctx, x: num, y: num| {
            Quantity::safe_bit_xor(ctx, x, y)
        }))
        .with_function(builtin_fn_v2!("bit_shl", |&ctx, x: num, y: num| {
            Quantity::safe_bit_shl(ctx, x, y)
        }))
        .with_function(builtin_fn_v2!("bit_shr", |&ctx, x: num, y: num| {
            Quantity::safe_bit_shr(ctx, x, y)
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
        .with_function(builtin_fn_v2!(
            "method_call",
            |&ctx, handle: any, spec: any| {
                // Extract handle
                let handle = match handle {
                    Value::Handle(h) => h,
                    Value::Ref(r) => match r.borrow().clone() {
                        Value::Handle(h) => h,
                        other => {
                            return Err(Exception::new(
                                "TypeError",
                                format!(
                                    "expected handle on lhs of '.', found {}",
                                    other.ty().pretty_string(ctx)
                                ),
                            )
                            .with_backtrace(ctx.backtrace()))
                        }
                    },
                    other => {
                        return Err(Exception::new(
                            "TypeError",
                            format!(
                                "expected handle on lhs of '.', found {}",
                                other.ty().pretty_string(ctx)
                            ),
                        )
                        .with_backtrace(ctx.backtrace()))
                    }
                };

                // Extract (method_name, args_list)
                let (method_name, arg_values) = match spec {
                    Value::Tuple(items) if items.len() == 2 => {
                        let name_val = &items[0];
                        let args_val = &items[1];

                        let name = match &**name_val {
                            Value::String(s) => s.clone(),
                            Value::Ref(r) => match r.borrow().clone() {
                                Value::String(s) => s,
                                other => {
                                    return Err(Exception::new(
                                        "TypeError",
                                        format!(
                                            "method name must be string, found {}",
                                            other.ty().pretty_string(ctx)
                                        ),
                                    )
                                    .with_backtrace(ctx.backtrace()))
                                }
                            },
                            other => {
                                return Err(Exception::new(
                                    "TypeError",
                                    format!(
                                        "method name must be string, found {}",
                                        other.ty().pretty_string(ctx)
                                    ),
                                )
                                .with_backtrace(ctx.backtrace()))
                            }
                        };

                        let args: Vec<_> = match &**args_val {
                            Value::List(list) => list.borrow_slice().iter().cloned().collect(),
                            Value::Ref(r) => match r.borrow().clone() {
                                Value::List(list) => list.borrow_slice().iter().cloned().collect(),
                                other => {
                                    return Err(Exception::new(
                                        "TypeError",
                                        format!(
                                            "method args must be list, found {}",
                                            other.ty().pretty_string(ctx)
                                        ),
                                    )
                                    .with_backtrace(ctx.backtrace()))
                                }
                            },
                            other => {
                                return Err(Exception::new(
                                    "TypeError",
                                    format!(
                                        "method args must be list, found {}",
                                        other.ty().pretty_string(ctx)
                                    ),
                                )
                                .with_backtrace(ctx.backtrace()))
                            }
                        };

                        (name, args)
                    }
                    Value::Tuple(_) => {
                        return Err(Exception::new(
                            "TypeError",
                            "method call tuple must be (name, args)".to_string(),
                        )
                        .with_backtrace(ctx.backtrace()))
                    }
                    other => {
                        return Err(Exception::new(
                            "TypeError",
                            format!(
                                "expected method call tuple on rhs of '.', found {}",
                                other.ty().pretty_string(ctx)
                            ),
                        )
                        .with_backtrace(ctx.backtrace()))
                    }
                };

                // Resolve the method function registered for this handle type.
                let func = ctx
                    .handle_methods
                    .get(handle.tag(), method_name.clone().into())
                    .ok_or_else(|| {
                        Exception::new(
                            "NameError",
                            format!(
                                "handle type '{}' has no method '{}'",
                                handle.tag(),
                                method_name
                            ),
                        )
                        .with_backtrace(ctx.backtrace())
                    })?;

                let mut call_args = Vec::with_capacity(1 + arg_values.len());
                call_args.push(Value::Handle(handle));
                call_args.extend(arg_values);

                interp::call_function(ctx, &func, call_args)
            }
        ));
}
