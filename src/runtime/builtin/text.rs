use crate::print::PrettyString;
use crate::runtime::{Context, Exception, Value};

pub(super) fn register(ctx: &mut Context) {
    ctx.module_mut("builtin")
        .expect("builtin module should exist")
        .new_submodule("text")
        .unwrap()
        .with_function(builtin_fn_v2!("trim", |&ctx, s: str| {
            Ok(s.trim().to_string())
        }))
        .with_function(builtin_fn_v2!("trim_start", |&ctx, s: str| {
            Ok(s.trim_start().to_string())
        }))
        .with_function(builtin_fn_v2!("trim_end", |&ctx, s: str| {
            Ok(s.trim_end().to_string())
        }))
        .with_function(builtin_fn_v2!("lower", |&ctx, s: str| {
            Ok(s.to_lowercase())
        }))
        .with_function(builtin_fn_v2!("upper", |&ctx, s: str| {
            Ok(s.to_uppercase())
        }))
        .with_function(builtin_fn_v2!("starts_with", |&ctx, s: str, prefix: str| {
            Ok(s.starts_with(prefix.as_str()))
        }))
        .with_function(builtin_fn_v2!("ends_with", |&ctx, s: str, suffix: str| {
            Ok(s.ends_with(suffix.as_str()))
        }))
        .with_function(builtin_fn_v2!("contains", |&ctx, s: str, needle: str| {
            Ok(s.contains(needle.as_str()))
        }))
        .with_function(builtin_fn_v2!("replace", |&ctx, s: str, from: str, to: str| {
            Ok(s.replace(from.as_str(), to.as_str()))
        }))
        .with_function(builtin_fn_v2!("split", |&ctx, s: str, sep: str?| {
            let items = match sep {
                None => s.split_whitespace().map(|part| Value::from(part.to_string())).collect(),
                Some(sep) => {
                    if sep.is_empty() {
                        return Err(Exception::new(
                            "ValueError",
                            "separator cannot be empty".to_string(),
                        )
                        .with_backtrace(ctx.backtrace()));
                    }
                    s.split(sep.as_str())
                        .map(|part| Value::from(part.to_string()))
                        .collect()
                }
            };
            Ok(Value::list(items))
        }))
        .with_function(builtin_fn_v2!("lines", |&ctx, s: str| {
            let items = s
                .lines()
                .map(|line| Value::from(line.to_string()))
                .collect();
            Ok(Value::list(items))
        }))
        .with_function(builtin_fn_v2!("strip_prefix", |&ctx, s: str, prefix: str| {
            Ok(s.strip_prefix(prefix.as_str()).unwrap_or(s.as_str()).to_string())
        }))
        .with_function(builtin_fn_v2!("strip_suffix", |&ctx, s: str, suffix: str| {
            Ok(s.strip_suffix(suffix.as_str()).unwrap_or(s.as_str()).to_string())
        }))
        .with_function(builtin_fn_v2!("join", |&ctx, items: any, sep: str| {
            let list = match items {
                Value::List(list) => list,
                other => {
                    return Err(Exception::new(
                        "TypeError",
                        format!(
                            "join expects list, found {}",
                            other.ty().plain_string(ctx)
                        ),
                    )
                    .with_backtrace(ctx.backtrace()))
                }
            };

            let mut parts = Vec::with_capacity(list.len());
            let slice = list.borrow_slice();
            for item in slice.iter() {
                let value = match item {
                    Value::Ref(r) => r.borrow().clone(),
                    other => other.clone(),
                };
                match value {
                    Value::String(s) => parts.push(s.to_string()),
                    other => {
                        return Err(Exception::new(
                            "TypeError",
                            format!(
                                "join expects string elements, found {}",
                                other.ty().plain_string(ctx)
                            ),
                        )
                        .with_backtrace(ctx.backtrace()))
                    }
                }
            }

            Ok(parts.join(sep.as_str()))
        }));
}
