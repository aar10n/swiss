use crate::interp;
use crate::print::{DisplayString, PrettyString};
use crate::runtime::{Context, Exception, IoHandle, Module, Value};

pub(super) fn register(module: &mut Module) {
    module
        .with_function(builtin_fn_v2!("print", |&ctx, ...args| {
            let args = args
                .into_iter()
                .map(|arg| arg.pretty_string(ctx))
                .collect::<Vec<_>>()
                .join(" ");

            println!("[PRINT] --> {}", args);
            Ok(Value::default())
        }))
        .with_function(builtin_fn_v2!("debug", |&ctx, v: any| {
            println!("[DEBUG] {:?}", v);
            Ok(v)
        }))
        .with_function(builtin_fn_v2!("write", |&ctx, io: io, v: any| {
            let content = match &v {
                Value::String(s) => s.clone(),
                _ => v.display_string(ctx),
            };
            io.write_str(&content).map_err(|e| {
                Exception::new("IoError", e.to_string()).with_backtrace(ctx.backtrace())
            })?;
            Ok(Value::default())
        }))
        .with_function(builtin_fn_v2!("writeln", |&ctx, io: io, v: any| {
            let mut content = match &v {
                Value::String(s) => s.clone(),
                _ => v.display_string(ctx),
            };
            content.push('\n');
            io.write_str(&content).map_err(|e| {
                Exception::new("IoError", e.to_string()).with_backtrace(ctx.backtrace())
            })?;
            Ok(Value::default())
        }))
        .with_function(
            builtin_fn_v2!("format_apply", |&ctx, value: any, formatter: fn| {
                let io = IoHandle::buffer();

                // formatter(value, io)
                interp::call_function(ctx, &formatter, vec![value.clone(), Value::Io(io.clone())])?;
                if let Some(buf) = io.take_buffer() {
                    ctx.set_pending_output(buf);
                }
                Ok(value)
            }),
        );
}
