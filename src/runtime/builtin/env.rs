use crate::runtime::{Context, Exception, FileHandle, Handle, UserTy, Value};
use std::env;

pub(super) fn register(ctx: &mut Context) {
    ctx.module_mut("builtin")
        .expect("builtin module should exist")
        .new_submodule("env")
        .unwrap()
        .with_function(builtin_fn_v2!("getenv", |&ctx, var: str| {
            match env::var(var) {
                Ok(value) => Ok(Value::String(value)),
                Err(env::VarError::NotPresent) => Ok(Value::Empty),
                Err(e) => {
                    Err(Exception::new("IoError", e.to_string()).with_backtrace(ctx.backtrace()))
                }
            }
        }))
        .with_function(builtin_fn_v2!("setenv", |&ctx, var: str, value: str| {
            env::set_var(var, value);
            Ok(Value::default())
        }))
        .with_function(builtin_fn_v2!("unsetenv", |&ctx, var: str| {
            env::remove_var(var);
            Ok(Value::default())
        }));
}
