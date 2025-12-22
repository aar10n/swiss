use crate::runtime::{Context, Exception, FileHandle, Handle, Module, Value};

pub(super) fn register(ctx: &mut Context) {
    ctx.module_mut("builtin")
        .expect("builtin module should exist")
        .new_submodule("os")
        .unwrap()
        .with_function(builtin_fn_v2!("open", |&ctx, path: str| {
            match FileHandle::open_read(&path) {
                Ok(file) => Ok(Value::Handle(Handle::new("file".into(), file))),
                Err(e) => {
                    Err(Exception::new("IoError", e.to_string()).with_backtrace(ctx.backtrace()))
                }
            }
        }))
        .with_function(builtin_fn_v2!("getenv", |&ctx, var: str| {
            match std::env::var(&var) {
                Ok(val) => Ok(Value::String(val)),
                Err(std::env::VarError::NotPresent) => Ok(Value::default()),
                Err(e) => {
                    Err(Exception::new("OsError", e.to_string()).with_backtrace(ctx.backtrace()))
                }
            }
        }));

    ctx.handle_methods.register(
        "file",
        "close",
        builtin_fn_v2!("file.close", |&ctx, f: file| {
            f.close();
            Ok(Value::default())
        }),
    );
    ctx.handle_methods.register(
        "file",
        "read_all",
        builtin_fn_v2!("file.read_all", |&ctx, f: file| {
            f.read_all().map(Value::String).map_err(|e| {
                Exception::new("IoError", e.to_string()).with_backtrace(ctx.backtrace())
            })
        }),
    );
}
