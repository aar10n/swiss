use crate::runtime::{Context, Exception, FileHandle, Handle, Module, Value};

pub(super) fn register(ctx: &mut Context) {
    ctx.get_module_mut("builtin")
        .expect("builtin module should exist")
        .with_function(builtin_fn_v2!("open", |&ctx, path: str| {
            match FileHandle::open_read(&path) {
                Ok(file) => Ok(Value::Handle(Handle::new("file".into(), file))),
                Err(e) => {
                    Err(Exception::new("IoError", e.to_string()).with_backtrace(ctx.backtrace()))
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
