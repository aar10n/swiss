use crate::runtime::{CastFrom, Context, Exception, Module, Value};

pub(super) fn register(module: &mut Module) {
    module.with_function(builtin_fn_v2!("typeof", |&ctx, v: any| Ok(v.ty().to_string())));
}

pub(crate) fn take_arg<T: CastFrom<Value>>(
    ctx: &Context,
    param: &str,
    args: &mut Vec<Value>,
) -> Result<T, Exception> {
    if args.is_empty() {
        Err(
            Exception::new("TypeError", format!("missing argument: {}", param))
                .with_backtrace(ctx.backtrace()),
        )
    } else {
        T::cast(ctx, args.remove(0))
    }
}

pub(crate) fn take_varargs(ctx: &Context, args: &mut Vec<Value>) -> Result<Vec<Value>, Exception> {
    Ok(args.drain(..).collect())
}
