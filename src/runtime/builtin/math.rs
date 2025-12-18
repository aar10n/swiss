use crate::runtime::{Context, Exception, Module, Quantity};

pub(super) fn register(module: &mut Module) {
    module
        .with_function(builtin_fn_v2!("ln", |&ctx, x: num| {
            Quantity::safe_ln(ctx, x)
        }))
        .with_function(builtin_fn_v2!("log2", |&ctx, x: num| {
            Quantity::safe_log2(ctx, x)
        }))
        .with_function(builtin_fn_v2!("log10", |&ctx, x: num| {
            Quantity::safe_log10(ctx, x)
        }))
        .with_function(builtin_fn_v2!("pow", |&ctx, x: num, y: num| {
            Quantity::safe_pow(ctx, x, y)
        }))
        .with_function(builtin_fn_v2!("sqrt", |&ctx, x: num| {
            Quantity::safe_sqrt(ctx, x)
        }))
        .with_function(builtin_fn_v2!("cbrt", |&ctx, x: num| {
            Quantity::safe_cbrt(ctx, x)
        }))
        .with_function(builtin_fn_v2!("floor", |&ctx, x: num| {
            Quantity::safe_floor(ctx, x)
        }))
        .with_function(builtin_fn_v2!("ceil", |&ctx, x: num| {
            Quantity::safe_ceil(ctx, x)
        }))
        .with_function(builtin_fn_v2!("round", |&ctx, x: num| {
            Quantity::safe_round(ctx, x)
        }))
        .with_function(builtin_fn_v2!("sin", |&ctx, x: num| {
            Quantity::safe_sin(ctx, x)
        }))
        .with_function(builtin_fn_v2!("cos", |&ctx, x: num| {
            Quantity::safe_cos(ctx, x)
        }))
        .with_function(builtin_fn_v2!("tan", |&ctx, x: num| {
            Quantity::safe_tan(ctx, x)
        }))
        .with_function(builtin_fn_v2!("asin", |&ctx, x: num| {
            Quantity::safe_asin(ctx, x)
        }))
        .with_function(builtin_fn_v2!("acos", |&ctx, x: num| {
            Quantity::safe_acos(ctx, x)
        }))
        .with_function(builtin_fn_v2!("atan", |&ctx, x: num| {
            Quantity::safe_atan(ctx, x)
        }))
        .with_function(builtin_fn_v2!("atan2", |&ctx, y: num, x: num| {
            Quantity::safe_atan2(ctx, y, x)
        }))
        .with_function(builtin_fn_v2!("sinh", |&ctx, x: num| {
            Quantity::safe_sinh(ctx, x)
        }))
        .with_function(builtin_fn_v2!("cosh", |&ctx, x: num| {
            Quantity::safe_cosh(ctx, x)
        }))
        .with_function(builtin_fn_v2!("tanh", |&ctx, x: num| {
            Quantity::safe_tanh(ctx, x)
        }));
}
