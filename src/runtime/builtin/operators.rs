use crate::runtime::{Context, Exception, Module, Quantity};

pub(super) fn register(module: &mut Module) {
    module
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
        }));
}
