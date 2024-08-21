use super::context::Context;
use super::exception::Exception;
use super::module::Module;
use super::name::{Function, Param};
use super::value::{CastFrom, Dim, Float, Integer, Number, Numeric, Quantity, Ty, Value};

use crate::print::PrettyString;
use crate::runtime::coerce;

use rug::ops::Pow;
use std::ops::{BitAnd, BitOr, Shl, Shr};
use ustr::Ustr;

#[rustfmt::skip]
macro_rules! builtin_type {
    (any) => { Value };
    (int) => { Integer };
    (float) => { Float };
    (num) => { Quantity };
}

#[rustfmt::skip]
macro_rules! builtin_ty {
    (int) => { Some(Ty::Int) };
    (float) => { Some(Ty::Float) };
    (num) => { Some(Ty::Num) };
    (any) => { Some(Ty::Any) };
}

macro_rules! builtin_params {
    () => { vec![] };
    ($p:ident : $ty:ident) => { vec![Param::from((stringify!($p).into(), builtin_ty!($ty)))] };
    ($p:ident : $ty:ident, $($rest:tt)*) => {{
        let mut v = vec![Param::from((stringify!($p).into(), builtin_ty!($ty)))];
        v.extend(builtin_params!($($rest)*));
        v
    }};
}

#[rustfmt::skip]
macro_rules! builtin_fn {
    ($name:tt, |&$ctx:ident, $($p:ident : $ty:ident),*| $body:expr) => {{
        let params = builtin_params!($($p:$ty),*);
        Function::builtin($name, params, |ctx, mut args| {
            let f = |$ctx: &mut Context, $($p: builtin_type!($ty)),*| -> Result<_, Exception> { $body };
            Ok(Value::from(f(ctx, $(take_arg(ctx, stringify!($p), &mut args)?),*)?))
        })
    }};
    ($name:tt, |$($p:ident : $ty:ident),*| $($part:ident)::+) => {{
        let params = builtin_params!($($p:$ty),*);
        Function::builtin($name, params, |ctx, mut args| {
            Ok(Value::from(($($part)::+)(ctx, $(take_arg(ctx, stringify!($p), &mut args)?),*)?))
        })
    }};
}

#[rustfmt::skip]
macro_rules! builtin_op {
    ($($_:tt)*) => {
        todo!()
    }
}

pub type NativeFn = fn(&mut Context, Vec<Value>) -> Result<Value, Exception>;

pub fn register_builtin_module(ctx: &mut Context) {
    ctx.modules
        .new_module("builtin")
        .unwrap()
        .with_function(builtin_fn!("pos", |&ctx, x: num| Ok(x)))
        .with_function(builtin_fn!("neg", |&ctx, x: num| Ok(-x)))
        .with_function(builtin_fn!("add", |x: num, y: num| Quantity::safe_add))
        .with_function(builtin_fn!("sub", |x: num, y: num| Quantity::safe_sub))
        .with_function(builtin_fn!("mul", |x: num, y: num| Quantity::safe_mul))
        .with_function(builtin_fn!("div", |x: num, y: num| Quantity::safe_div))
        .with_function(builtin_fn!("mod", |x: num, y: num| Quantity::safe_mod))
        .with_function(builtin_fn!("pow", |x: num, y: num| Quantity::safe_pow))
        .with_function(builtin_fn!("eq", |x: num, y: num| Quantity::safe_eq))
        .with_function(builtin_fn!("ne", |x: num, y: num| Quantity::safe_ne))
        .with_function(builtin_fn!("lt", |x: num, y: num| Quantity::safe_lt))
        .with_function(builtin_fn!("le", |x: num, y: num| Quantity::safe_le))
        .with_function(builtin_fn!("gt", |x: num, y: num| Quantity::safe_gt))
        .with_function(builtin_fn!("ge", |x: num, y: num| Quantity::safe_ge))
        .with_function(builtin_fn!("not", |x: num| Quantity::safe_not))
        .with_function(builtin_fn!("and", |x: num, y: num| Quantity::safe_and))
        .with_function(builtin_fn!("or", |x: num, y: num| Quantity::safe_or))
        .with_function(builtin_fn!("bit_not", |x: num| Quantity::safe_bit_not))
        .with_function(builtin_fn!("bit_or", |&ctx, x: num, y: num| {
            Quantity::safe_bit_or(ctx, x, y)
        }))
        .with_function(builtin_fn!("bit_and", |&ctx, x: num, y: num| {
            Quantity::safe_bit_and(ctx, x, y)
        }))
        .with_function(builtin_fn!("bit_xor", |&ctx, x: num, y: num| {
            Quantity::safe_bit_xor(ctx, x, y)
        }))
        .with_function(builtin_fn!("bit_shl", |&ctx, x: num, y: num| {
            Quantity::safe_bit_shl(ctx, x, y)
        }))
        .with_function(builtin_fn!("bit_shr", |&ctx, x: num, y: num| {
            Quantity::safe_bit_shr(ctx, x, y)
        }))
        .with_function(builtin_fn!("typeof", |&ctx, v: any| Ok(Value::from(
            v.ty().to_string()
        ))))
        .with_function(builtin_fn!("debug", |&ctx, v: any| {
            if let Some(frame) = ctx.last_frame() {
                let src_ref = ctx.sources.lookup_span(frame.call_site).unwrap();
                print!("[DEBUG] {{{}}}", src_ref.file_name());
            } else {
                print!("[DEBUG]     ");
            }
            println!("{:?}", v);
            Ok(v)
        }));
}

fn take_arg<T: CastFrom<Value>>(
    ctx: &Context,
    param: &str,
    args: &mut Vec<Value>,
) -> Result<T, Exception> {
    if args.is_empty() {
        Err(
            Exception::new("missing argument", format!("missing argument: {}", param))
                .with_backtrace(ctx.backtrace()),
        )
    } else {
        T::cast(ctx, args.remove(0))
    }
}
