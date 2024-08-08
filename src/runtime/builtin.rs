use super::context::Context;
use super::exception::Exception;
use super::module::Module;
use super::name::{Function, Param};
use super::value::{CastFrom, Dim, Float, Integer, Number, Numeric, Quantity, Ty, Value};

use crate::print::PrettyString;

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
    ($name:ident, |&$ctx:ident, $($p:ident : $ty:ident),*| $body:expr) => {{
        let params = builtin_params!($($p:$ty),*);
        Function::builtin(stringify!($name), params, |ctx, mut args| {
            let f = |$ctx: &mut Context, $($p: builtin_type!($ty)),*| -> Result<_, Exception> { $body };
            Ok(Value::from(f(ctx, $(take_arg(ctx, stringify!($p), &mut args)?),*)?))
        })
    }};
    ($name:ident, |$($p:ident : $ty:ident),*| $($part:ident)::+) => {{
        let params = builtin_params!($($p:$ty),*);
        Function::builtin(stringify!($name), params, |ctx, mut args| {
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
        .with_function(builtin_fn!(pos, |&ctx, x: num| Ok(x)))
        .with_function(builtin_fn!(neg, |&ctx, x: num| Ok(-x)))
        .with_function(builtin_fn!(add, |x: num, y: num| builtin::safe_add))
        .with_function(builtin_fn!(sub, |x: num, y: num| builtin::safe_sub))
        .with_function(builtin_fn!(mul, |x: num, y: num| builtin::safe_mul))
        .with_function(builtin_fn!(div, |x: num, y: num| builtin::safe_div))
        .with_function(builtin_fn!(pow, |x: num, y: num| builtin::safe_pow))
        .with_function(builtin_fn!(eq, |x: num, y: num| builtin::safe_eq))
        .with_function(builtin_fn!(ne, |x: num, y: num| builtin::safe_ne))
        .with_function(builtin_fn!(lt, |x: num, y: num| builtin::safe_lt))
        .with_function(builtin_fn!(le, |x: num, y: num| builtin::safe_le))
        .with_function(builtin_fn!(gt, |x: num, y: num| builtin::safe_gt))
        .with_function(builtin_fn!(ge, |x: num, y: num| builtin::safe_ge))
        .with_function(builtin_fn!(not, |x: int| builtin::safe_not))
        .with_function(builtin_fn!(and, |x: int, y: int| builtin::safe_and))
        .with_function(builtin_fn!(or, |x: int, y: int| builtin::safe_or))
        .with_function(builtin_fn!(bit_not, |x: num| builtin::safe_bit_not))
        .with_function(builtin_fn!(bit_or, |x: num, y: int| builtin::safe_bit_or))
        .with_function(builtin_fn!(bit_and, |x: num, y: int| builtin::safe_bit_and))
        .with_function(builtin_fn!(bit_shl, |x: num, y: int| builtin::safe_bit_shl))
        .with_function(builtin_fn!(bit_shr, |x: num, y: int| builtin::safe_bit_shr));
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

mod builtin {
    use super::*;
    use crate::runtime::coerce;

    // MARK: Arithmetic functions

    pub fn safe_neg(ctx: &Context, a: Quantity) -> Result<Quantity, Exception> {
        Ok(-a)
    }

    pub fn safe_add(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, b) = coerce::binary_pair(ctx, a, b);
        let (a, a_dim, a_ty) = a.into_tuple_with_ty();
        let (b, b_dim, b_ty) = b.into_tuple_with_ty();

        let dim = Dim::unify(ctx, a_dim, b_dim)?;
        let number = match (a, b) {
            (Number::Int(a), Number::Int(b)) => Ok(Number::Int(a + b)),
            (Number::Float(a), Number::Float(b)) => Ok(Number::Float(a + b)),
            (a, b) => Err(Exception::new(
                "TypeError: type mismatch",
                format!("{} != {}", a_ty.pretty_string(&()), b_ty.pretty_string(&())),
            )
            .with_backtrace(ctx.backtrace())),
        }?;

        Ok(Quantity::new(number, dim))
    }

    pub fn safe_sub(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, b) = coerce::binary_pair(ctx, a, b);
        let (a, a_dim, a_ty) = a.into_tuple_with_ty();
        let (b, b_dim, b_ty) = b.into_tuple_with_ty();

        let dim = Dim::unify(ctx, a_dim, b_dim)?;
        let number = match (a, b) {
            (Number::Int(a), Number::Int(b)) => Ok(Number::Int(a - b)),
            (Number::Float(a), Number::Float(b)) => Ok(Number::Float(a - b)),
            (a, b) => Err(Exception::new(
                "TypeError: type mismatch",
                format!("{} != {}", a_ty.pretty_string(&()), b_ty.pretty_string(&())),
            )
            .with_backtrace(ctx.backtrace())),
        }?;

        Ok(Quantity::new(number, dim))
    }

    pub fn safe_mul(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, b) = coerce::binary_pair(ctx, a, b);
        let (a, a_dim, a_ty) = a.into_tuple_with_ty();
        let (b, b_dim, b_ty) = b.into_tuple_with_ty();

        let dim = Dim::unify(ctx, a_dim, b_dim)?;
        let number = match (a, b) {
            (Number::Int(a), Number::Int(b)) => Ok(Number::Int(a * b)),
            (Number::Float(a), Number::Float(b)) => Ok(Number::Float(a * b)),
            (a, b) => Err(Exception::new(
                "TypeError: type mismatch",
                format!("{} != {}", a_ty.pretty_string(&()), b_ty.pretty_string(&())),
            )
            .with_backtrace(ctx.backtrace())),
        }?;

        Ok(Quantity::new(number, dim))
    }

    pub fn safe_div(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, b) = coerce::binary_pair(ctx, a, b);
        let (a, a_dim, a_ty) = a.into_tuple_with_ty();
        let (b, b_dim, b_ty) = b.into_tuple_with_ty();
        if b.is_zero() {
            return Err(Exception::new(
                "ValueError: division by zero",
                format!("cannot divide {} by zero", a.pretty_string(&())),
            )
            .with_backtrace(ctx.backtrace()));
        }

        let dim = Dim::unify(ctx, a_dim, b_dim)?;
        let number = match (a, b) {
            (Number::Int(a), Number::Int(b)) => {
                let (quo, rem) = a.div_rem(b.clone());
                if rem.is_zero() {
                    Ok(Number::Int(quo))
                } else {
                    let prec = ctx.config.precision;
                    let rem = Float::with_val(prec, rem) / Float::with_val(prec, b);
                    Ok(Number::Float(Float::with_val(prec, quo) + rem))
                }
            }
            (Number::Float(a), Number::Float(b)) => Ok(Number::Float(a / b)),
            (a, b) => Err(Exception::new(
                "TypeError: type mismatch",
                format!("{} != {}", a_ty.pretty_string(&()), b_ty.pretty_string(&())),
            )
            .with_backtrace(ctx.backtrace())),
        }?;

        Ok(Quantity::new(number, dim))
    }

    pub fn safe_pow(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, a_dim) = a.into_tuple();
        let (b, b_dim) = b.into_tuple();

        let dim = Dim::unify(ctx, a_dim, b_dim)?;
        let number = match (a, b) {
            (Number::Int(a), Number::Int(b)) => {
                if b < 0 {
                    let prec = ctx.config.precision;
                    Ok(Number::Float(
                        Float::with_val(prec, a).pow(Float::with_val(prec, -b)),
                    ))
                } else {
                    Ok(Number::Int(a.pow(b.to_u32().unwrap())))
                }
            }
            (Number::Float(a), Number::Float(b)) => Ok(Number::Float(a.pow(b))),
            (a, b) => Err(Exception::new(
                "TypeError: type mismatch",
                format!("{} != {}", a.pretty_string(&()), b.pretty_string(&())),
            )
            .with_backtrace(ctx.backtrace())),
        }?;

        Ok(Quantity::new(number, dim))
    }

    // MARK: Bitwise functions

    pub fn safe_bit_not(ctx: &Context, a: Quantity) -> Result<Quantity, Exception> {
        let (a, dim) = a.into_tuple();
        let a = a.get_int().cloned().ok_or_else(|| {
            Exception::new(
                "TypeError: expected integer",
                format!("got {}", a.ty().pretty_string(&())),
            )
            .with_backtrace(ctx.backtrace())
        })?;

        let number = Number::Int(!a);
        Ok(Quantity::new(number, dim))
    }

    pub fn safe_bit_and(ctx: &Context, a: Quantity, b: Integer) -> Result<Quantity, Exception> {
        let (a, dim) = a.into_tuple();
        let number = match a {
            Number::Int(i) => Ok(Number::Int(i & b)),
            Number::Float(_) => Err(Exception::new(
                "TypeError: expected integer",
                format!("got {}", a.ty().pretty_string(&())),
            )
            .with_backtrace(ctx.backtrace())),
        }?;

        Ok(Quantity::new(number, dim))
    }

    pub fn safe_bit_or(ctx: &Context, a: Quantity, b: Integer) -> Result<Quantity, Exception> {
        let (a, dim) = a.into_tuple();
        let number = match a {
            Number::Int(i) => Ok(Number::Int(i | b)),
            Number::Float(_) => Err(Exception::new(
                "TypeError: expected integer",
                format!("got {}", a.ty().pretty_string(&())),
            )
            .with_backtrace(ctx.backtrace())),
        }?;

        Ok(Quantity::new(number, dim))
    }

    pub fn safe_bit_shl(ctx: &Context, a: Quantity, b: Integer) -> Result<Quantity, Exception> {
        if b < 0 {
            return Err(
                Exception::new("ValueError", "negative shift count".to_owned())
                    .with_backtrace(ctx.backtrace()),
            );
        }

        let shift = b.to_u32().ok_or_else(|| {
            Exception::new("ValueError: shift count too large", format!("{}", b))
                .with_backtrace(ctx.backtrace())
        })?;
        let integer = match a.number {
            Number::Int(i) => Ok(i),
            Number::Float(_) => Err(Exception::new(
                "TypeError: expected integer",
                format!("got {}", a.ty().pretty_string(&())),
            )
            .with_backtrace(ctx.backtrace())),
        }?;

        let number = Number::Int(integer << shift);
        Ok(Quantity::new(number, a.dim))
    }

    pub fn safe_bit_shr(ctx: &Context, a: Quantity, b: Integer) -> Result<Quantity, Exception> {
        if b < 0 {
            return Err(
                Exception::new("ValueError", "negative shift count".to_owned())
                    .with_backtrace(ctx.backtrace()),
            );
        }

        let shift = b.to_u32().ok_or_else(|| {
            Exception::new("ValueError: shift count too large", format!("{}", b))
                .with_backtrace(ctx.backtrace())
        })?;
        let integer = match a.number {
            Number::Int(i) => Ok(i),
            Number::Float(_) => Err(Exception::new(
                "TypeError: expected integer",
                format!("got {}", a.ty().pretty_string(&())),
            )
            .with_backtrace(ctx.backtrace())),
        }?;

        let number = Number::Int(integer >> shift);
        Ok(Quantity::new(number, a.dim))
    }

    // MARK: Logical functions

    pub fn safe_not(ctx: &Context, a: Integer) -> Result<Integer, Exception> {
        Ok(Integer::from(a.is_zero()))
    }

    pub fn safe_and(ctx: &Context, a: Integer, b: Integer) -> Result<Integer, Exception> {
        Ok(Integer::from(!a.is_zero() && !b.is_zero()))
    }

    pub fn safe_or(ctx: &Context, a: Integer, b: Integer) -> Result<Integer, Exception> {
        Ok(Integer::from(!a.is_zero() || !b.is_zero()))
    }

    // MARK: Comp. functions

    pub fn safe_eq(ctx: &Context, a: Integer, b: Integer) -> Result<Integer, Exception> {
        Ok(Integer::from(a == b))
    }

    pub fn safe_ne(ctx: &Context, a: Integer, b: Integer) -> Result<Integer, Exception> {
        Ok(Integer::from(a != b))
    }

    pub fn safe_lt(ctx: &Context, a: Integer, b: Integer) -> Result<Integer, Exception> {
        Ok(Integer::from(a < b))
    }

    pub fn safe_le(ctx: &Context, a: Integer, b: Integer) -> Result<Integer, Exception> {
        Ok(Integer::from(a <= b))
    }

    pub fn safe_gt(ctx: &Context, a: Integer, b: Integer) -> Result<Integer, Exception> {
        Ok(Integer::from(a > b))
    }

    pub fn safe_ge(ctx: &Context, a: Integer, b: Integer) -> Result<Integer, Exception> {
        Ok(Integer::from(a >= b))
    }
}
