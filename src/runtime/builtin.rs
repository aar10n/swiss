use super::context::Context;
use super::exception::Exception;
use super::module::Module;
use super::name::{Function, Param};
use super::value::{
    CastFrom, CastInto, Dim, Float, Integer, Number, Numeric, Quantity, Ty, VRef, Value, ValueRef,
};

use crate::print::PrettyString;
use crate::runtime::coerce;

use paste::paste;
use rug::ops::Pow;
use std::ops::{BitAnd, BitOr, Shl, Shr};
use ustr::Ustr;

#[rustfmt::skip]
macro_rules! builtin_ty_v2 {
    (& $($rest:tt)*) => { Ty::Ref(Box::new(builtin_ty_v2!($($rest)*))) };
    (int) => { Ty::Int };
    (float) => { Ty::Float };
    (num) => { Ty::Num };
    (any) => { Ty::Any };
    (str) => { Ty::Str };
    (bool) => { Ty::Bool };
    (list) => { Ty::List };
    (tuple[$($t:ident),*]) => { Ty::Tuple(vec![$(builtin_ty_v2!($t)),*]) };
    (unit) => { Ty::Unit };
    (ty) => { Ty::Type };
}

#[rustfmt::skip]
macro_rules! builtin_type_v2 {
    (& $($rest:tt)*) => { &mut builtin_type_v2!($($rest)*) };
    (any) => { Value };
    (int) => { Integer };
    (float) => { Float };
    (num) => { Quantity };
    (str) => { String };
    (bool) => { bool };
    (unit) => { Ustr };
    (ty) => { Ty };
    (...) => { Vec<Value> };
}

#[rustfmt::skip]
macro_rules! builtin_fn_v2 {
    (__params ($($acc:tt)*) ... $p:ident | $($rest:tt)*) => {
        vec![$($acc)* Param::variadic(stringify!($p).into())]
    };
    (__params ($($acc:tt)*) $p:ident : & $t:ident | $($rest:tt)*) => {
        vec![$($acc)* Param::from((stringify!($p).into(), Some(builtin_ty_v2!(& $t))))]
    };
    (__params ($($acc:tt)*) $p:ident : $t:ident | $($rest:tt)*) => {
        vec![$($acc)* Param::from((stringify!($p).into(), Some(builtin_ty_v2!($t))))]
    };
    (__params ($($acc:tt)*) $p:ident : & $t:ident, $($rest:tt)*) => {
        builtin_fn_v2!(__params ($($acc)* Param::from((stringify!($p).into(), Some(builtin_ty_v2!(& $t)))),) $($rest)*)
    };
    (__params ($($acc:tt)*) $p:ident : $t:ident, $($rest:tt)*) => {
        builtin_fn_v2!(__params ($($acc)* Param::from((stringify!($p).into(), Some(builtin_ty_v2!($t)))),) $($rest)*)
    };

    (__closure ($($acc:tt)*) ... $p:ident | $($rest:tt)*) => {
        |$($acc)* $p: builtin_type_v2!(...)| -> Result<_, Exception> { $($rest)* }
    };
    (__closure ($($acc:tt)*) $p:ident : & $t:ident | $($rest:tt)*) => {
        |$($acc)* $p: builtin_type_v2!(& $t)| -> Result<_, Exception> { $($rest)* }
    };
    (__closure ($($acc:tt)*) $p:ident : $t:ident | $($rest:tt)*) => {
        |$($acc)* $p: builtin_type_v2!($t)| -> Result<_, Exception> { $($rest)* }
    };
    (__closure ($($acc:tt)*) $p:ident : & $t:ident, $($rest:tt)*) => {
        builtin_fn_v2!(__closure ($($acc)* $p: builtin_type_v2!(& $t),) $($rest)*)
    };
    (__closure ($($acc:tt)*) $p:ident : $t:ident, $($rest:tt)*) => {
        builtin_fn_v2!(__closure ($($acc)* $p: builtin_type_v2!($t),) $($rest)*)
    };

    (__invoke ($($deferred:tt)*) $res:ident $f:ident ($ctx:ident, $args:expr, $($acc:tt)*) ... $p:ident | $($rest:tt)*) => {
        let $p = take_varargs($ctx, $args)?;
        $res = $f($ctx, $($acc)* $p)?;
        $($deferred)*
    };
    (__invoke ($($deferred:tt)*) $res:ident $f:ident ($ctx:ident, $args:expr, $($acc:tt)*) $p:ident : & $t:ident | $($rest:tt)*) => {paste!{
        let [< $p _ref >] = take_arg::<ValueRef>($ctx, stringify!($p), $args)?;
        let mut $p = CastInto::<builtin_type_v2!($t)>::cast($ctx, [< $p _ref >].borrow().clone())?;
        $res = $f($ctx, $($acc)* &mut $p)?;
        [<$p _ref>].set($p.into());
        $($deferred)*
    }};
    (__invoke ($($deferred:tt)*) $res:ident $f:ident ($ctx:ident, $args:expr, $($acc:tt)*) $p:ident : $t:ident | $($rest:tt)*) => {
        let $p = take_arg::<builtin_type_v2!($t)>($ctx, stringify!($p), $args)?;
        $res = $f($ctx, $($acc)* $p)?;
        $($deferred)*
    };
    (__invoke ($($deferred:tt)*) $res:ident $f:ident ($ctx:ident, $args:expr, $($acc:tt)*) $p:ident : & $t:ident, $($rest:tt)*) => {paste!{
        let [< $p _ref >] = take_arg::<ValueRef>($ctx, stringify!($p), $args)?;
        let mut $p = CastInto::<builtin_type_v2!($t)>::cast($ctx, [< $p _ref >].borrow().clone())?;
        builtin_fn_v2!(__invoke ($($deferred)* [<$p _ref>].set($p.into());) $res $f ($ctx, $args, $($acc)* &mut $p,) $($rest)*)
    }};
    (__invoke ($($deferred:tt)*) $res:ident $f:ident ($ctx:ident, $args:expr, $($acc:tt)*) $p:ident : $t:ident, $($rest:tt)*) => {
        let $p = take_arg::<builtin_type_v2!($t)>($ctx, stringify!($p), $args)?;
        builtin_fn_v2!(__invoke ($($deferred)*) $res $f ($ctx, $args, $($acc)* $p,) $($rest)*)
    };

    ($name:tt, |&$ctx:ident, $($rest:tt)*) => {{
        let params = builtin_fn_v2!(__params () $($rest)*);
        Function::builtin($name, params, |ctx, mut args| {
            let f = builtin_fn_v2!(__closure ($ctx: &mut Context, ) $($rest)*);
            let result: _;
            builtin_fn_v2!(__invoke () result f (ctx, &mut args,) $($rest)*);
            Ok(Value::from(result))
        })
    }};
}

pub type NativeFn = fn(&mut Context, Vec<Value>) -> Result<Value, Exception>;

pub fn register_builtin_module(ctx: &mut Context) {
    ctx.modules
        .new_module("builtin")
        .unwrap()
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
        .with_function(builtin_fn_v2!("mod", |&ctx, x: num, y: num| {
            Quantity::safe_mod(ctx, x, y)
        }))
        .with_function(builtin_fn_v2!("pow", |&ctx, x: num, y: num| {
            Quantity::safe_pow(ctx, x, y)
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
        }))
        .with_function(builtin_fn_v2!("typeof", |&ctx, v: any| Ok(v
            .ty()
            .to_string())))
        .with_function(builtin_fn_v2!("unit_cast", |&ctx, v: num, u: unit| {
            let target_unit = ctx
                .active_module()
                .unwrap()
                .resolve_unit_suffix(u.into())
                .map_err(|_| Exception::new("NameError", format!("unknown unit: {}", u)))?;

            // first, convert the input quantity to base units
            let base_value = match &v.dim.unit {
                Some((_, source_scale)) => {
                    Number::safe_div(ctx, v.number.clone(), source_scale.clone())?
                }
                None => v.number.clone(),
            };

            // then convert from base units to target units by dividing by target scale
            let converted_number = Number::safe_div(ctx, base_value, target_unit.scale.clone())?;
            let new_dim = Dim::new(target_unit.dim_expr.clone(), Some((u, Number::one())));
            Ok(Value::Quantity(Quantity::new(converted_number, new_dim)))
        }))
        .with_function(builtin_fn_v2!("print", |&ctx, ...args| {
            print!("[PRINT] --> ");
            for arg in args {
                print!("{}", arg.pretty_string(ctx));
            }
            println!();
            Ok(Value::default())
        }))
        .with_function(builtin_fn_v2!("debug", |&ctx, v: any| {
            println!("[DEBUG] {:?}", v);
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
            Exception::new("TypeError", format!("missing argument: {}", param))
                .with_backtrace(ctx.backtrace()),
        )
    } else {
        T::cast(ctx, args.remove(0))
    }
}

fn take_varargs(ctx: &Context, args: &mut Vec<Value>) -> Result<Vec<Value>, Exception> {
    Ok(args.drain(..).collect())
}
