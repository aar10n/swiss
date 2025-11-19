use super::context::Context;
use super::exception::Exception;
use super::module::Module;
use super::name::{Function, Param};
use super::value::{
    CastFrom, CastInto, Dim, Float, Integer, Number, Numeric, Quantity, Ty, VRef, Value, ValueRef,
};

use crate::interp::InterpError;
use crate::print::PrettyString;
use crate::runtime::{builtin, coerce};

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

#[rustfmt::skip]
macro_rules! builtin_interface {
    // Main entry point - parse all function declarations
    [$name:literal $($rest:tt)*] => {{
        use crate::ast::Spanned;
        use crate::source::SourceSpan;
        use super::interface::Interface;
        use super::name::{Function, FunctionKind, Param};
        use ustr::UstrSet;

        let functions = builtin_interface!(__parse_all_functions [] $($rest)*);
        let optional_set = builtin_interface!(__parse_optional_set [] $($rest)*);

        Interface::new(
            Spanned::new($name.into(), SourceSpan::default()),
            functions,
            optional_set
        )
    }};

    // Parse all functions (both required and optional)
    (__parse_all_functions [$($acc:tt)*]) => {
        vec![$($acc)*]
    };
    // Optional function with return type
    (__parse_all_functions [$($acc:tt)*] $fn_name:ident ? : ($($param_ty:ident),*) -> $ret_ty:ident; $($rest:tt)*) => {
        builtin_interface!(__parse_all_functions [$($acc)*
            Function::new(
                Spanned::new(stringify!($fn_name).into(), SourceSpan::default()),
                vec![$(Param::from((format!("_{}", stringify!($param_ty)).into(), Some(builtin_ty_v2!($param_ty))))),*],
                FunctionKind::Native(|_, _| unreachable!("interface function should not be called"))
            ),
        ] $($rest)*)
    };
    // Optional function without return type
    (__parse_all_functions [$($acc:tt)*] $fn_name:ident ? : ($($param_ty:ident),*); $($rest:tt)*) => {
        builtin_interface!(__parse_all_functions [$($acc)*
            Function::new(
                Spanned::new(stringify!($fn_name).into(), SourceSpan::default()),
                vec![$(Param::from((format!("_{}", stringify!($param_ty)).into(), Some(builtin_ty_v2!($param_ty))))),*],
                FunctionKind::Native(|_, _| unreachable!("interface function should not be called"))
            ),
        ] $($rest)*)
    };
    // Required function with return type
    (__parse_all_functions [$($acc:tt)*] $fn_name:ident : ($($param_ty:ident),*) -> $ret_ty:ident; $($rest:tt)*) => {
        builtin_interface!(__parse_all_functions [$($acc)*
            Function::new(
                Spanned::new(stringify!($fn_name).into(), SourceSpan::default()),
                vec![$(Param::from((format!("_{}", stringify!($param_ty)).into(), Some(builtin_ty_v2!($param_ty))))),*],
                FunctionKind::Native(|_, _| unreachable!("interface function should not be called"))
            ),
        ] $($rest)*)
    };
    // Required function without return type
    (__parse_all_functions [$($acc:tt)*] $fn_name:ident : ($($param_ty:ident),*); $($rest:tt)*) => {
        builtin_interface!(__parse_all_functions [$($acc)*
            Function::new(
                Spanned::new(stringify!($fn_name).into(), SourceSpan::default()),
                vec![$(Param::from((format!("_{}", stringify!($param_ty)).into(), Some(builtin_ty_v2!($param_ty))))),*],
                FunctionKind::Native(|_, _| unreachable!("interface function should not be called"))
            ),
        ] $($rest)*)
    };

    // Parse optional function names
    (__parse_optional_set [$($name:expr),*]) => {{
        let mut set = UstrSet::default();
        $(set.insert($name.into());)*
        set
    }};
    (__parse_optional_set [$($acc:expr),*] $fn_name:ident ? : ($($param_ty:ident),*) -> $ret_ty:ident; $($rest:tt)*) => {
        builtin_interface!(__parse_optional_set [$($acc,)* stringify!($fn_name)] $($rest)*)
    };
    (__parse_optional_set [$($acc:expr),*] $fn_name:ident ? : ($($param_ty:ident),*); $($rest:tt)*) => {
        builtin_interface!(__parse_optional_set [$($acc,)* stringify!($fn_name)] $($rest)*)
    };
    (__parse_optional_set [$($acc:expr),*] $fn_name:ident : ($($param_ty:ident),*) -> $ret_ty:ident; $($rest:tt)*) => {
        builtin_interface!(__parse_optional_set [$($acc),*] $($rest)*)
    };
    (__parse_optional_set [$($acc:expr),*] $fn_name:ident : ($($param_ty:ident),*); $($rest:tt)*) => {
        builtin_interface!(__parse_optional_set [$($acc),*] $($rest)*)
    };
}

pub type NativeFn = fn(&mut Context, Vec<Value>) -> Result<Value, Exception>;

pub fn register_builtin_module(ctx: &mut Context) {
    ctx.modules
        .new_module("builtin")
        .unwrap()
        .with_interface(builtin_interface![
            "UnitImpl"
            to_base: (num) -> num;
            from_base: (num) -> num;
            display_name?: () -> str;
        ])
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
        }))
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
        }))
        .with_function(builtin_fn_v2!("len", |&ctx, v: any| {
            let len = match v {
                Value::String(s) => s.chars().count(),
                Value::List(l) => l.borrow().len(),
                Value::Tuple(t) => t.len(),
                _ => {
                    return Err(Exception::new(
                        "TypeError",
                        format!("cannot get length of type: {}", v.ty().plain_string(ctx)),
                    )
                    .with_backtrace(ctx.backtrace()))
                }
            };
            Ok(Value::from(len))
        }))
        .with_function(builtin_fn_v2!("typeof", |&ctx, v: any| Ok(v
            .ty()
            .to_string())))
        .with_function(builtin_fn_v2!("unit_cast", |&ctx, v: num, u: unit| {
            let (target_name, target_conv, target_dim) = {
                let target_unit = ctx
                    .active_module()
                    .unwrap()
                    .resolve_unit_suffix(u.into())
                    .map_err(|_| Exception::new("NameError", format!("unknown unit: {}", u)))?;

                // ensure the dimensions are compatible, but dont take the result
                // because we explicitly want to use the right-hand side unit
                Dim::unify(ctx, v.dim.clone(), target_unit.as_dim())?;

                (target_unit.name.raw, target_unit.conversion.clone(), target_unit.as_dim())
            };

            // Convert from source unit to target unit
            // Step 1: Convert source to base units (if needed)
            let base_value = if let Some((source_unit, source_conv)) = &v.dim.unit {
                if source_unit == &target_name {
                    // Same unit, no conversion needed
                    v.number.clone()
                } else {
                    // Convert to base units first
                    source_conv.to_base(ctx, v.number.clone())
                        .map_err(|e| match e {
                            InterpError::Exception(ex) => ex,
                            _ => Exception::new("ValueError", format!("unit conversion error: {:?}", e))
                        })?
                }
            } else {
                // Already in base units (dimensionless or base unit)
                v.number.clone()
            };

            // Step 2: Convert from base units to target unit
            let target_value = target_conv.from_base(ctx, base_value)
                .map_err(|e| match e {
                    InterpError::Exception(ex) => ex,
                    _ => Exception::new("ValueError", format!("unit conversion error: {:?}", e))
                })?;

            let quantity = Quantity::new(target_value, target_dim);
            Ok(Value::Quantity(quantity))
        }))
        .with_function(builtin_fn_v2!("print", |&ctx, ...args| {
            let args = args.into_iter()
                .map(|arg| arg.pretty_string(ctx))
                .collect::<Vec<_>>()
                .join(" ");

            println!("[PRINT] --> {}", args);
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
