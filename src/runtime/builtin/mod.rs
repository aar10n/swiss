pub(super) use super::context::Context;
pub(super) use super::exception::Exception;
pub(super) use super::module::Module;
pub(super) use super::name::{Function, Param};
pub(super) use super::value::{
    CastFrom, CastInto, Dim, Float, Integer, Number, Numeric, Quantity, Ty, VRef, Value, ValueRef,
};
pub(super) use super::{Conversion, IoHandle};

#[rustfmt::skip]
macro_rules! builtin_ty_v2 {
    (& $($rest:tt)*) => { crate::runtime::Ty::Ref(Box::new(builtin_ty_v2!($($rest)*))) };
    (int) => { crate::runtime::Ty::Int };
    (float) => { crate::runtime::Ty::Float };
    (num) => { crate::runtime::Ty::Num };
    (any) => { crate::runtime::Ty::Any };
    (str) => { crate::runtime::Ty::Str };
    (bool) => { crate::runtime::Ty::Bool };
    (fn) => { crate::runtime::Ty::Function };
    (io) => { crate::runtime::Ty::Io };
    (list) => { crate::runtime::Ty::List };
    (tuple[$($t:ident),*]) => { crate::runtime::Ty::Tuple(vec![$(builtin_ty_v2!($t)),*]) };
    (unit) => { crate::runtime::Ty::Unit };
    (ty) => { crate::runtime::Ty::Type };
}

#[rustfmt::skip]
macro_rules! builtin_type_v2 {
    (& $($rest:tt)*) => { &mut builtin_type_v2!($($rest)*) };
    (any) => { crate::runtime::Value };
    (int) => { crate::runtime::Integer };
    (float) => { crate::runtime::Float };
    (num) => { crate::runtime::Quantity };
    (str) => { String };
    (bool) => { bool };
    (fn) => { crate::runtime::Function };
    (io) => { crate::runtime::IoHandle };
    (unit) => { ustr::Ustr };
    (ty) => { crate::runtime::Ty };
    (...) => { Vec<crate::runtime::Value> };
}

#[rustfmt::skip]
macro_rules! builtin_fn_v2 {
    (__params ($($acc:tt)*) ... $p:ident | $($rest:tt)*) => {
        vec![$($acc)* crate::runtime::Param::variadic(stringify!($p).into())]
    };
    (__params ($($acc:tt)*) $p:ident : & $t:ident | $($rest:tt)*) => {
        vec![$($acc)* crate::runtime::Param::from((stringify!($p).into(), Some(builtin_ty_v2!(& $t))))]
    };
    (__params ($($acc:tt)*) $p:ident : $t:ident | $($rest:tt)*) => {
        vec![$($acc)* crate::runtime::Param::from((stringify!($p).into(), Some(builtin_ty_v2!($t))))]
    };
    (__params ($($acc:tt)*) $p:ident : & $t:ident, $($rest:tt)*) => {
        builtin_fn_v2!(__params ($($acc)* crate::runtime::Param::from((stringify!($p).into(), Some(builtin_ty_v2!(& $t)))),) $($rest)*)
    };
    (__params ($($acc:tt)*) $p:ident : $t:ident, $($rest:tt)*) => {
        builtin_fn_v2!(__params ($($acc)* crate::runtime::Param::from((stringify!($p).into(), Some(builtin_ty_v2!($t)))),) $($rest)*)
    };

    (__closure ($($acc:tt)*) ... $p:ident | $($rest:tt)*) => {
        |$($acc)* $p: builtin_type_v2!(...)| -> Result<_, Exception> { $($rest)* }
    };
    (__closure ($($acc:tt)*) $p:ident : & $t:ident | $($rest:tt)*) => {
        |$($acc)* $p: builtin_type_v2!(& $t)| -> Result<_, crate::runtime::Exception> { $($rest)* }
    };
    (__closure ($($acc:tt)*) $p:ident : $t:ident | $($rest:tt)*) => {
        |$($acc)* $p: builtin_type_v2!($t)| -> Result<_, crate::runtime::Exception> { $($rest)* }
    };
    (__closure ($($acc:tt)*) $p:ident : & $t:ident, $($rest:tt)*) => {
        builtin_fn_v2!(__closure ($($acc)* $p: builtin_type_v2!(& $t),) $($rest)*)
    };
    (__closure ($($acc:tt)*) $p:ident : $t:ident, $($rest:tt)*) => {
        builtin_fn_v2!(__closure ($($acc)* $p: builtin_type_v2!($t),) $($rest)*)
    };

    (__invoke ($($deferred:tt)*) $res:ident $f:ident ($ctx:ident, $args:expr, $($acc:tt)*) ... $p:ident | $($rest:tt)*) => {
        let $p = crate::runtime::builtin::take_varargs($ctx, $args)?;
        $res = $f($ctx, $($acc)* $p)?;
        $($deferred)*
    };
    (__invoke ($($deferred:tt)*) $res:ident $f:ident ($ctx:ident, $args:expr, $($acc:tt)*) $p:ident : & $t:ident | $($rest:tt)*) => {paste::paste!{
        let [< $p _ref >] = crate::runtime::builtin::take_arg::<crate::runtime::ValueRef>($ctx, stringify!($p), $args)?;
        let mut $p = crate::runtime::CastInto::<builtin_type_v2!($t)>::cast($ctx, [< $p _ref >].borrow().clone())?;
        $res = $f($ctx, $($acc)* &mut $p)?;
        [<$p _ref>].set($p.into());
        $($deferred)*
    }};
    (__invoke ($($deferred:tt)*) $res:ident $f:ident ($ctx:ident, $args:expr, $($acc:tt)*) $p:ident : $t:ident | $($rest:tt)*) => {
        let $p = crate::runtime::builtin::take_arg::<builtin_type_v2!($t)>($ctx, stringify!($p), $args)?;
        $res = $f($ctx, $($acc)* $p)?;
        $($deferred)*
    };
    (__invoke ($($deferred:tt)*) $res:ident $f:ident ($ctx:ident, $args:expr, $($acc:tt)*) $p:ident : & $t:ident, $($rest:tt)*) => {paste::paste!{
        let [< $p _ref >] = crate::runtime::builtin::take_arg::<crate::runtime::ValueRef>($ctx, stringify!($p), $args)?;
        let mut $p = crate::runtime::CastInto::<builtin_type_v2!($t)>::cast($ctx, [< $p _ref >].borrow().clone())?;
        builtin_fn_v2!(__invoke ($($deferred)* [<$p _ref>].set($p.into());) $res $f ($ctx, $args, $($acc)* &mut $p,) $($rest)*)
    }};
    (__invoke ($($deferred:tt)*) $res:ident $f:ident ($ctx:ident, $args:expr, $($acc:tt)*) $p:ident : $t:ident, $($rest:tt)*) => {
        let $p = crate::runtime::builtin::take_arg::<builtin_type_v2!($t)>($ctx, stringify!($p), $args)?;
        builtin_fn_v2!(__invoke ($($deferred)*) $res $f ($ctx, $args, $($acc)* $p,) $($rest)*)
    };

    ($name:tt, |&$ctx:ident, $($rest:tt)*) => {{
        let params = builtin_fn_v2!(__params () $($rest)*);
        crate::runtime::Function::builtin($name, params, |ctx, mut args| {
            let f = builtin_fn_v2!(__closure ($ctx: &mut crate::runtime::Context, ) $($rest)*);
            let result: _;
            builtin_fn_v2!(__invoke () result f (ctx, &mut args,) $($rest)*);
            Ok(crate::runtime::Value::from(result))
        })
    }};
}

#[rustfmt::skip]
macro_rules! builtin_interface {
    // Main entry point - parse all function declarations
    [$name:literal $($rest:tt)*] => {{
        use crate::ast::Spanned;
        use crate::runtime::interface::Interface;
        use crate::runtime::name::{Function, FunctionKind, Param};
        use crate::source::SourceSpan;
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

mod helpers;
mod operators;
mod math;
mod collections;
mod units;
mod io;

pub(crate) use helpers::{take_arg, take_varargs};

pub type NativeFn = fn(&mut Context, Vec<Value>) -> Result<Value, Exception>;

pub fn register_builtin_module(ctx: &mut Context) {
    let module = ctx.modules.new_module("builtin").unwrap();

    operators::register(module);
    math::register(module);
    collections::register(module);
    helpers::register(module);
    units::register(module);
    io::register(module);
}
