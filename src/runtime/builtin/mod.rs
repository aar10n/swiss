use crate::print::{DisplayString, PrettyString};
use crate::source::{SourceSpan, Spanned};
use ustr::Ustr;

pub(super) use super::context::Context;
pub(super) use super::exception::Exception;
pub(super) use super::module::Module;
pub(super) use super::name::{Function, Param};
pub(super) use super::value::{
    CastFrom, CastInto, Dim, Float, Integer, IterValue, Number, Numeric, Quantity, Ty, Tuple,
    VRef, Value, ValueRef,
};
pub(super) use super::{Conversion, IoHandle, ModuleId, UserTy};

#[rustfmt::skip]
macro_rules! builtin_ty_v2 {
    (& $t:ident ?) => { builtin_ty_v2!(& $t) };
    ($t:ident ?) => { builtin_ty_v2!($t) };
    (& $($rest:tt)*) => { crate::runtime::Ty::Ref(Box::new(builtin_ty_v2!($($rest)*))) };
    (int) => { crate::runtime::Ty::Int };
    (float) => { crate::runtime::Ty::Float };
    (num) => { crate::runtime::Ty::Num };
    (any) => { crate::runtime::Ty::Any };
    (str) => { crate::runtime::Ty::Str };
    (bool) => { crate::runtime::Ty::Bool };
    (fn) => { crate::runtime::Ty::Function };
    (iter) => { crate::runtime::Ty::Iter };
    (io) => { crate::runtime::Ty::UserType(ustr::Ustr::from("io")) };
    (file) => { crate::runtime::Ty::UserType(ustr::Ustr::from("file")) };
    (list) => { crate::runtime::Ty::List };
    (tuple[$($t:ident),*]) => { crate::runtime::Ty::Tuple(vec![$(builtin_ty_v2!($t)),*]) };
    (unit) => { crate::runtime::Ty::Unit };
    (ty) => { crate::runtime::Ty::Type };
}

#[rustfmt::skip]
macro_rules! builtin_type_v2 {
    (& $t:ident ?) => { Option<&mut builtin_type_v2!($t)> };
    ($t:ident ?) => { Option<builtin_type_v2!($t)> };
    (& $($rest:tt)*) => { &mut builtin_type_v2!($($rest)*) };
    (any) => { crate::runtime::Value };
    (int) => { crate::runtime::Integer };
    (float) => { crate::runtime::Float };
    (num) => { crate::runtime::Quantity };
    (str) => { String };
    (bool) => { bool };
    (fn) => { crate::runtime::Function };
    (iter) => { crate::runtime::Value };
    (io) => { crate::runtime::IoHandle };
    (file) => { crate::runtime::FileHandle };
    (unit) => { ustr::Ustr };
    (ty) => { crate::runtime::Ty };
    (...) => { Vec<crate::runtime::Value> };
}

#[rustfmt::skip]
macro_rules! builtin_fn_v2 {
    (__params ($($acc:tt)*) ... $p:ident | $($rest:tt)*) => {
        vec![$($acc)* crate::runtime::Param::variadic(stringify!($p).into())]
    };
    (__params ($($acc:tt)*) $p:ident : & $t:ident ? | $($rest:tt)*) => {
        vec![$($acc)* crate::runtime::Param::optional_from(stringify!($p).into(), Some(builtin_ty_v2!(& $t)))]
    };
    (__params ($($acc:tt)*) $p:ident : $t:ident ? | $($rest:tt)*) => {
        vec![$($acc)* crate::runtime::Param::optional_from(stringify!($p).into(), Some(builtin_ty_v2!($t)))]
    };
    (__params ($($acc:tt)*) $p:ident : & $t:ident | $($rest:tt)*) => {
        vec![$($acc)* crate::runtime::Param::from((stringify!($p).into(), Some(builtin_ty_v2!(& $t))))]
    };
    (__params ($($acc:tt)*) $p:ident : $t:ident | $($rest:tt)*) => {
        vec![$($acc)* crate::runtime::Param::from((stringify!($p).into(), Some(builtin_ty_v2!($t))))]
    };
    (__params ($($acc:tt)*) $p:ident : & $t:ident ?, $($rest:tt)*) => {
        builtin_fn_v2!(__params ($($acc)* crate::runtime::Param::optional_from(stringify!($p).into(), Some(builtin_ty_v2!(& $t))),) $($rest)*)
    };
    (__params ($($acc:tt)*) $p:ident : $t:ident ?, $($rest:tt)*) => {
        builtin_fn_v2!(__params ($($acc)* crate::runtime::Param::optional_from(stringify!($p).into(), Some(builtin_ty_v2!($t))),) $($rest)*)
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
    (__closure ($($acc:tt)*) $p:ident : & $t:ident ? | $($rest:tt)*) => {
        |$($acc)* $p: builtin_type_v2!(& $t ?)| -> Result<_, crate::runtime::Exception> { $($rest)* }
    };
    (__closure ($($acc:tt)*) $p:ident : $t:ident ? | $($rest:tt)*) => {
        |$($acc)* $p: builtin_type_v2!($t ?)| -> Result<_, crate::runtime::Exception> { $($rest)* }
    };
    (__closure ($($acc:tt)*) $p:ident : & $t:ident | $($rest:tt)*) => {
        |$($acc)* $p: builtin_type_v2!(& $t)| -> Result<_, crate::runtime::Exception> { $($rest)* }
    };
    (__closure ($($acc:tt)*) $p:ident : $t:ident | $($rest:tt)*) => {
        |$($acc)* $p: builtin_type_v2!($t)| -> Result<_, crate::runtime::Exception> { $($rest)* }
    };
    (__closure ($($acc:tt)*) $p:ident : & $t:ident ?, $($rest:tt)*) => {
        builtin_fn_v2!(__closure ($($acc)* $p: builtin_type_v2!(& $t ?),) $($rest)*)
    };
    (__closure ($($acc:tt)*) $p:ident : $t:ident ?, $($rest:tt)*) => {
        builtin_fn_v2!(__closure ($($acc)* $p: builtin_type_v2!($t ?),) $($rest)*)
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
    (__invoke ($($deferred:tt)*) $res:ident $f:ident ($ctx:ident, $args:expr, $($acc:tt)*) $p:ident : & $t:ident ? | $($rest:tt)*) => {paste::paste!{
        let [< $p _ref_opt >] = crate::runtime::builtin::take_optional_arg::<crate::runtime::ValueRef>($ctx, stringify!($p), $args)?;
        if let Some([< $p _ref >]) = [< $p _ref_opt >] {
            let mut $p = crate::runtime::CastInto::<builtin_type_v2!($t)>::cast($ctx, [< $p _ref >].borrow().clone())?;
            $res = $f($ctx, $($acc)* Some(&mut $p))?;
            [< $p _ref >].set($ctx, $p.into())?;
        } else {
            $res = $f($ctx, $($acc)* None)?;
        }
        $($deferred)*
    }};
    (__invoke ($($deferred:tt)*) $res:ident $f:ident ($ctx:ident, $args:expr, $($acc:tt)*) $p:ident : $t:ident ? | $($rest:tt)*) => {
        let $p = crate::runtime::builtin::take_optional_arg::<builtin_type_v2!($t)>($ctx, stringify!($p), $args)?;
        $res = $f($ctx, $($acc)* $p)?;
        $($deferred)*
    };
    (__invoke ($($deferred:tt)*) $res:ident $f:ident ($ctx:ident, $args:expr, $($acc:tt)*) $p:ident : & $t:ident | $($rest:tt)*) => {paste::paste!{
        let [< $p _ref >] = crate::runtime::builtin::take_arg::<crate::runtime::ValueRef>($ctx, stringify!($p), $args)?;
        let mut $p = crate::runtime::CastInto::<builtin_type_v2!($t)>::cast($ctx, [< $p _ref >].borrow().clone())?;
        $res = $f($ctx, $($acc)* &mut $p)?;
        [<$p _ref>].set($ctx, $p.into())?;
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
        builtin_fn_v2!(__invoke ($($deferred)* [<$p _ref>].set($ctx, $p.into())?;) $res $f ($ctx, $args, $($acc)* &mut $p,) $($rest)*)
    }};
    (__invoke ($($deferred:tt)*) $res:ident $f:ident ($ctx:ident, $args:expr, $($acc:tt)*) $p:ident : & $t:ident ?, $($rest:tt)*) => {paste::paste!{
        let [< $p _ref_opt >] = crate::runtime::builtin::take_optional_arg::<crate::runtime::ValueRef>($ctx, stringify!($p), $args)?;
        if let Some([< $p _ref >]) = [< $p _ref_opt >] {
            let mut $p = crate::runtime::CastInto::<builtin_type_v2!($t)>::cast($ctx, [< $p _ref >].borrow().clone())?;
            builtin_fn_v2!(__invoke ($($deferred)* [<$p _ref>].set($ctx, $p.into())?;) $res $f ($ctx, $args, $($acc)* Some(&mut $p),) $($rest)*)
        } else {
            builtin_fn_v2!(__invoke ($($deferred)*) $res $f ($ctx, $args, $($acc)* None,) $($rest)*)
        }
    }};
    (__invoke ($($deferred:tt)*) $res:ident $f:ident ($ctx:ident, $args:expr, $($acc:tt)*) $p:ident : $t:ident, $($rest:tt)*) => {
        let $p = crate::runtime::builtin::take_arg::<builtin_type_v2!($t)>($ctx, stringify!($p), $args)?;
        builtin_fn_v2!(__invoke ($($deferred)*) $res $f ($ctx, $args, $($acc)* $p,) $($rest)*)
    };
    (__invoke ($($deferred:tt)*) $res:ident $f:ident ($ctx:ident, $args:expr, $($acc:tt)*) $p:ident : $t:ident ?, $($rest:tt)*) => {
        let $p = crate::runtime::builtin::take_optional_arg::<builtin_type_v2!($t)>($ctx, stringify!($p), $args)?;
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

    ($name:tt, |&$ctx:ident| $body:block) => {{
        let params = Vec::new();
        crate::runtime::Function::builtin($name, params, |ctx, _args| {
            let f = |$ctx: &mut crate::runtime::Context| -> Result<_, Exception> { $body };
            let result: _;
            result = f(ctx)?;
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
                FunctionKind::Native(|_, _| unreachable!("interface function should not be called")),
                None,
                false
            ),
        ] $($rest)*)
    };
    // Optional function without return type
    (__parse_all_functions [$($acc:tt)*] $fn_name:ident ? : ($($param_ty:ident),*); $($rest:tt)*) => {
        builtin_interface!(__parse_all_functions [$($acc)*
            Function::new(
                Spanned::new(stringify!($fn_name).into(), SourceSpan::default()),
                vec![$(Param::from((format!("_{}", stringify!($param_ty)).into(), Some(builtin_ty_v2!($param_ty))))),*],
                FunctionKind::Native(|_, _| unreachable!("interface function should not be called")),
                None,
                false
            ),
        ] $($rest)*)
    };
    // Required function with return type
    (__parse_all_functions [$($acc:tt)*] $fn_name:ident : ($($param_ty:ident),*) -> $ret_ty:ident; $($rest:tt)*) => {
        builtin_interface!(__parse_all_functions [$($acc)*
            Function::new(
                Spanned::new(stringify!($fn_name).into(), SourceSpan::default()),
                vec![$(Param::from((format!("_{}", stringify!($param_ty)).into(), Some(builtin_ty_v2!($param_ty))))),*],
                FunctionKind::Native(|_, _| unreachable!("interface function should not be called")),
                None,
                false
            ),
        ] $($rest)*)
    };
    // Required function without return type
    (__parse_all_functions [$($acc:tt)*] $fn_name:ident : ($($param_ty:ident),*); $($rest:tt)*) => {
        builtin_interface!(__parse_all_functions [$($acc)*
            Function::new(
                Spanned::new(stringify!($fn_name).into(), SourceSpan::default()),
                vec![$(Param::from((format!("_{}", stringify!($param_ty)).into(), Some(builtin_ty_v2!($param_ty))))),*],
                FunctionKind::Native(|_, _| unreachable!("interface function should not be called")),
                None,
                false
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

mod collections;
mod encoding;
mod env;
mod fs;
mod io;
mod math;
mod operators;
mod text;
mod units;

pub type NativeFn = fn(&mut Context, Vec<Value>) -> Result<Value, Exception>;

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

pub(crate) fn take_optional_arg<T: CastFrom<Value>>(
    ctx: &Context,
    _param: &str,
    args: &mut Vec<Value>,
) -> Result<Option<T>, Exception> {
    if args.is_empty() {
        return Ok(None);
    }

    let value = args.remove(0);
    if matches!(value, Value::Empty) {
        return Ok(None);
    }

    T::cast(ctx, value).map(Some)
}

pub(crate) fn take_varargs(ctx: &Context, args: &mut Vec<Value>) -> Result<Vec<Value>, Exception> {
    Ok(args.drain(..).collect())
}

pub fn register_builtin_module(ctx: &mut Context) {
    ctx.modules
        .new_module("builtin")
        .unwrap()
        .with_type("io")
        .with_type("file")
        .with_function(builtin_fn_v2!("dir", |&ctx, v: any?| {
            let mut names: Vec<String> = Vec::new();

            let mut collect_module_names = |module: &Module, include_opened: bool| {
                names.extend(module.names.iter_names().map(|name| name.to_string()));
                names.extend(module.types.keys().map(|name| name.to_string()));
                names.extend(
                    module
                        .module_aliases
                        .keys()
                        .map(|name| name.to_string()),
                );
                names.extend(
                    ctx.modules
                        .child_module_names(module.id)
                        .into_iter()
                        .map(|name| name.to_string()),
                );
                if include_opened {
                    for module_id in &module.opened {
                        names.extend(
                            ctx.modules[*module_id]
                                .names
                                .iter_names()
                                .map(|name| name.to_string()),
                        );
                        names.extend(
                            ctx.modules[*module_id]
                                .types
                                .keys()
                                .map(|name| name.to_string()),
                        );
                    }
                }
            };

            let value = match v {
                Some(value) => value,
                None => {
                    if let Some(module) = ctx.active_module() {
                        let in_function = ctx.call_stack_len() > 1;
                        if !in_function {
                            collect_module_names(module, true);
                        }
                        for scope in ctx.local_scopes() {
                            names.extend(scope.vars().keys().map(|name| name.to_string()));
                        }
                    }
                    names.sort();
                    names.dedup();
                    let values = names.into_iter().map(Value::String).collect();
                    return Ok(Value::list(values));
                }
            };

            let value = match value {
                Value::Ref(r) => r.borrow().clone(),
                other => other,
            };

            match value {
                Value::UserType(user_ty) => match user_ty {
                    UserTy::Handle(handle) => {
                        if handle.tag() == Ustr::from("module") {
                            let module_id =
                                handle.borrow::<ModuleId>(Ustr::from("module"), ctx)?;
                            let module = &ctx.modules[*module_id];
                            collect_module_names(module, false);
                        } else if let Some(module) = ctx.active_module() {
                            if let Ok(ty) = ctx.modules.resolve_type_in(
                                module.id,
                                Spanned::new(handle.tag(), SourceSpan::default()),
                            ) {
                                names.extend(
                                    ty.method_names()
                                        .into_iter()
                                        .map(|name| name.to_string()),
                                );
                            }
                        }
                    }
                },
                _ => {}
            }

            names.sort();
            names.dedup();
            let values = names.into_iter().map(Value::String).collect();
            Ok(Value::list(values))
        }))
        .with_function(builtin_fn_v2!("typeof", |&ctx, v: any| {
            let value = match v {
                Value::Ref(r) => r.borrow().clone(),
                other => other,
            };
            let name = match value {
                Value::Ty(ty) => ty.to_string(),
                other => other.ty().to_string(),
            };
            Ok(name)
        }))
        .with_function(builtin_fn_v2!("bool_new", |&_ctx| {
            Ok(Value::Boolean(false))
        }))
        .with_function(builtin_fn_v2!("int_new", |&_ctx| {
            Ok(Value::from(Quantity::from(Number::from(Integer::from(0)))))
        }))
        .with_function(builtin_fn_v2!("float_new", |&ctx| {
            Ok(Value::from(Quantity::from(Number::from(Float::with_val(
                ctx.config.float_precision,
                0,
            )))))
        }))
        .with_function(builtin_fn_v2!("str_new", |&_ctx| {
            Ok(Value::String(String::new()))
        }))
        .with_function(builtin_fn_v2!("iter_new", |&ctx, v: iter| {
            let iter = v.try_into_iter(ctx)?;
            Ok(Value::Iter(IterValue::new(iter)))
        }))
        .with_function(builtin_fn_v2!("list_new", |&ctx, v: iter| {
            let mut iter = v.try_into_iter(ctx)?;
            let mut items = Vec::new();
            while let Some(value) = iter.next(ctx)? {
                items.push(value);
            }
            Ok(Value::list(items))
        }))
        .with_function(builtin_fn_v2!("tuple_new", |&ctx, v: iter| {
            let mut iter = v.try_into_iter(ctx)?;
            let mut items = Vec::new();
            while let Some(value) = iter.next(ctx)? {
                items.push(value);
            }
            let boxed = items.into_iter().map(Box::new).collect();
            Ok(Value::Tuple(Tuple::new(smallvec::SmallVec::from_vec(
                boxed,
            ))))
        }))
        .with_function(builtin_fn_v2!("object_new", |&ctx, v: iter| {
            let mut iter = v.try_into_iter(ctx)?;
            let mut items = Vec::new();
            while let Some(value) = iter.next(ctx)? {
                let value = match value {
                    Value::Ref(r) => r.borrow().clone(),
                    other => other,
                };
                let tuple = match value {
                    Value::Tuple(items) => items,
                    other => {
                        return Err(Exception::new(
                            "TypeError",
                            format!(
                                "object expects tuples of (key, value), found {}",
                                other.ty().pretty_string(ctx)
                            ),
                        )
                        .with_backtrace(ctx.backtrace()))
                    }
                };
                if tuple.len() != 2 {
                    return Err(Exception::new(
                        "TypeError",
                        "object expects tuples of (key, value)".to_string(),
                    )
                    .with_backtrace(ctx.backtrace()));
                }
                let key_value = tuple.get(0).expect("tuple length checked");
                let key = match &**key_value {
                    Value::String(s) => s.clone(),
                    Value::Ref(r) => match r.borrow().clone() {
                        Value::String(s) => s,
                        other => {
                            return Err(Exception::new(
                                "TypeError",
                                format!(
                                    "object keys must be strings, found {}",
                                    other.ty().pretty_string(ctx)
                                ),
                            )
                            .with_backtrace(ctx.backtrace()))
                        }
                    },
                    other => {
                        return Err(Exception::new(
                            "TypeError",
                            format!(
                                "object keys must be strings, found {}",
                                other.ty().pretty_string(ctx)
                            ),
                        )
                        .with_backtrace(ctx.backtrace()))
                    }
                };
                let value = tuple.get(1).expect("tuple length checked");
                items.push((Ustr::from(key.as_str()), value.as_ref().clone()));
            }
            Ok(Value::object(items))
        }))
        .with_function(builtin_fn_v2!("unit_new", |&ctx, name: str| {
            let module_id = ctx
                .active_module()
                .map(|module| module.id)
                .ok_or_else(|| {
                    Exception::new("RuntimeError", "no active module".to_string())
                        .with_backtrace(ctx.backtrace())
                })?;
            let unit = ctx
                .modules
                .resolve_unit_suffix_in(
                    module_id,
                    Spanned::new(Ustr::from(name.as_str()), SourceSpan::default()),
                )
                .map_err(|_| {
                    Exception::new("NameError", format!("unknown unit: {}", name))
                        .with_backtrace(ctx.backtrace())
                })?;
            Ok(Value::Unit(unit.name.raw))
        }))
        .with_function(builtin_fn_v2!("to_string", |&ctx, v: any| {
            Ok(v.plain_string(ctx))
        }))
        .with_function(builtin_fn_v2!("error", |&ctx, msg: str?| {
            let message = msg.unwrap_or_default();
            Err::<Value, Exception>(
                Exception::new("Error", message).with_backtrace(ctx.backtrace()),
            )
        }));

    collections::register(ctx);
    encoding::register(ctx);
    env::register(ctx);
    fs::register(ctx);
    io::register(ctx);
    math::register(ctx);
    operators::register(ctx);
    text::register(ctx);
    units::register(ctx);
}
