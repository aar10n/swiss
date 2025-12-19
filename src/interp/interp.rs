use super::{InterpError, InterpResult, NameError, TypeError, Value};

use crate::ast::*;
use crate::diag::IntoError;
use crate::id::VarId;
use crate::interp::{Exception, VRef};
use crate::print::{PrettyPrint, PrettyString};
use crate::runtime::{
    self as rt, CastInto, Constant, Context, ContextProvider, Function, LRValue, LocalScope,
    StackFrame, ValueRef,
};
use crate::source::{SourceSpan, Spanned};

use either::{Either, Left, Right};
use rug::{Float, Integer};
use smallvec::{smallvec, SmallVec};
use std::cell::{Ref, RefCell};
use std::rc::Rc;
use ustr::Ustr;

const TABWIDTH: &str = "    ";

macro_rules! trace {
    ($self:ident, $intrp:expr, $msg:expr, $expr:expr) => {{
        let tab = TABWIDTH.repeat($intrp.trace_level);
        if $intrp.trace_on {
            eprintln!(
                "[TRACE] {{{}}} {tab}{}: {}",
                $intrp.trace_level,
                $msg,
                $self.pretty_string(&())
            );
        }

        $intrp.trace_level += 1;
        let result = $expr;
        $intrp.trace_level -= 1;

        if $intrp.trace_on {
            eprintln!(
                "[TRACE] {{{}}} {tab}{}: {}",
                $intrp.trace_level,
                $msg,
                result.pretty_string($intrp.ctx)
            );
        }
        result
    }};
}

macro_rules! no_trace {
    (self, $intrp:expr, $msg:expr, $expr:expr) => {
        $expr
    };
}

// MARK: Interpreter

pub struct Interpreter<'ctx> {
    pub ctx: &'ctx mut Context,

    pub(crate) trace_on: bool,
    trace_level: usize,
}

impl<'ctx> Interpreter<'ctx> {
    pub fn new(ctx: &'ctx mut Context) -> Interpreter<'ctx> {
        Interpreter {
            ctx,

            trace_on: std::env::var("TRACE_INTERP").is_ok(),
            trace_level: 0,
        }
    }

    pub fn active_module(&mut self) -> &mut rt::Module {
        self.ctx.active_module_mut().unwrap()
    }

    fn trace_debug(&self, msg: &str) {
        let tab = TABWIDTH.repeat(self.trace_level);
        if self.trace_on {
            eprintln!("[TRACE] {{{}}} {tab}{}", self.trace_level, msg);
        }
    }
}

impl<'ctx> Interpreter<'ctx> {
    pub fn call_by_id(&mut self, func_id: VarId, values: Vec<Value>) -> InterpResult<Value> {
        let f = self
            .active_module()
            .get_unnamed_function(func_id)
            .ok_or_else(|| {
                Exception::new(
                    "RuntimeError",
                    format!("undefined function with id {:?}", func_id),
                )
            })?
            .clone();

        use rt::FunctionKind;
        self.trace_debug(&format!(
            "call_function {}({} args)",
            f.name.raw,
            values.len()
        ));

        let is_variadic = f.params.last().map_or(false, |p| p.is_variadic());
        let expected_fixed_args = if is_variadic {
            f.params.len() - 1
        } else {
            f.params.len()
        };

        // Check argument count for non-variadic functions
        if !is_variadic && values.len() != f.params.len() {
            return Err(InterpError::TypeError(TypeError {
                expected: Some(format!(
                    "function {} expects {} argument(s)",
                    f.name.raw,
                    f.params.len()
                )),
                found: SourceSpan::default().into_spanned(format!("{} argument(s)", values.len())),
                context: None,
            }));
        }

        // Check minimum argument count for variadic functions
        if is_variadic && values.len() < expected_fixed_args {
            return Err(InterpError::TypeError(TypeError {
                expected: Some(format!(
                    "function {} expects at least {} argument(s)",
                    f.name.raw, expected_fixed_args
                )),
                found: SourceSpan::default().into_spanned(format!("{} argument(s)", values.len())),
                context: None,
            }));
        }

        // Apply type coercion for fixed parameters
        let mut coerced_values = Vec::new();
        for (i, param) in f.params[..expected_fixed_args].iter().enumerate() {
            let val = values[i].clone();
            let ty = param.ty.clone().map_or(rt::Ty::Any, |ty| ty.raw);

            // Type coercion (except for references which are already values)
            if !ty.is_ref() && ty != rt::Ty::Unit {
                coerced_values.push(rt::coerce::to_ty(self.ctx, val, ty));
            } else {
                coerced_values.push(val);
            }
        }

        // Handle variadic arguments
        if is_variadic {
            let variadic_args: Vec<Value> = values[expected_fixed_args..].to_vec();

            // For native functions, spread the variadic args directly
            // For source functions, wrap them in a list
            if matches!(&f.kind, rt::FunctionKind::Native(_)) {
                coerced_values.extend(variadic_args);
            } else {
                coerced_values.push(Value::list(variadic_args));
            }
        }

        // Create a dummy call site span since we don't have source information
        let call_site = SourceSpan::default();

        // Invoke the function
        let result = match &f.kind {
            FunctionKind::Native(builtin) => {
                let frame = StackFrame::new(f.name, call_site);
                let scope = LocalScope::new();
                Context::with_fn_call(self, frame, scope, |intrp| {
                    builtin(intrp.ctx, coerced_values).map_err(InterpError::from)
                })
            }
            FunctionKind::Source(block) => {
                let frame = StackFrame::new(f.name, call_site);
                let scope = LocalScope::from(
                    f.params
                        .iter()
                        .map(|p| p.name.raw)
                        .zip(coerced_values.into_iter()),
                );

                Context::with_fn_call(self, frame, scope, |intrp| {
                    Interp::<Value>::eval(block, intrp)
                })
            }
        };

        match result {
            Ok(value) => Ok(value),
            Err(InterpError::Return(value)) => Ok(value),
            Err(err) => Err(err),
        }
    }

    fn invoke(
        &mut self,
        f: &Function,
        args: ListNode<Expr>,
        call_site: SourceSpan,
    ) -> InterpResult<Value> {
        use rt::FunctionKind;
        self.trace_debug(&format!(
            "invoke function {}({})",
            f.name.raw,
            args.pretty_string(&())
        ));

        let is_variadic = f.params.last().map_or(false, |p| p.is_variadic());
        let check_min_args = if is_variadic {
            f.params.len() - 1
        } else {
            f.params.len()
        };

        // check that the minimim number of fixed arguments are provided
        let mut values = vec![];
        let mut pos = args.start_pos() + 1;
        for (i, param) in f.params[..check_min_args].iter().enumerate() {
            let arg = args.get(i).ok_or_else(|| {
                TypeError::mismatch(
                    format!(
                        "function {} expects {} argument(s)",
                        f.name.raw,
                        f.params.len(),
                    ),
                    pos.as_span().into_spanned(format!("expected argument")),
                )
            })?;

            // Check if splat is used for non-variadic parameter (error)
            if let ExprKind::Splat(_) = &arg.kind {
                return Err(TypeError::mismatch(
                    "regular argument".to_string(),
                    arg.span()
                        .into_spanned("splat can only be used for variadic parameters".to_string()),
                )
                .into());
            }

            let ty = param.ty.clone().map_or(rt::Ty::Any, |ty| ty.raw);
            if ty.is_ref() {
                let vref = Interp::<ValueRef>::eval(arg, self)?;
                values.push(vref.into_value());
            } else if ty == rt::Ty::Unit {
                // Allow either bare unit identifiers (e.g. `ms`) or any expression that
                // evaluates to a unit value (e.g. a variable holding a unit).
                let val = match &arg.kind {
                    // For a simple identifier/path, prefer a bound variable/constant if it exists;
                    // otherwise treat it as a unit literal for backwards compatibility.
                    ExprKind::Ident(ident) => {
                        if let Some(vref) = self
                            .ctx
                            .local_scopes()
                            .iter()
                            .rev()
                            .find_map(|scope| scope.get(ident.raw))
                        {
                            vref.get()
                        } else if let Ok(constant) = self
                            .ctx
                            .active_module()
                            .unwrap()
                            .resolve_constant(ident.as_spanned_ustr())
                        {
                            constant.value.get()
                        } else {
                            Value::Unit(ident.raw)
                        }
                    }
                    ExprKind::Path(path) if path.parts.len() == 1 => {
                        let ident = &path.parts[0];
                        if let Some(vref) = self
                            .ctx
                            .local_scopes()
                            .iter()
                            .rev()
                            .find_map(|scope| scope.get(ident.raw))
                        {
                            vref.get()
                        } else if let Ok(constant) = self
                            .ctx
                            .active_module()
                            .unwrap()
                            .resolve_constant(ident.as_spanned_ustr())
                        {
                            constant.value.get()
                        } else {
                            Value::Unit(ident.raw)
                        }
                    }
                    _ => Interp::<Value>::eval(arg, self)?,
                };

                // Ensure the argument is a unit value.
                let unit_value = match val {
                    Value::Unit(u) => Value::Unit(u),
                    Value::Ref(r) => match r.borrow().clone() {
                        Value::Unit(u) => Value::Unit(u),
                        other => {
                            let u = rt::CastInto::<ustr::Ustr>::cast(self.ctx, other)?;
                            Value::Unit(u)
                        }
                    },
                    other => {
                        let u = rt::CastInto::<ustr::Ustr>::cast(self.ctx, other)?;
                        Value::Unit(u)
                    }
                };

                values.push(unit_value);
            } else {
                let val = Interp::<Value>::eval(arg, self)?;
                values.push(rt::coerce::to_ty(self.ctx, val, ty));
            }
            pos = arg.span().end_pos();
        }

        if is_variadic {
            // push the remaining arguments into the variadic parameter
            let mut variadic = vec![];
            for arg in args.iter().skip(check_min_args) {
                // Check if this is a splat expression
                if let ExprKind::Splat(inner) = &arg.kind {
                    // Evaluate the inner expression and expand it
                    let inner_val = inner.eval(self)?;
                    match inner_val {
                        Value::List(list) => {
                            // Expand the list into individual arguments
                            for item in list.borrow().iter() {
                                variadic.push(item.clone());
                            }
                        }
                        Value::Tuple(tuple) => {
                            // Expand the tuple into individual arguments
                            for item in tuple.iter() {
                                variadic.push((*item.clone()).clone());
                            }
                        }
                        _ => {
                            return Err(TypeError::mismatch(
                                "list or tuple".to_string(),
                                arg.span().into_spanned(
                                    "splat can only be applied to lists or tuples".to_string(),
                                ),
                            )
                            .into());
                        }
                    }
                } else {
                    variadic.push(arg.eval(self)?);
                }
            }

            // For native functions, we need to pass the variadic args directly as a Vec
            // For source functions, we need to wrap them in a list
            if matches!(&f.kind, rt::FunctionKind::Native(_)) {
                // Native functions expect the variadic args to be spread into the values vector
                values.extend(variadic);
            } else {
                // Source functions expect a single list value containing all variadic args
                values.push(Value::list(variadic));
            }
        } else {
            // Check if any argument uses splat in non-variadic function
            for arg in args.iter() {
                if let ExprKind::Splat(_) = &arg.kind {
                    return Err(TypeError::mismatch(
                        "regular argument".to_string(),
                        arg.span().into_spanned(
                            "splat can only be used for variadic parameters".to_string(),
                        ),
                    )
                    .into());
                }
            }

            if args.len() > f.params.len() {
                return Err(TypeError::mismatch(
                    format!(
                        "function {} expects {} argument(s)",
                        f.name.raw,
                        f.params.len(),
                    ),
                    args[values.len()]
                        .span()
                        .into_spanned(format!("unexpected")),
                )
                .into());
            }
        }

        // invoke the function
        let result = match &f.kind {
            FunctionKind::Native(builtin) => {
                let frame = StackFrame::new(f.name, call_site);
                let scope = LocalScope::new();
                Context::with_fn_call(self, frame, scope, |intrp| {
                    builtin(intrp.ctx, values).map_err(InterpError::from)
                })
            }
            FunctionKind::Source(block) => {
                let frame = StackFrame::new(f.name, call_site);
                let scope =
                    LocalScope::from(f.params.iter().map(|p| p.name.raw).zip(values.into_iter()));

                Context::with_fn_call(self, frame, scope, |intrp| {
                    Interp::<Value>::eval(block, intrp)
                })
            }
        };

        match result {
            Ok(value) => Ok(value),
            Err(InterpError::Return(value)) => Ok(value),
            Err(err) => Err(err),
        }
    }

    /// Invoke a function using already-evaluated argument values.
    pub fn invoke_with_values(
        &mut self,
        f: &Function,
        mut values: Vec<Value>,
        call_site: SourceSpan,
    ) -> InterpResult<Value> {
        use rt::FunctionKind;

        let is_variadic = f.params.last().map_or(false, |p| p.is_variadic());
        let expected_fixed_args = if is_variadic {
            f.params.len() - 1
        } else {
            f.params.len()
        };

        if !is_variadic && values.len() != f.params.len() {
            return Err(InterpError::TypeError(TypeError {
                expected: Some(format!(
                    "function {} expects {} argument(s)",
                    f.name.raw,
                    f.params.len(),
                )),
                found: call_site.into_spanned(format!("found {}", values.len())),
                context: None,
            }));
        }
        if is_variadic && values.len() < expected_fixed_args {
            return Err(InterpError::TypeError(TypeError {
                expected: Some(format!(
                    "function {} expects at least {} argument(s)",
                    f.name.raw, expected_fixed_args,
                )),
                found: call_site.into_spanned(format!("found {}", values.len())),
                context: None,
            }));
        }

        let mut coerced_values = Vec::new();
        for (i, param) in f.params.iter().enumerate() {
            if param.is_variadic() {
                let rest = values.split_off(i);
                if matches!(&f.kind, FunctionKind::Native(_)) {
                    coerced_values.extend(rest);
                } else {
                    coerced_values.push(Value::List(VRef::new(rest)));
                }
                break;
            }

            let mut v = values.get(i).cloned().ok_or_else(|| {
                InterpError::TypeError(TypeError {
                    expected: Some(format!("function {} expects argument", f.name.raw)),
                    found: call_site.into_spanned("missing argument".to_string()),
                    context: None,
                })
            })?;

            let ty = param.ty.clone().map_or(rt::Ty::Any, |ty| ty.raw);
            if ty.is_ref() {
                match v {
                    Value::Ref(_) => coerced_values.push(v),
                    _ => {
                        return Err(InterpError::TypeError(TypeError {
                            expected: Some("reference".to_string()),
                            found: call_site.into_spanned(v.ty().pretty_string(self.ctx)),
                            context: Some(param.name.to_string_inner()),
                        }))
                    }
                }
            } else {
                coerced_values.push(rt::coerce::to_ty(self.ctx, v, ty));
            }
        }

        let result = match &f.kind {
            FunctionKind::Native(builtin) => {
                builtin(self.ctx, coerced_values).map_err(InterpError::from)
            }
            FunctionKind::Source(body) => {
                let frame = StackFrame::new(f.name.clone(), call_site);
                let scope = LocalScope::from(
                    f.params
                        .iter()
                        .map(|p| p.name.raw)
                        .zip(coerced_values.into_iter()),
                );

                Context::with_fn_call(self, frame, scope, |intrp| {
                    Interp::<Value>::eval(body, intrp)
                })
            }
        };

        match result {
            Ok(value) => Ok(value),
            Err(InterpError::Return(value)) => Ok(value),
            Err(err) => Err(err),
        }
    }
}

/// Convenience for calling a function value from builtin code.
pub fn call_function(
    ctx: &mut Context,
    f: Function,
    values: Vec<Value>,
) -> Result<Value, Exception> {
    let mut intrp = Interpreter::new(ctx);
    match intrp.invoke_with_values(&f, values, SourceSpan::default()) {
        Ok(v) => Ok(v),
        Err(InterpError::Return(v)) => Ok(v),
        Err(InterpError::Exception(e)) => Err(e),
        Err(other) => {
            Err(Exception::new("RuntimeError", other.to_string()).with_backtrace(ctx.backtrace()))
        }
    }
}

impl ContextProvider for Interpreter<'_> {
    fn context(&self) -> &Context {
        self.ctx
    }

    fn context_mut(&mut self) -> &mut Context {
        self.ctx
    }
}

//
// MARK: Traits
//

/// A trait for nodes that can be evaluated by an `Interpreter`.
pub(super) trait Interp<'ctx, T> {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<T>;
}

impl<'ctx, T: Interp<'ctx, U>, U> Interp<'ctx, U> for Box<T> {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<U> {
        self.as_ref().eval(intrp)
    }
}

impl<'ctx, T, U> Interp<'ctx, Vec<U>> for ListNode<T>
where
    T: Interp<'ctx, U> + PrettyPrint<()>,
    U: PrettyPrint<Context>,
{
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Vec<U>> {
        no_trace! {self, intrp, "Interp::<U>::ListNode", {
            self.iter().map(|item| item.eval(intrp)).collect::<InterpResult<Vec<U>>>()
        }}
    }
}

impl<'ctx> Interp<'ctx, Value> for ListNode<Stmt> {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Value> {
        trace! {self, intrp, "Interp::<Value>::ListNode<Stmt>", {
            for item in &self.items[..self.items.len() - 1] {
                match &item.kind {
                    StmtKind::Break => {
                        return Err(InterpError::Break);
                    }
                    StmtKind::Continue => {
                        return Err(InterpError::Continue);
                    }
                    StmtKind::Expr(expr) => Interp::<Value>::eval(expr, intrp)?,
                    StmtKind::Return(expr) => {
                        return Err(InterpError::Return(expr.eval(intrp)?));
                    }
                };
            }

            Ok(match &self.items.last().unwrap().kind {
                StmtKind::Break => return Err(InterpError::Break),
                StmtKind::Continue => return Err(InterpError::Continue),
                StmtKind::Expr(expr) => expr.eval(intrp)?,
                StmtKind::Return(expr) => return Err(InterpError::Return(expr.eval(intrp)?)),
            })
        }}
    }
}

impl<'ctx> Interp<'ctx, Value> for ListNode<Expr> {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Value> {
        trace! {self, intrp, "Interp::<Value>::ListNode<Expr>", {
            for item in &self.items[..self.items.len() - 1] {
                Interp::<Value>::eval(item, intrp)?;
            }
            self.items.last().unwrap().eval(intrp)
        }}
    }
}

impl<'ctx> Interp<'ctx, Option<Value>> for Item {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Option<Value>> {
        match &self.kind {
            ItemKind::Import(path) => todo!(),
            ItemKind::Directive(d) => d.eval(intrp).map(|_| None),
            ItemKind::DimDecl(decl) => decl.eval(intrp).map(|_| None),
            ItemKind::UnitDecl(decl) => decl.eval(intrp).map(|_| None),
            ItemKind::OpDecl(decl) => decl.eval(intrp).map(|_| None),
            ItemKind::ConstDecl(decl) => decl.eval(intrp).map(|_| None),
            ItemKind::FnDecl(decl) => Interp::<()>::eval(decl, intrp).map(|_| None),
            ItemKind::Expr(expr) => {
                let v = Interp::<Value>::eval(expr, intrp)?;
                intrp.ctx.set_last_value(v.clone());
                Ok(Some(v))
            }
        }
    }
}

//
// MARK: Item Impls
//

impl<'ctx> Interp<'ctx, ()> for Directive {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<()> {
        no_trace! {self, intrp, "Interp::<()>::Directive", {
            match &self.kind {
                &DirectiveKind::FloatPrecision(prec) => intrp.ctx.config.float_precision = prec,
                DirectiveKind::DefaultFormatter(name) => {
                    intrp.ctx.set_default_formatter(name.raw);
                }
                _ => (), // nothing to do for other directives, as they are used during parsing
            }
            Ok(())
        }}
    }
}

impl<'ctx> Interp<'ctx, ()> for DimDecl {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<()> {
        no_trace! {self, intrp, "Interp::<()>::DimDecl", {
            let name = self.name.as_spanned_ustr();
            let dim_expr = match &self.dimension {
                Some(expr) => expr.eval(intrp)?,
                None => rt::DimExpr::Dimension(self.name.raw),
            };

            let dimension = if let Some(ref label) = self.label {
                rt::Dimension::with_label(name, dim_expr, label.as_spanned_ustr())
            } else {
                rt::Dimension::new(name, dim_expr)
            };

            intrp
                .active_module()
                .register_dimension(dimension)
                .map_err(InterpError::from)
        }}
    }
}

impl<'ctx> Interp<'ctx, ()> for UnitDecl {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<()> {
        no_trace! {self, intrp, "Interp::<()>::UnitDecl", {
            let kind = self.kind;
            let name = self.name.as_spanned_ustr();
            let suffixes = self.suffixes.iter().map(|s| s.as_spanned_ustr()).collect();

            let unit = match (&self.dimension, &self.value) {
                // Expression-based unit: dimension is None, compute from expression
                (None, Some(Left(expr))) => {
                    let value = Interp::<Value>::eval(expr, intrp)?;

                    // Extract dimension and conversion factor from the evaluated expression
                    match value {
                        Value::Quantity(q) => {
                            let dim_expr = q.dim.expr.clone();
                            let num = q.number.clone();
                            rt::Unit::new(kind, name, suffixes, dim_expr, num)
                        }
                        _ => {
                            // Scalar value - treat as dimensionless
                            let num = match CastInto::<rt::Number>::cast(intrp.ctx, value) {
                                Ok(num) => num,
                                Err(_) => {
                                    return Err(TypeError::mismatch(
                                        format!("expected scalar or quantity value in unit declaration"),
                                        expr.span().into_spanned("given value".to_string()),
                                    )
                                    .into());
                                }
                            };
                            rt::Unit::new(kind, name, suffixes, rt::DimExpr::one(), num)
                        }
                    }
                }

                // Standard units with explicit dimension
                (Some(dimension), value_opt) => {
                    let dim_expr = dimension.eval(intrp)?;

                    match value_opt {
                        Some(value) => match value {
                            Left(expr) => {
                                let value = Interp::<Value>::eval(expr, intrp)?;
                                let num = match CastInto::<rt::Number>::cast(intrp.ctx, value) {
                                    Ok(num) => num,
                                    Err(_) => {
                                        return Err(TypeError::mismatch(
                                            format!("expected scalar value in unit declaration"),
                                            expr.span().into_spanned("given value".to_string()),
                                        )
                                        .into());
                                    }
                                };
                                rt::Unit::new(kind, name, suffixes, dim_expr, num)
                            }
                            Right(unit_impl) => {
                                let impl_obj = Interp::<rt::UnitImpl>::eval(unit_impl, intrp)?;
                                let conversion = rt::Conversion::Impl(impl_obj);
                                rt::Unit::with_conversion(kind, name, suffixes, dim_expr, conversion)
                            },
                        },
                        None => rt::Unit::new(kind, name, suffixes, dim_expr, rt::Number::Int(1.into()))
                    }
                }

                // Invalid: dimension None with unit_impl
                (None, Some(Right(_))) => {
                    return Err(TypeError::simple(
                        self.name.span().into_spanned("unit implementation requires explicit dimension annotation".to_string())
                    ).into());
                }

                // Invalid: neither dimension nor value
                (None, None) => {
                    return Err(TypeError::simple(
                        self.name.span().into_spanned("unit declaration requires either dimension or value expression".to_string())
                    ).into());
                }
            };

            // Use update_unit instead of register_unit to replace placeholder units registered during parsing
            intrp.active_module().update_unit(unit);
            Ok(())
        }}
    }
}

impl<'ctx> Interp<'ctx, rt::UnitImpl> for UnitImpl {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<rt::UnitImpl> {
        no_trace! {self, intrp, "Interp::<rt::UnitImpl>::UnitImpl", {
            // Evaluate ALL functions to get Function objects
            let all_funcs: Result<Vec<_>, _> = self.functions.iter()
                .map(|decl| Interp::<Function>::eval(decl, intrp))
                .collect();
            let all_funcs = all_funcs?;

            // Validate against UnitImpl interface from builtin module
            let builtin_module = intrp.ctx.modules.get_module("builtin")
                .expect("builtin module should exist");
            if let Some(interface) = builtin_module.get_interface("UnitImpl").cloned() {
                // Validate all implemented functions
                // This will check that:
                // 1. All required functions are present
                // 2. All provided functions match expected signatures
                // 3. No extra functions are defined that aren't part of the interface
                interface.validate(intrp.ctx, all_funcs.iter().collect())?;
            }

            // Extract the specific functions we need
            let to_base_func = all_funcs.iter().find(|f| f.name.raw == "to_base").unwrap().clone();
            let from_base_func = all_funcs.iter().find(|f| f.name.raw == "from_base").unwrap().clone();
            let display_name_func = all_funcs.iter().find(|f| f.name.raw == "display_name").cloned();

            // Get module ID for storing with the UnitImpl
            let module_id = intrp.active_module().id;

            // Register as unnamed functions and get their IDs
            let to_base_id = intrp.active_module().register_unnamed_function(to_base_func);
            let from_base_id = intrp.active_module().register_unnamed_function(from_base_func);
            let display_name_id = display_name_func.map(|f| intrp.active_module().register_unnamed_function(f));

            // Create and return the runtime UnitImpl
            Ok(rt::UnitImpl::new(module_id, to_base_id, from_base_id, display_name_id))
        }}
    }
}

impl<'ctx> Interp<'ctx, ()> for OpDecl {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<()> {
        no_trace! {self, intrp, "Interp::<()>::FnDecl", {
            let name = self.name.as_spanned_ustr();
            let params = self.params.eval(intrp)?;
            let func = match &self.body {
                Left(path) => Interp::<Function>::eval(path, intrp)?,
                Right(exprs) => {
                    // if the operator declaration has the function defined inline, we need to lift it
                    // into a proper registered function so that path resolution works.
                    let func = Function::source(name, params, exprs.clone());
                    intrp
                        .active_module()
                        .register_function(func)
                        .map_err(InterpError::from);

                    intrp.active_module().resolve_function(name).unwrap().clone()
                },
            };

            let expected_params = match &self.kind {
                OpKind::Prefix | OpKind::Postfix => 1,
                OpKind::Infix => 2,
            };
            if func.params.len() != expected_params {
                return Err(TypeError::mismatch(
                    format!(
                        "expected function that takes {} parameter(s)",
                        expected_params
                    ),
                    Spanned::new(
                        format!("given function accepts {}", func.params.len()),
                        self.body.span(),
                    ),
                )
                .into());
            }

            Ok(())
        }}
    }
}

impl<'ctx> Interp<'ctx, ()> for ConstDecl {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<()> {
        no_trace! {self, intrp, "Interp::<()>::ConstDecl", {
            let name = self.name.as_spanned_ustr();
            let value = Interp::<Value>::eval(&self.value, intrp)?;

            let constant = Constant::new(name, value);
            intrp
                .active_module()
                .register_constant(constant)
                .map_err(InterpError::from)
        }}
    }
}

impl<'ctx> Interp<'ctx, Function> for FnDecl {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Function> {
        no_trace! {self, intrp, "Interp::<Function>::FnDecl", {
            // validate the parameters
            for (i, param) in self.params.iter().enumerate() {
                if param.is_variadic && i != self.params.len() - 1 {
                    return Err(TypeError::simple(
                        param
                            .span()
                            .into_spanned("variadic parameter must be last".to_string()),
                    )
                    .into());
                }
            }

            let name = self.name.as_spanned_ustr();
            let params = self.params.eval(intrp)?;
            let kind = rt::FunctionKind::Source(self.body.clone());

            Ok(Function::new(name, params, kind))
        }}
    }
}

impl<'ctx> Interp<'ctx, ()> for FnDecl {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<()> {
        let func = Interp::<Function>::eval(self, intrp)?;
        intrp
            .active_module()
            .register_function(func)
            .map_err(InterpError::from)
    }
}

impl<'ctx> Interp<'ctx, rt::Param> for Param {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<rt::Param> {
        no_trace! {self, intrp, "Interp::<Param>::Param", {
            if self.is_variadic {
                assert!(self.anno.is_none());
                return Ok(rt::Param::variadic(self.name.as_spanned_ustr()));
            }

            let ty = match &self.anno {
                Some(Left(dim_node)) => {
                    // Check if this is a simple identifier that might be a unit constraint (e.g., [rad])
                    let ty = if let DimExprKind::Ident(ident) = &dim_node.kind {
                            // Try to resolve as a unit suffix first
                        let module = intrp.ctx.active_module_mut().unwrap();
                        if let Ok(unit) = module.resolve_unit_suffix(ident.as_spanned_ustr()) {
                            // This is a unit constraint - create a Dim with unit info
                            rt::Ty::Dim(rt::Dim::simple(unit.dim_expr.clone(), ident.raw, unit.conversion.clone()))
                        } else {
                            // Not a unit, treat as a regular dimension expression
                            rt::Ty::Dim(rt::Dim::from(dim_node.eval(intrp)?))
                        }
                    } else {
                        // Complex dimension expression
                        rt::Ty::Dim(rt::Dim::from(dim_node.eval(intrp)?))
                    };
                    Some(dim_node.span().into_spanned(ty))
                },
                Some(Right(ty_node)) => {
                    let ty = ty_node.eval(intrp)?;
                    Some(ty_node.span().into_spanned(ty))
                },
                None => None,
            };
            Ok(rt::Param::new(self.name.as_spanned_ustr(), ty))
        }}
    }
}

//
// MARK: Expr Impls
//

impl<'ctx> Interp<'ctx, rt::DimExpr> for DimExpr {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<rt::DimExpr> {
        use rt::DimExpr;
        Ok(match &self.kind {
            DimExprKind::Mul(lhs, rhs) => {
                let lhs = lhs.eval(intrp)?;
                let rhs = rhs.eval(intrp)?;
                DimExpr::Mul(lhs.into(), rhs.into())
            }
            DimExprKind::Div(lhs, rhs) => {
                let lhs = lhs.eval(intrp)?;
                let rhs = rhs.eval(intrp)?;
                DimExpr::Div(lhs.into(), rhs.into())
            }
            DimExprKind::Pow(lhs, rhs) => {
                let lhs = lhs.eval(intrp)?;
                let rhs = rhs.eval(intrp)?;
                DimExpr::Pow(lhs.into(), rhs.into())
            }
            DimExprKind::Neg(expr) => {
                let expr = expr.eval(intrp)?;
                DimExpr::Neg(expr.into())
            }
            DimExprKind::Ident(name) => {
                let module = intrp.ctx.active_module_mut().unwrap();
                let dim = module
                    .resolve_dimension(name.as_spanned_ustr())
                    .map_err(InterpError::from)?;

                dim.expr.clone()
            }
            DimExprKind::Number(num) => {
                let number = num.eval(intrp)?;
                DimExpr::Number(number)
            }
            DimExprKind::Unit(suffix) => {
                // For unit constraints like [rad], we look up the unit and return its dimension
                let module = intrp.ctx.active_module_mut().unwrap();
                let unit = module
                    .resolve_unit_suffix(suffix.as_spanned_ustr())
                    .map_err(InterpError::from)?;

                unit.dim_expr.clone()
            }
        })
    }
}

impl<'ctx> Interp<'ctx, LRValue> for Expr {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<LRValue> {
        trace! {self, intrp, "Interp::<Value>::Expr", {
            match &self.kind {
                ExprKind::Assign(lhs, rhs) => {
                    let pat = lhs.eval(intrp)?;
                    let value = Interp::<Value>::eval(rhs, intrp)?;
                    let bindings = pat.bind_with(intrp.ctx, value.clone())?;

                    let mut new_bindings = vec![];
                    for (name, value) in bindings.into_iter() {
                        match intrp.ctx.resolve_variable(name) {
                            Ok(vref) if vref.is_mut() => vref.set(value),
                            Ok(_) => new_bindings.push((name, value)), // allow overshadowing the constant
                            Err(_) => new_bindings.push((name, value)), // no existing variable, create one
                        }
                    }

                    if !new_bindings.is_empty() {
                        let scope = LocalScope::from(new_bindings.into_iter());
                        intrp.ctx.push_local_scope(scope);
                    }
                    Ok(LRValue::R(value))
                }
                ExprKind::InfixOp(op, lhs, rhs) => {
                    let func = op.eval(intrp)?;
                    let args = ListNode::from(vec![*lhs.clone(), *rhs.clone()]);
                    Ok(LRValue::R(intrp.invoke(&func, args, op.span())?))
                }
                ExprKind::IndexAssign(container, index, value) => {
                    // Evaluate container and index refs (container must be mutable for list/object)
                    let container_ref = Interp::<ValueRef>::eval(container, intrp)?;
                    let idx_val = Interp::<Value>::eval(index, intrp)?;
                    let new_val = Interp::<Value>::eval(value, intrp)?;

                    let mut container_mut = container_ref.borrow_mut();
                    match &mut *container_mut {
                        Value::List(list) => {
                            let idx_usize = match idx_val {
                                Value::Quantity(q) if q.is_dimless() => q
                                    .number
                                    .into_int(intrp.ctx)
                                    .map_err(InterpError::from)?
                                    .to_usize()
                                    .ok_or_else(|| {
                                        InterpError::Exception(Exception::new(
                                            "IndexError",
                                            "index must be non-negative".to_string(),
                                        )
                                        .with_backtrace(intrp.ctx.backtrace()))
                                    })?,
                                _ => {
                                    return Err(InterpError::Exception(Exception::new(
                                        "TypeError",
                                        "list index must be an integer".to_string(),
                                    )
                                    .with_backtrace(intrp.ctx.backtrace())));
                                }
                            };

                            let mut vec_ref = list.borrow_mut();
                            if idx_usize >= vec_ref.len() {
                                return Err(InterpError::Exception(
                                    Exception::new(
                                        "IndexError",
                                        format!("list index out of range: {}", idx_usize),
                                    )
                                    .with_backtrace(intrp.ctx.backtrace()),
                                ));
                            }
                            vec_ref[idx_usize] = new_val;
                            Ok(LRValue::R(Value::Empty))
                        }
                        Value::Object(object) => {
                            let key = match idx_val {
                                Value::String(s) => s,
                                _ => {
                                    return Err(InterpError::Exception(
                                        Exception::new(
                                            "TypeError",
                                            "object indices must be strings".to_string(),
                                        )
                                        .with_backtrace(intrp.ctx.backtrace()),
                                    ));
                                }
                            };

                            let key_ustr = Ustr::from(&key);
                            let mut fields = object.borrow_mut();
                            if let Some((_, val)) = fields.iter_mut().find(|(k, _)| *k == key_ustr) {
                                *val = new_val;
                            } else {
                                fields.push((key_ustr, new_val));
                            }
                            Ok(LRValue::R(Value::Empty))
                        }
                        Value::Tuple(_) => Err(InterpError::Exception(
                            Exception::new("TypeError", "cannot assign into tuple".to_string())
                                .with_backtrace(intrp.ctx.backtrace()),
                        )),
                        other => Err(InterpError::Exception(
                            Exception::new(
                                "TypeError",
                                format!(
                                    "cannot assign to index of type: {}",
                                    other.ty().pretty_string(intrp.ctx)
                                ),
                            )
                            .with_backtrace(intrp.ctx.backtrace()),
                        )),
                    }
                }
                ExprKind::PrefixOp(op, expr) => {
                    let func = op.eval(intrp)?;
                    let args = ListNode::from(vec![*expr.clone()]);
                    Ok(LRValue::R(intrp.invoke(&func, args, op.span())?))
                }
                ExprKind::PostfixOp(expr, op) => {
                    let func = op.eval(intrp)?;
                    let args = ListNode::from(vec![*expr.clone()]);
                    Ok(LRValue::R(intrp.invoke(&func, args, op.span())?))
                }
                ExprKind::UnitCast(expr, unit) => {
                    let value = Interp::<Value>::eval(expr, intrp)?;

                    use rt::Number;
                    match value {
                        Value::Quantity(q) => {
                            let (name, conv, expr) = {
                                let unit = intrp
                                    .ctx
                                    .active_module_mut()
                                    .unwrap()
                                    .resolve_unit_suffix(unit.span().into_spanned(unit.name))
                                    .map_err(InterpError::from)?;

                                (unit.name.raw, unit.conversion.clone(), unit.dim_expr.clone())
                            };

                            // Delayed conversion: keep value in display units, not base units
                            // Only convert to base when mixing units or explicitly requested
                            let number = q.number.clone();
                            let dim = (expr, name, conv).into();
                            let quantity: rt::Quantity = (number, dim).into();
                            Ok(LRValue::R(Value::Quantity(quantity)))
                        }
                        _ => Err(TypeError::mismatch(
                            format!("expected number"),
                            expr.span().into_spanned("given value".to_string()),
                        )
                        .into()),
                    }
                }
                ExprKind::IfElse(cond, then, else_) => {
                    let cond = Interp::<Value>::eval(cond, intrp)?;
                    Ok(LRValue::R(if !cond.is_zero() {
                        Interp::<Value>::eval(then, intrp)?
                    } else {
                        Interp::<Value>::eval(else_, intrp)?
                    }))
                }
                ExprKind::ForRange(pat, iter, body) => {
                    let pat = pat.eval(intrp)?;
                    let iter = Interp::<ValueRef>::eval(iter, intrp)?.try_into_list(intrp.ctx)?;
                    for value in iter.borrow().iter().cloned() {
                        let scope = LocalScope::from(pat.bind_with(intrp.ctx, value)?.into_iter());
                        match Context::with_scope(intrp, scope, |intrp| Interp::<Value>::eval(body, intrp)) {
                            Ok(_) => {}, // Normal iteration
                            Err(InterpError::Continue) => continue, // Skip to next iteration
                            Err(InterpError::Break) => break, // Exit loop
                            Err(e) => return Err(e), // Propagate other errors
                        }
                    }
                    Ok(LRValue::R(Value::Empty))
                }
                ExprKind::FnCall(func, args) => {
                    let span = func.span().union_with(args.span());
                    let func = Interp::<Function>::eval(func, intrp)?;
                    Ok(LRValue::R(intrp.invoke(&func, args.clone(), span)?))
                }
                ExprKind::List(node) => {
                    let mut values = vec![];
                    for item in node.iter() {
                        let value = Interp::<Value>::eval(item, intrp)?;
                        values.push(value.into());
                    }
                    Ok(LRValue::R(Value::List(VRef::new(values))))
                }
                ExprKind::Object(node) => {
                    let mut fields: Vec<(Ustr, Value)> = vec![];
                    for field in node.iter() {
                        let key = Ustr::from(field.key.raw.as_str());
                        let value = Interp::<Value>::eval(&field.value, intrp)?;
                        if let Some((_, existing)) = fields.iter_mut().find(|(k, _)| *k == key) {
                            *existing = value.into();
                        } else {
                            fields.push((key, value.into()));
                        }
                    }
                    Ok(LRValue::R(Value::object(fields)))
                }
                ExprKind::Tuple(node) => {
                    let mut values = vec![];
                    for item in node.iter() {
                        let value = Interp::<Value>::eval(item, intrp)?;
                        values.push(value.into());
                    }
                    Ok(LRValue::R(Value::Tuple(SmallVec::from_vec(values))))
                }
                ExprKind::Path(path) => {
                    let res = Interp::<ValueRef>::eval(path, intrp)?;
                    Ok(LRValue::L(res))
                }
                ExprKind::Ident(ident) => {
                    let res = Interp::<ValueRef>::eval(ident, intrp)?;
                    Ok(LRValue::L(res))
                }
                ExprKind::Number(num) => Ok(LRValue::R(
                    Interp::<rt::Number>::eval(num, intrp).map(Value::from)?,
                )),
                ExprKind::String(s) => Ok(LRValue::R(Value::String(s.clone()))),
                ExprKind::Boolean(b) => Ok(LRValue::R(Value::Boolean(*b))),
                ExprKind::Unit(unit) => {
                    let unit = intrp
                        .ctx
                        .active_module()
                        .unwrap()
                        .resolve_unit_suffix(unit.clone().into_raw_spanned())?;
                    Ok(LRValue::R(Value::Unit(unit.name.into())))
                }
                ExprKind::Slice(container, start, stop) => {
                    let value = Interp::<Value>::eval(container, intrp)?;
                    let start_val = match start {
                        Some(expr) => Some(Interp::<Value>::eval(expr, intrp)?),
                        None => None,
                    };
                    let stop_val = match stop {
                        Some(expr) => Some(Interp::<Value>::eval(expr, intrp)?),
                        None => None,
                    };

                    let slice_result = apply_slice(intrp.ctx, value, start_val, stop_val)?;
                    Ok(LRValue::R(slice_result))
                }
                ExprKind::Type(ty) => {
                    let ty = ty.eval(intrp)?;
                    Ok(LRValue::R(Value::Ty(ty)))
                }
                ExprKind::Splat(_) => {
                    Err(TypeError::mismatch(
                        "splat expressions can only be used as arguments to variadic functions".to_string(),
                        self.span().into_spanned("invalid splat usage".to_string()),
                    ).into())
                }
            }
        }}
    }
}

impl<'ctx> Interp<'ctx, Value> for Expr {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Value> {
        match Interp::<LRValue>::eval(self, intrp)? {
            LRValue::L(vref) => Ok(vref.get()),
            LRValue::R(value) => Ok(value),
        }
    }
}

fn value_to_isize(ctx: &mut Context, value: Value) -> Result<isize, Exception> {
    match value {
        Value::Ref(r) => value_to_isize(ctx, r.borrow().clone()),
        Value::Quantity(q) if q.is_dimless() => {
            let int = q.number.into_int(ctx)?;
            int.to_isize().ok_or_else(|| {
                Exception::new(
                    "IndexError",
                    "index must be non-negative and in range".to_string(),
                )
                .with_backtrace(ctx.backtrace())
            })
        }
        other => Err(Exception::new(
            "TypeError",
            format!(
                "slice indices must be integers, found {}",
                other.ty().pretty_string(ctx)
            ),
        )
        .with_backtrace(ctx.backtrace())),
    }
}

fn apply_slice(
    ctx: &mut Context,
    container: Value,
    start: Option<Value>,
    stop: Option<Value>,
) -> Result<Value, InterpError> {
    let (len, slicer): (usize, Box<dyn Fn(usize, usize) -> Value>) = match container {
        Value::List(list) => {
            let items = list.borrow().clone();
            let len = items.len();
            let slicer = move |start, stop| {
                let slice = items[start..stop].to_vec();
                Value::list(slice)
            };
            (len, Box::new(slicer))
        }
        Value::Tuple(tuple) => {
            let items: Vec<Value> = tuple.iter().map(|v| (**v).clone()).collect();
            let len = items.len();
            let slicer = move |start, stop| {
                let slice = items[start..stop].to_vec();
                Value::Tuple(SmallVec::from_vec(
                    slice.into_iter().map(Box::new).collect(),
                ))
            };
            (len, Box::new(slicer))
        }
        Value::String(s) => {
            let chars: Vec<char> = s.chars().collect();
            let len = chars.len();
            let slicer = move |start, stop| {
                let slice: String = chars[start..stop].iter().collect();
                Value::String(slice)
            };
            (len, Box::new(slicer))
        }
        other => {
            return Err(InterpError::from(
                Exception::new(
                    "TypeError",
                    format!("cannot slice type: {}", other.ty().pretty_string(ctx)),
                )
                .with_backtrace(ctx.backtrace()),
            ))
        }
    };

    let len_isize = len as isize;

    let mut start_idx = match start {
        Some(v) => value_to_isize(ctx, v)?,
        None => 0,
    };
    let mut stop_idx = match stop {
        Some(v) => value_to_isize(ctx, v)?,
        None => len_isize,
    };

    if start_idx < 0 {
        start_idx += len_isize;
    }
    if stop_idx < 0 {
        stop_idx += len_isize;
    }

    start_idx = start_idx.clamp(0, len_isize);
    stop_idx = stop_idx.clamp(0, len_isize);

    let (start_u, stop_u) = if start_idx > stop_idx {
        let v = stop_idx as usize;
        (v, v)
    } else {
        (start_idx as usize, stop_idx as usize)
    };

    Ok(slicer(start_u, stop_u))
}

impl<'ctx> Interp<'ctx, ValueRef> for Expr {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<ValueRef> {
        match Interp::<LRValue>::eval(self, intrp)? {
            LRValue::L(vref) => Ok(vref),
            LRValue::R(value) => Ok(ValueRef::new(value)),
        }
    }
}

impl<'ctx> Interp<'ctx, rt::Pattern> for BindPat {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<rt::Pattern> {
        trace! {self, intrp, "Interp::<Pattern>::BindPat", {
            use rt::Pattern;
            Ok(match &self.kind {
                BindPatKind::Ignored => Pattern::Ignore,
                BindPatKind::Var(ident) => Pattern::Var(ident.as_spanned_ustr()),
                BindPatKind::Tuple(pats) => {
                    let pats = pats.eval(intrp)?;
                    Pattern::Tuple(pats)
                }
            })
        }}
    }
}

impl<'ctx> Interp<'ctx, rt::Ty> for Ty {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<rt::Ty> {
        use rt::Ty;
        no_trace! {self, intrp, "Interp::<Ty>::Ty", {
            Ok(match &self.kind {
                TyKind::Any => Ty::Any,
                TyKind::Bool => Ty::Bool,
                TyKind::Int => Ty::Int,
                TyKind::Float => Ty::Float,
                TyKind::Str => Ty::Str,
                TyKind::Function => Ty::Function,
                TyKind::Io => Ty::Io,
                TyKind::Num => Ty::Num,
                TyKind::Unit => Ty::Unit,
                TyKind::Type => Ty::Type,
                TyKind::List => Ty::List,
                TyKind::Object => Ty::Object,
                TyKind::Tuple(tys) => {
                    let tys = tys.eval(intrp)?.into_iter().map(|ty| Box::new(ty)).collect();
                    Ty::Tuple(SmallVec::from_vec(tys))
                },
                TyKind::Ref(r) => Ty::Ref(Box::new(r.eval(intrp)?)),
            })
        }}
    }
}

impl<'ctx> Interp<'ctx, Function> for Operator {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Function> {
        trace! {self, intrp, "Interp::<Function>::Operator", {
            let module = intrp.ctx.active_module_mut().unwrap();

            let op = module.resolve_operator(self.kind, self.as_spanned_ustr()).map_err(InterpError::from)?;
            match op.func.clone() {
                Left(path) => path.eval(intrp),
                Right(body) => todo!(),
            }
        }}
    }
}

impl<'ctx> Interp<'ctx, Value> for Path {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Value> {
        intrp
            .ctx
            .resolve_variable(self.path_parts())
            .map(|vref| vref.get())
            .or_else(|_| {
                intrp
                    .ctx
                    .resolve_function(self.path_parts())
                    .map(|f| Value::Function(f.clone()))
            })
            .map_err(InterpError::from)
    }
}

impl<'ctx> Interp<'ctx, ValueRef> for Path {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<ValueRef> {
        intrp
            .ctx
            .resolve_variable(self.path_parts())
            .map_err(InterpError::from)
    }
}

impl<'ctx> Interp<'ctx, Function> for Path {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Function> {
        trace! {self, intrp, "Interp::<Function>::Path", {
            intrp.ctx.resolve_function(self.path_parts()).map_err(InterpError::from).cloned()
        }}
    }
}

impl<'ctx> Interp<'ctx, Value> for Ident {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Value> {
        trace! {self, intrp, "Interp::<Value>::Ident", {
            let name = self.as_spanned_ustr();
            intrp
                .ctx
                .resolve_variable(name)
                .map(|vref| vref.get())
                .or_else(|_| intrp.ctx.resolve_function(name).map(|f| Value::Function(f.clone())))
                .map_err(InterpError::from)
        }}
    }
}

impl<'ctx> Interp<'ctx, ValueRef> for Ident {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<ValueRef> {
        intrp
            .ctx
            .resolve_variable(self.as_spanned_ustr())
            .map_err(InterpError::from)
    }
}

impl<'ctx> Interp<'ctx, Function> for Ident {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Function> {
        trace! {self, intrp, "Interp::<Function>::Ident", {
            let name = self.as_spanned_ustr();
            intrp.ctx.resolve_function(name).map_err(InterpError::from).cloned()
        }}
    }
}

impl<'ctx> Interp<'ctx, rt::Number> for Number {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<rt::Number> {
        trace! {self, intrp, "Interp::<Number>::Number", {
            Ok(match &self.kind {
                NumberKind::Integer(v) => rt::Number::Int(v.clone()),
                NumberKind::Float(v) => rt::Number::Float(v.clone()),
            })
        }}
    }
}
