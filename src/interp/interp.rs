use super::{InterpError, InterpResult, NameError, TypeError, Value};

use crate::ast::*;
use crate::diag::IntoError;
use crate::print::{PrettyPrint, PrettyString};
use crate::runtime::{
    self as rt, CastInto, Context, ContextProvider, Function, LocalScope, StackFrame,
};
use crate::source::{SourceSpan, Spanned};

use either::{Either, Left, Right};
use rug::{Float, Integer};
use smallvec::{smallvec, SmallVec};
use std::cell::RefCell;
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

        // evaluate each argument while checking that the correct number of arguments are given
        let mut values = vec![];
        let mut pos = args.start_pos() + 1;
        for (i, param) in f.params.iter().enumerate() {
            let arg = args.get(i).ok_or_else(|| {
                TypeError::new(
                    format!(
                        "function {} expects {} argument(s)",
                        f.name.raw,
                        f.params.len(),
                    ),
                    pos.as_span().into_spanned(format!("expected argument")),
                )
            })?;

            pos = arg.span().end_pos();
            values.push(arg.eval(self)?);
        }

        // check if there are any remaining arguments that shouldnt be
        if args.len() > f.params.len() {
            return Err(TypeError::new(
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

        // apply any coercion from the arguments to the parameters
        let values = values
            .into_iter()
            .zip(f.params.iter())
            .map(|(v, p)| (v, p.ty.clone().map_or(rt::Ty::Any, |ty| ty.raw)))
            .map(|(v, t)| rt::coerce::to_ty(self.ctx, v, t))
            .collect::<Vec<_>>();

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
        }?;

        Ok(result)
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

/// A trait for nodes that can be intrpreted.
pub(super) trait Interp<'ctx, T> {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<T>;
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

impl<'ctx> Interp<'ctx, Value> for ListNode<Expr> {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Value> {
        trace! {self, intrp, "Interp::<Value>::ListNode<Expr>", {
            for item in &self.items[..self.items.len() - 1] {
                item.eval(intrp)?;
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
            ItemKind::FnDecl(decl) => decl.eval(intrp).map(|_| None),
            ItemKind::Expr(expr) => expr.eval(intrp).map(Some),
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

            let dimension = rt::Dimension::new(name, dim_expr);
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
            let dim_expr = self.dimension.eval(intrp)?;
            let scalar = match &self.scalar {
                Some(scalar) => {
                    let value = scalar.eval(intrp)?;
                    match CastInto::<rt::Number>::cast(intrp.ctx, value) {
                        Ok(num) => num,
                        Err(_) => {
                            return Err(TypeError::new(
                                format!("expected scalar value in unit declaration"),
                                scalar.span().into_spanned("given value".to_string()),
                            )
                            .into());
                        }
                    }
                },
                None => rt::Number::Int(1.into())
            };

            let unit = rt::Unit::new(kind, name, suffixes, dim_expr, scalar);
            intrp
                .active_module()
                .register_unit(unit)
                .map_err(InterpError::from)
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
                return Err(TypeError::new(
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

impl<'ctx> Interp<'ctx, ()> for FnDecl {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<()> {
        no_trace! {self, intrp, "Interp::<()>::FnDecl", {
            let name = self.name.as_spanned_ustr();
            let params = self.params.eval(intrp)?;
            let kind = rt::FunctionKind::Source(self.body.clone());

            let func = Function::new(name, params, kind);
            intrp
                .active_module()
                .register_function(func)
                .map_err(InterpError::from)
        }}
    }
}

impl<'ctx> Interp<'ctx, rt::Param> for Param {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<rt::Param> {
        no_trace! {self, intrp, "Interp::<Param>::Param", {
            let ty = match &self.anno {
                Some(Left(dim_node)) => {
                    let ty = rt::Ty::Dim(rt::Dim::from(dim_node.eval(intrp)?));
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
        })
    }
}

impl<'ctx> Interp<'ctx, Value> for Expr {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Value> {
        trace! {self, intrp, "Interp::<Value>::Expr", {
            match &self.kind {
                ExprKind::Assign(lhs, rhs) => {
                    let pat = lhs.eval(intrp)?;
                    let value = rhs.eval(intrp)?;
                    let bindings = pat.bind_with(intrp.ctx, value.clone())?;

                    let scope = LocalScope::from(bindings.into_iter());
                    intrp.ctx.push_local_scope(scope);
                    Ok(value)
                }
                ExprKind::InfixOp(op, lhs, rhs) => {
                    let func = op.eval(intrp)?;
                    let args = ListNode::from(vec![*lhs.clone(), *rhs.clone()]);
                    intrp.invoke(&func, args, op.span())
                }
                ExprKind::PrefixOp(op, expr) => {
                    let func = op.eval(intrp)?;
                    let args = ListNode::from(vec![*expr.clone()]);
                    intrp.invoke(&func, args, op.span())
                }
                ExprKind::PostfixOp(expr, op) => {
                    let func = op.eval(intrp)?;
                    let args = ListNode::from(vec![*expr.clone()]);
                    intrp.invoke(&func, args, op.span())
                }
                ExprKind::Unit(expr, unit) => {
                    let value = expr.eval(intrp)?;

                    use rt::Number;
                    match value {
                        Value::Quantity(q) => {
                            let (name, scale, expr) = {
                                let unit = intrp
                                    .ctx
                                    .active_module_mut()
                                    .unwrap()
                                    .resolve_unit_suffix(unit.span().into_spanned(unit.name))
                                    .map_err(InterpError::from)?;

                                (unit.name.raw, unit.scale.clone(), unit.dim_expr.clone())
                            };

                            let number = Number::safe_mul(intrp.ctx, q.number.clone(), scale.clone())?;
                            let dim = (expr, name, scale).into();
                            let quantity = (number, dim).into();
                            Ok(Value::Quantity(quantity))
                        }
                        _ => Err(TypeError::new(
                            format!("expected number"),
                            expr.span().into_spanned("given value".to_string()),
                        )
                        .into()),
                    }
                }
                ExprKind::IfElse(cond, then, else_) => {
                    let cond = cond.eval(intrp)?;
                    if !cond.is_zero() {
                        then.eval(intrp)
                    } else {
                        else_.eval(intrp)
                    }
                }
                ExprKind::ForRange(pat, iter, body) => {
                    let pat = pat.eval(intrp)?;
                    let iter = iter.eval(intrp)?.into_list(intrp.ctx)?;
                    for value in iter.borrow().iter().cloned() {
                        let scope = LocalScope::from(pat.bind_with(intrp.ctx, value)?.into_iter());
                        Context::with_scope(intrp, scope, |intrp| {
                            Interp::<Value>::eval(body, intrp)
                        })?;
                    }
                    Ok(Value::Empty)
                }
                ExprKind::FnCall(func, args) => {
                    let span = func.span().union_with(args.span());
                    let func = Interp::<Function>::eval(func, intrp)?;
                    intrp.invoke(&func, args.clone(), span)
                }
                ExprKind::List(node) => {
                    let mut values = vec![];
                    for item in node.iter() {
                        let value = item.eval(intrp)?;
                        values.push(value.into());
                    }
                    Ok(Value::List(Rc::new(RefCell::new(values))))
                }
                ExprKind::Tuple(node) => {
                    let mut values = vec![];
                    for item in node.iter() {
                        let value = item.eval(intrp)?;
                        values.push(value.into());
                    }
                    Ok(Value::Tuple(SmallVec::from_vec(values)))
                }
                ExprKind::Path(path) => Interp::<Value>::eval(path, intrp),
                ExprKind::Ident(ident) => Interp::<Value>::eval(ident, intrp),
                ExprKind::Number(num) => Interp::<rt::Number>::eval(num, intrp).map(Value::from),
                ExprKind::String(s) => Ok(Value::String(s.clone())),
                ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
            }
        }}
    }
}

impl<'ctx> Interp<'ctx, rt::Pattern> for BindPat {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<rt::Pattern> {
        no_trace! {self, intrp, "Interp::<Pattern>::BindPat", {
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
                TyKind::Num => Ty::Num,
                TyKind::List => Ty::List,
                TyKind::Tuple(tys) => {
                    let tys = tys.eval(intrp)?.into_iter().map(|ty| Box::new(ty)).collect();
                    Ty::Tuple(SmallVec::from_vec(tys))
                },
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
        trace! {self, intrp, "Interp::<Value>::Path", {
            intrp.ctx.resolve_value(self.path_parts()).map_err(InterpError::from).cloned()
        }}
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
            intrp.ctx.resolve_value(name).map_err(InterpError::from).cloned()
        }}
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
