use super::{Context, Dim, DimExpr, Exception, Float, Integer, Number, Numeric, Quantity, Value};

use crate::ast::{BinaryCoercion, Coercion, FloatConversion};
use crate::print::ansi::{
    chars::{LBRAC, RBRAC},
    ATTR, RESET,
};
use crate::print::{PrettyPrint, PrettyString};

use smallvec::SmallVec;

// MARK: Ty

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Ty {
    Any,
    Bool,
    Float,
    Int,
    Str,
    Num,
    Dim(Dim),
    List,
    Tuple(SmallVec<[Box<Ty>; 3]>),
}

impl Ty {
    /// Returns whether `a` is a stricter or different type than `b`.
    pub fn is_stricter_or_not(a: &Ty, b: &Ty) -> bool {
        match (a, b) {
            (a, b) if a == b => false,
            (_, Ty::Any) => true,
            (Ty::Int, Ty::Num) => true,
            (Ty::Float, Ty::Num) => true,
            (Ty::Dim(_), Ty::Num) => true,
            (Ty::Tuple(a), Ty::Tuple(b)) => a
                .iter()
                .zip(b.iter())
                .any(|(a, b)| Ty::is_stricter_or_not(a, b)),
            _ => false,
        }
    }

    pub fn unify(ctx: &Context, a: &Ty, b: &Ty) -> Result<Ty, Exception> {
        match (a, b) {
            (Ty::Any, _) => Ok(b.clone()),
            (_, Ty::Any) => Ok(a.clone()),
            (Ty::Bool, Ty::Bool) => Ok(Ty::Bool),
            (Ty::Float, Ty::Float) => Ok(Ty::Float),
            (Ty::Int, Ty::Int) => Ok(Ty::Int),
            (Ty::Str, Ty::Str) => Ok(Ty::Str),
            (Ty::Num, Ty::Num) => Ok(Ty::Num),
            (Ty::Dim(a), Ty::Dim(b)) => Ok(Ty::Dim(Dim::unify(ctx, a.clone(), b.clone())?)),
            (Ty::List, Ty::List) => Ok(Ty::List),
            (Ty::Tuple(a), Ty::Tuple(b)) => {
                if a.len() != b.len() {
                    return Err(Exception::new(
                        "TypeError: tuple length mismatch",
                        format!("{} != {}", a.len(), b.len()),
                    )
                    .with_backtrace(ctx.backtrace()));
                }

                let mut result = SmallVec::new();
                for (a, b) in a.iter().zip(b.iter()) {
                    result.push(Box::new(Ty::unify(ctx, a, b)?));
                }

                Ok(Ty::Tuple(result))
            }
            _ => Err(Exception::new(
                "TypeError: type mismatch",
                format!("{} != {}", a.pretty_string(&()), b.pretty_string(&())),
            )
            .with_backtrace(ctx.backtrace())),
        }
    }
}

impl ToString for Ty {
    fn to_string(&self) -> String {
        match self {
            Ty::Any => "any".to_owned(),
            Ty::Bool => "bool".to_owned(),
            Ty::Float => "float".to_owned(),
            Ty::Int => "int".to_owned(),
            Ty::Str => "str".to_owned(),
            Ty::Num => "num".to_owned(),
            Ty::Dim(dim) => dim.to_string(),
            Ty::List => "list".to_owned(),
            Ty::Tuple(ty) => {
                let mut result = String::new();
                result.push_str("(");
                for (i, ty) in ty.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&ty.to_string());
                }
                result.push_str(")");
                result
            }
        }
    }
}

impl<Ctx> PrettyPrint<Ctx> for Ty {
    fn pretty_print<Output: std::io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        level: usize,
    ) -> std::io::Result<()> {
        match self {
            Ty::Any => write!(out, "{ATTR}any{RESET}"),
            Ty::Bool => write!(out, "{ATTR}bool{RESET}"),
            Ty::Float => write!(out, "{ATTR}float{RESET}"),
            Ty::Int => write!(out, "{ATTR}int{RESET}"),
            Ty::Str => write!(out, "{ATTR}str{RESET}"),
            Ty::Num => write!(out, "{ATTR}num{RESET}"),
            Ty::Dim(dim) => write!(out, "{LBRAC}{}{RBRAC}", dim.pretty_string(ctx)),
            Ty::List => write!(out, "{ATTR}list{RESET}"),
            Ty::Tuple(ty) => {
                write!(out, "{LBRAC}", LBRAC = LBRAC)?;
                for (i, ty) in ty.iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ")?;
                    }
                    ty.pretty_print(out, ctx, level)?;
                }
                write!(out, "{RBRAC}", RBRAC = RBRAC)
            }
        }
    }
}

// MARK: CastInto

pub trait CastInto<T> {
    fn cast(ctx: &Context, value: Self) -> Result<T, Exception>
    where
        Self: Sized;
}

impl CastInto<Value> for Value {
    fn cast(ctx: &Context, value: Value) -> Result<Value, Exception> {
        Ok(value)
    }
}

impl CastInto<Quantity> for Value {
    fn cast(ctx: &Context, value: Value) -> Result<Quantity, Exception> {
        match value {
            Value::Boolean(b) => Ok(Quantity::new(Number::from(b), Dim::none())),
            Value::Quantity(q) => Ok(q),
            v => Err(Exception::new(
                "ValueError: expected quantity",
                format!("found {}", v.ty().pretty_string(&())),
            )
            .with_backtrace(ctx.backtrace())),
        }
    }
}

impl CastInto<Number> for Value {
    fn cast(ctx: &Context, value: Value) -> Result<Number, Exception> {
        match value {
            Value::Boolean(b) => Ok(Number::from(b)),
            Value::Quantity(q) => {
                if q.is_dimless() {
                    Ok(q.number)
                } else {
                    Err(Exception::new(
                        "ValueError: expected number",
                        format!("found {}", q.ty().pretty_string(&())),
                    )
                    .with_backtrace(ctx.backtrace()))
                }
            }
            v => Err(Exception::new(
                "ValueError: expected number",
                format!("found {}", v.ty().pretty_string(&())),
            )),
        }
    }
}

impl CastInto<Integer> for Value {
    fn cast(ctx: &Context, value: Value) -> Result<Integer, Exception> {
        match value {
            Value::Boolean(b) => Ok(Integer::from(b)),
            Value::Quantity(q) if q.is_int() && q.is_dimless() => {
                Ok(q.number.get_int().unwrap().clone())
            }
            value => Err(Exception::new(
                "TypeError: expected integer",
                format!("found {}", value.ty().pretty_string(&())),
            )
            .with_backtrace(ctx.backtrace())),
        }
    }
}

impl CastInto<Float> for Value {
    fn cast(ctx: &Context, value: Value) -> Result<Float, Exception> {
        match value {
            Value::Quantity(q) if q.is_float() && q.is_dimless() => {
                Ok(q.number.get_float().unwrap().clone())
            }
            value => Err(Exception::new(
                "TypeError: expected float",
                format!("found {}", value.ty().pretty_string(&())),
            )
            .with_backtrace(ctx.backtrace())),
        }
    }
}

// MARK: CastFrom

pub trait CastFrom<T> {
    fn cast(ctx: &Context, value: T) -> Result<Self, Exception>
    where
        Self: Sized;
}

impl<T: CastInto<U>, U> CastFrom<T> for U {
    fn cast(ctx: &Context, value: T) -> Result<U, Exception> {
        CastInto::<U>::cast(ctx, value)
    }
}

// MARK: TryCoerce

// This trait is used for boxed values that can be coerced into another inner type.
pub trait TryCoerce<T> {
    // Coerce the inner value into the target type. If the coercion is not possible,
    // the original value is returned.
    fn coerce(ctx: &Context, value: Self) -> Self
    where
        Self: Sized;
}

impl TryCoerce<Value> for Value {
    fn coerce(ctx: &Context, value: Value) -> Value {
        value
    }
}

impl TryCoerce<Quantity> for Value {
    fn coerce(ctx: &Context, value: Value) -> Value {
        match value {
            Value::Tuple(t) => Value::Tuple(t),
            Value::List(l) => Value::List(l),
            Value::Quantity(q) => Value::Quantity(q),
            Value::String(s) => Value::String(s),
            Value::Boolean(b) => Value::from(b),
        }
    }
}

impl TryCoerce<Integer> for Value {
    fn coerce(ctx: &Context, value: Value) -> Value {
        match value {
            Value::Quantity(q) if q.is_dimless() => match q.number {
                Number::Int(v) => Value::from(v),
                Number::Float(v) => match ctx.config.float_conversion {
                    FloatConversion::Trunc => v.clone().trunc().to_integer(),
                    FloatConversion::Round => v.clone().round().to_integer(),
                }
                .map_or_else(|| Value::from(v.to_integer().unwrap()), Value::from),
            },
            value => value,
        }
    }
}

impl TryCoerce<Float> for Value {
    fn coerce(ctx: &Context, value: Value) -> Value {
        match value {
            Value::Quantity(q) if q.is_dimless() => match q.number {
                Number::Int(v) => Value::from(Float::with_val(ctx.config.precision, v)),
                Number::Float(v) => Value::from(v),
            },
            value => value,
        }
    }
}

impl TryCoerce<String> for Value {
    fn coerce(ctx: &Context, value: Value) -> Value {
        match value {
            value => value,
        }
    }
}

impl TryCoerce<bool> for Value {
    fn coerce(ctx: &Context, value: Value) -> Value {
        match value {
            Value::Quantity(q) if q.is_dimless() => Value::from(q.is_truthy()),
            value => value,
        }
    }
}

// MARK: coerce

pub mod coerce {
    use super::*;
    use crate::ast::Coercion;

    /// Coerce a value to a target type.
    pub fn to_ty(ctx: &Context, v: Value, ty: Ty) -> Value {
        if ctx.config.coercion.is_never() {
            return v;
        }

        match ty {
            Ty::Any => v,
            Ty::Bool => TryCoerce::<bool>::coerce(ctx, v),
            Ty::Float => TryCoerce::<Float>::coerce(ctx, v),
            Ty::Int => TryCoerce::<Integer>::coerce(ctx, v),
            Ty::Str => TryCoerce::<String>::coerce(ctx, v),
            Ty::Num => TryCoerce::<Quantity>::coerce(ctx, v),
            Ty::Dim(d) => Value::from(Quantity::one().with_dim(d)),
            Ty::List | Ty::Tuple(_) => v,
        }
    }

    /// Coerce a pair of binary quantities to a common underlying type and dimension.
    pub fn binary_pair(ctx: &Context, lhs: Quantity, rhs: Quantity) -> (Quantity, Quantity) {
        match ctx.config.binary_coercion {
            BinaryCoercion::Left => {
                let rhs = to_ty(ctx, rhs.into(), lhs.ty());
                let rhs = CastInto::<Quantity>::cast(ctx, rhs).unwrap();
                (lhs, rhs)
            }
            BinaryCoercion::Right => {
                let lhs = to_ty(ctx, lhs.into(), rhs.ty());
                let lhs = CastInto::<Quantity>::cast(ctx, lhs).unwrap();
                (lhs, rhs)
            }
            BinaryCoercion::FloatOrLeft => {
                if rhs.is_float() {
                    let lhs = to_ty(ctx, lhs.into(), rhs.ty());
                    let lhs = CastInto::<Quantity>::cast(ctx, lhs).unwrap();
                    (lhs, rhs)
                } else {
                    let rhs = to_ty(ctx, rhs.into(), lhs.ty());
                    let rhs = CastInto::<Quantity>::cast(ctx, rhs).unwrap();
                    (lhs, rhs)
                }
            }
            BinaryCoercion::IntOrLeft => {
                if rhs.is_int() {
                    let lhs = to_ty(ctx, lhs.into(), rhs.ty());
                    let lhs = CastInto::<Quantity>::cast(ctx, lhs).unwrap();
                    (lhs, rhs)
                } else {
                    let rhs = to_ty(ctx, rhs.into(), lhs.ty());
                    let rhs = CastInto::<Quantity>::cast(ctx, rhs).unwrap();
                    (lhs, rhs)
                }
            }
        }
    }
}
