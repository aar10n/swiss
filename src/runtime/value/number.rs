use super::{Context, Exception, Ty, Value};

use crate::ast::{BinaryCoercion, FloatConversion};
use crate::print::ansi::{NUMBER, RESET};
use crate::print::{PrettyPrint, PrettyString};

use rug::{ops::Pow, Float, Integer};
use std::cmp::PartialOrd;

macro_rules! impl_safe_cmp_op {
    ($ctx: ident, $a:ident, $b:ident, $op: ident) => {
        match ($a, $b) {
            (Number::Int(a), Number::Int(b)) => Ok(Number::Int(Integer::from(a.$op(&b)))),
            (Number::Float(a), Number::Float(b)) => Ok(Number::Int(Integer::from(a.$op(&b)))),
            (Number::Int(a), Number::Float(b)) => Ok(Number::Int(Integer::from(
                Float::with_val($ctx.config.float_precision, a).$op(&b),
            ))),
            (Number::Float(a), Number::Int(b)) => Ok(Number::Int(Integer::from(
                a.$op(&Float::with_val($ctx.config.float_precision, b)),
            ))),
        }
    };
}

/// Number is a numeric value.
#[derive(Clone, Debug)]
pub enum Number {
    Int(Integer),
    Float(Float),
}

impl Number {
    pub fn zero() -> Self {
        Number::Int(Integer::new())
    }

    pub fn one() -> Self {
        Number::Int(Integer::from(1))
    }

    pub fn nan(prec: u32) -> Self {
        Number::Float(Float::with_val(prec, rug::float::Special::Nan))
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Number::Int(_))
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Number::Float(_))
    }

    pub fn is_zero(&self) -> bool {
        match &self {
            Number::Int(v) => v.is_zero(),
            Number::Float(v) => v.is_zero(),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !self.is_zero()
    }

    pub fn get_int(&self) -> Option<&Integer> {
        match self {
            Number::Int(v) => Some(v),
            Number::Float(_) => None,
        }
    }

    pub fn get_float(&self) -> Option<&Float> {
        match self {
            Number::Int(_) => None,
            Number::Float(v) => Some(v),
        }
    }

    pub fn ty(&self) -> Ty {
        match self {
            Number::Int(_) => Ty::Int,
            Number::Float(_) => Ty::Float,
        }
    }

    pub fn to_display_string(&self, ctx: &Context) -> String {
        self.pretty_string(ctx)
    }

    pub fn into_int(self, ctx: &Context) -> Result<Integer, Exception> {
        match self {
            Number::Int(v) => Ok(v),
            Number::Float(v) => (match ctx.config.float_conversion {
                FloatConversion::Trunc => v.trunc().to_integer(),
                FloatConversion::Round => v.round().to_integer(),
            })
            .ok_or_else(|| {
                Exception::new(
                    "TypeError",
                    "rounding failed during conversion to int".to_owned(),
                )
            }),
        }
    }

    pub fn into_float(self, ctx: &Context) -> Result<Float, Exception> {
        match self {
            Number::Int(v) => Ok(Float::with_val(ctx.config.float_precision, v)),
            Number::Float(v) => Ok(v),
        }
    }
}

//
// MARK: Number Operations
//

impl Number {
    // MARK: Arithmetic Functions

    pub fn safe_add(ctx: &Context, a: Number, b: Number) -> Result<Number, Exception> {
        match (a, b) {
            (Number::Int(a), Number::Int(b)) => Ok(Number::Int(a + b)),
            (Number::Float(a), Number::Float(b)) => Ok(Number::Float(a + b)),
            (Number::Int(a), Number::Float(b)) => Ok(Number::Float(
                Float::with_val(ctx.config.float_precision, a) + b,
            )),
            (Number::Float(a), Number::Int(b)) => Ok(Number::Float(
                a + Float::with_val(ctx.config.float_precision, b),
            )),
        }
    }

    pub fn safe_sub(ctx: &Context, a: Number, b: Number) -> Result<Number, Exception> {
        match (a, b) {
            (Number::Int(a), Number::Int(b)) => Ok(Number::Int(a - b)),
            (Number::Float(a), Number::Float(b)) => Ok(Number::Float(a - b)),
            (Number::Int(a), Number::Float(b)) => Ok(Number::Float(
                Float::with_val(ctx.config.float_precision, a) - b,
            )),
            (Number::Float(a), Number::Int(b)) => Ok(Number::Float(
                a - Float::with_val(ctx.config.float_precision, b),
            )),
        }
    }

    pub fn safe_mul(ctx: &Context, a: Number, b: Number) -> Result<Number, Exception> {
        match (a, b) {
            (Number::Int(a), Number::Int(b)) => Ok(Number::Int(a * b)),
            (Number::Float(a), Number::Float(b)) => Ok(Number::Float(a * b)),
            (Number::Int(a), Number::Float(b)) => Ok(Number::Float(
                Float::with_val(ctx.config.float_precision, a) * b,
            )),
            (Number::Float(a), Number::Int(b)) => Ok(Number::Float(
                a * Float::with_val(ctx.config.float_precision, b),
            )),
        }
    }

    pub fn safe_div(ctx: &Context, a: Number, b: Number) -> Result<Number, Exception> {
        if b.is_zero() {
            return Err(Exception::new("ValueError", "division by zero".to_owned())
                .with_backtrace(ctx.backtrace()));
        }

        match (a, b) {
            (Number::Int(a), Number::Int(b)) => {
                if a.is_divisible(&b) {
                    Ok(Number::Int(a / b))
                } else {
                    Ok(Number::Float(
                        Float::with_val(ctx.config.float_precision, a)
                            / Float::with_val(ctx.config.float_precision, b),
                    ))
                }
            }
            (Number::Float(a), Number::Float(b)) => Ok(Number::Float(a / b)),
            (Number::Int(a), Number::Float(b)) => Ok(Number::Float(
                Float::with_val(ctx.config.float_precision, a) / b,
            )),
            (Number::Float(a), Number::Int(b)) => Ok(Number::Float(
                a / Float::with_val(ctx.config.float_precision, b),
            )),
        }
    }

    pub fn safe_mod(ctx: &Context, a: Number, b: Number) -> Result<Number, Exception> {
        if b.is_zero() {
            return Ok(Number::nan(ctx.config.float_precision));
        }

        match (a, b) {
            (Number::Int(a), Number::Int(b)) => Ok(Number::Int(a % b)),
            (Number::Float(a), Number::Float(b)) => Ok(Number::Float(a % b)),
            (Number::Int(a), Number::Float(b)) => Ok(Number::Float(
                Float::with_val(ctx.config.float_precision, a) % b,
            )),
            (Number::Float(a), Number::Int(b)) => Ok(Number::Float(
                a % Float::with_val(ctx.config.float_precision, b),
            )),
        }
    }

    pub fn safe_pow(ctx: &Context, a: Number, b: Number) -> Result<Number, Exception> {
        match (a, b) {
            (Number::Int(a), Number::Int(b)) => {
                if b < 0 {
                    let prec = ctx.config.float_precision;
                    Ok(Number::Float(
                        Float::with_val(prec, a).pow(Float::with_val(prec, -b)),
                    ))
                } else {
                    Ok(Number::Int(a.pow(b.to_u32().unwrap())))
                }
            }
            (Number::Float(a), Number::Float(b)) => Ok(Number::Float(a.pow(b))),
            (Number::Int(a), Number::Float(b)) => Ok(Number::Float(
                Float::with_val(ctx.config.float_precision, a).pow(b),
            )),
            (Number::Float(a), Number::Int(b)) => Ok(Number::Float(
                a.pow(Float::with_val(ctx.config.float_precision, b)),
            )),
        }
    }

    // MARK: Bitwise Functions

    pub fn safe_bit_not(ctx: &Context, a: Number) -> Result<Number, Exception> {
        let a = a.into_int(ctx)?;
        Ok(Number::Int(!a))
    }

    pub fn safe_bit_and(ctx: &Context, a: Number, b: Number) -> Result<Number, Exception> {
        let a = a.into_int(ctx)?;
        let b = b.into_int(ctx)?;
        Ok(Number::Int(a & b))
    }

    pub fn safe_bit_or(ctx: &Context, a: Number, b: Number) -> Result<Number, Exception> {
        let a = a.into_int(ctx)?;
        let b = b.into_int(ctx)?;
        Ok(Number::Int(a | b))
    }

    pub fn safe_bit_xor(ctx: &Context, a: Number, b: Number) -> Result<Number, Exception> {
        let a = a.into_int(ctx)?;
        let b = b.into_int(ctx)?;
        Ok(Number::Int(a ^ b))
    }

    pub fn safe_bit_shl(ctx: &Context, a: Number, b: Number) -> Result<Number, Exception> {
        Number::safe_bit_shift(ctx, a, b, false)
    }

    pub fn safe_bit_shr(ctx: &Context, a: Number, b: Number) -> Result<Number, Exception> {
        Number::safe_bit_shift(ctx, a, b, true)
    }

    fn safe_bit_shift(
        ctx: &Context,
        a: Number,
        b: Number,
        shift_right: bool,
    ) -> Result<Number, Exception> {
        let Some(b) = b.get_int().cloned() else {
            return Err(
                Exception::new("TypeError", format!("non-integer shift count"))
                    .with_backtrace(ctx.backtrace()),
            );
        };
        if b < 0 {
            return Err(
                Exception::new("ValueError", format!("negative shift count"))
                    .with_backtrace(ctx.backtrace()),
            );
        }

        let shift = b.to_u32().ok_or_else(|| {
            Exception::new("ValueError", format!("shift count too large"))
                .with_backtrace(ctx.backtrace())
        })?;
        let integer = match a {
            Number::Int(i) => Ok(i),
            _ => Err(Exception::new(
                "TypeError",
                format!(
                    "expected integer, got {} ({})",
                    a.pretty_string(ctx),
                    a.ty().pretty_string(ctx)
                ),
            )
            .with_backtrace(ctx.backtrace())),
        }?;

        Ok(Number::Int(if shift_right {
            integer >> shift
        } else {
            integer << shift
        }))
    }

    // MARK: Logical functions

    pub fn safe_not(ctx: &Context, a: Number) -> Result<Number, Exception> {
        Ok(Number::Int(Integer::from(a.is_zero())))
    }

    pub fn safe_and(ctx: &Context, a: Number, b: Number) -> Result<Number, Exception> {
        Ok(Number::Int(Integer::from(a.is_truthy() && b.is_truthy())))
    }

    pub fn safe_or(ctx: &Context, a: Number, b: Number) -> Result<Number, Exception> {
        Ok(Number::Int(Integer::from(a.is_truthy() || b.is_truthy())))
    }

    // MARK: Comparison functions

    pub fn safe_eq(ctx: &Context, a: Number, b: Number) -> Result<Number, Exception> {
        Ok(Number::Int(Integer::from(a == b)))
    }

    pub fn safe_ne(ctx: &Context, a: Number, b: Number) -> Result<Number, Exception> {
        Ok(Number::Int(Integer::from(a != b)))
    }

    pub fn safe_lt(ctx: &Context, a: Number, b: Number) -> Result<Number, Exception> {
        impl_safe_cmp_op!(ctx, a, b, lt)
    }

    pub fn safe_le(ctx: &Context, a: Number, b: Number) -> Result<Number, Exception> {
        impl_safe_cmp_op!(ctx, a, b, le)
    }

    pub fn safe_gt(ctx: &Context, a: Number, b: Number) -> Result<Number, Exception> {
        impl_safe_cmp_op!(ctx, a, b, gt)
    }

    pub fn safe_ge(ctx: &Context, a: Number, b: Number) -> Result<Number, Exception> {
        impl_safe_cmp_op!(ctx, a, b, ge)
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.to_string() == other.to_string()
    }
}

impl Eq for Number {}

impl ToString for Number {
    fn to_string(&self) -> String {
        match self {
            Number::Int(v) => v.to_string(),
            Number::Float(v) => v.to_string(),
        }
    }
}

impl PrettyPrint<Context> for Number {
    fn pretty_print<Output: std::io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> std::io::Result<()> {
        write!(out, "{NUMBER}")?;
        match self {
            Number::Int(v) => write!(out, "{}", v),
            Number::Float(v) => {
                if v.is_zero() {
                    write!(out, "{}", if v.is_sign_negative() { "-0" } else { "0" })
                } else if v.is_infinite() {
                    write!(out, "{}", if v.is_sign_negative() { "-Inf" } else { "Inf" })
                } else if v.is_nan() {
                    write!(out, "{}", if v.is_sign_negative() { "-NaN" } else { "NaN" })
                } else if v.is_integer() {
                    write!(out, "{}", v.to_integer().unwrap_or_else(|| Integer::new()))
                } else if let Some(num_places) = ctx.config.decimal_places {
                    if v.prec() <= 53 {
                        let v = v.to_f64(); // `f64` has a precision of 53
                        write!(out, "{:.*}", num_places as usize, v)
                    } else {
                        write!(out, "{}", v.to_string_radix(10, Some(num_places as usize)))
                    }
                } else if v.prec() <= 53 {
                    write!(out, "{}", v.to_f64())
                } else {
                    write!(out, "{}", v)
                }
            }
        }?;
        write!(out, "{RESET}")
    }
}

impl From<Integer> for Number {
    fn from(value: Integer) -> Self {
        Number::Int(value)
    }
}

impl From<Float> for Number {
    fn from(value: Float) -> Self {
        Number::Float(value)
    }
}

impl From<bool> for Number {
    fn from(value: bool) -> Self {
        if value {
            Number::Int(Integer::from(1))
        } else {
            Number::Int(Integer::from(0))
        }
    }
}
