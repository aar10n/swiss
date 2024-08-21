use super::{Context, Dim, DimExpr, Exception, Number, Ty, Value};

use crate::print::ansi::{
    chars::{LBRAC, RBRAC},
    NUMBER, RESET, UNIT,
};
use crate::print::{PrettyPrint, PrettyString};
use crate::source::Spanned;

use rug::ops::Pow;
use rug::{Float, Integer};
use std::cmp::Ordering;
use std::ops::Neg;
use ustr::Ustr;

// MARK: Quantity

/// Quantity is a numeric value with a dimensional component.
#[derive(Clone, Debug)]
pub struct Quantity {
    pub number: Number,
    pub dim: Dim,
}

impl Quantity {
    pub const fn new(number: Number, dim: Dim) -> Self {
        Self { number, dim }
    }

    pub const fn int(number: Integer, dim: Dim) -> Self {
        Self::new(Number::Int(number), dim)
    }

    pub const fn float(number: Float, dim: Dim) -> Self {
        Self::new(Number::Float(number), dim)
    }

    pub fn with_dim(self, dim: Dim) -> Self {
        Self::new(self.number, dim)
    }

    pub fn with_dim_unified(self, ctx: &Context, other_dim: Dim) -> Result<Self, Exception> {
        let (num, dim) = self.into_tuple();
        Ok(Quantity::new(num, Dim::unify(ctx, dim, other_dim)?))
    }

    pub fn zero() -> Self {
        Self::new(Number::zero(), Dim::none())
    }

    pub fn one() -> Self {
        Self::new(Number::one(), Dim::none())
    }

    //

    pub fn is_dimless(&self) -> bool {
        self.dim.is_none()
    }

    pub fn is_int(&self) -> bool {
        matches!(&self.number, Number::Int(_))
    }

    pub fn is_float(&self) -> bool {
        matches!(&self.number, Number::Float(_))
    }

    pub fn is_zero(&self) -> bool {
        self.number.is_zero()
    }

    pub fn is_truthy(&self) -> bool {
        self.number.is_truthy()
    }

    pub fn get_unit(&self) -> Option<&Ustr> {
        self.dim.unit.as_ref().map(|(unit, _)| unit)
    }

    pub fn get_scaled(&self, ctx: &Context) -> Result<Number, Exception> {
        if let Some((_, scale)) = &self.dim.unit {
            Number::safe_div(ctx, self.number.clone(), scale.clone())
        } else {
            Ok(self.number.clone())
        }
    }

    pub fn ty(&self) -> Ty {
        if self.is_dimless() {
            if self.is_int() {
                Ty::Int
            } else {
                Ty::Float
            }
        } else {
            Ty::Dim(self.dim.clone())
        }
    }

    pub fn to_display_string(&self, ctx: &Context) -> String {
        self.pretty_string(ctx)
    }

    pub fn into_tuple(self) -> (Number, Dim) {
        (self.number, self.dim)
    }

    pub fn into_tuple_with_ty(self) -> (Number, Dim, Ty) {
        let ty = self.ty();
        let (number, dim) = self.into_tuple();
        (number, dim, ty)
    }
}

//
// Quantity Operations
//

impl Quantity {
    // MARK: Arithmetic Functions

    pub fn safe_add(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, a_dim) = a.into_tuple();
        let (b, b_dim) = b.into_tuple();

        let dim = Dim::unify(ctx, a_dim, b_dim)?;
        let number = Number::safe_add(ctx, a, b)?;
        Ok(Quantity::new(number, dim))
    }

    pub fn safe_sub(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, a_dim) = a.into_tuple();
        let (b, b_dim) = b.into_tuple();

        let dim = Dim::unify(ctx, a_dim, b_dim)?;
        let number = Number::safe_sub(ctx, a, b)?;
        Ok(Quantity::new(number, dim))
    }

    pub fn safe_mul(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, a_dim) = a.into_tuple();
        let (b, b_dim) = b.into_tuple();

        let dim = Dim::unify(ctx, a_dim, b_dim)?;
        let number = Number::safe_mul(ctx, a, b)?;
        Ok(Quantity::new(number, dim))
    }

    pub fn safe_div(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, a_dim) = a.into_tuple();
        let (b, b_dim) = b.into_tuple();

        let dim = Dim::unify(ctx, a_dim, b_dim)?;
        let number = Number::safe_div(ctx, a, b)?;
        Ok(Quantity::new(number, dim))
    }

    pub fn safe_mod(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, a_dim) = a.into_tuple();
        let (b, b_dim) = b.into_tuple();

        Dim::unify(ctx, a_dim, b_dim)?;
        let number = Number::safe_mod(ctx, a, b)?;
        Ok(Quantity::new(number, Dim::none()))
    }

    pub fn safe_pow(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, a_dim) = a.into_tuple();
        let (b, b_dim) = b.into_tuple();

        let dim = Dim::unify(ctx, a_dim, b_dim)?;
        let number = Number::safe_pow(ctx, a, b)?;
        Ok(Quantity::new(number, dim))
    }

    // MARK: Bitwise Functions

    pub fn safe_bit_not(ctx: &Context, a: Quantity) -> Result<Quantity, Exception> {
        let (number, dim) = a.into_tuple();
        let number = Number::safe_bit_not(ctx, number)?;
        Ok(Quantity::new(number, dim))
    }

    pub fn safe_bit_and(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, a_dim) = a.into_tuple();
        let (b, b_dim) = b.into_tuple();

        let dim = Dim::unify(ctx, a_dim, b_dim)?;
        let number = Number::safe_bit_and(ctx, a, b)?;
        Ok(Quantity::new(number, dim))
    }

    pub fn safe_bit_or(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, a_dim) = a.into_tuple();
        let (b, b_dim) = b.into_tuple();

        let dim = Dim::unify(ctx, a_dim, b_dim)?;
        let number = Number::safe_bit_or(ctx, a, b)?;
        Ok(Quantity::new(number, dim))
    }

    pub fn safe_bit_xor(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, a_dim) = a.into_tuple();
        let (b, b_dim) = b.into_tuple();

        let dim = Dim::unify(ctx, a_dim, b_dim)?;
        let number = Number::safe_bit_xor(ctx, a, b)?;
        Ok(Quantity::new(number, dim))
    }

    pub fn safe_bit_shl(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, a_dim) = a.into_tuple();
        let (b, b_dim) = b.into_tuple();

        let dim = Dim::unify(ctx, a_dim, b_dim)?;
        let number = Number::safe_bit_shl(ctx, a, b)?;
        Ok(Quantity::new(number, dim))
    }

    pub fn safe_bit_shr(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, a_dim) = a.into_tuple();
        let (b, b_dim) = b.into_tuple();

        let dim = Dim::unify(ctx, a_dim, b_dim)?;
        let number = Number::safe_bit_shr(ctx, a, b)?;
        Ok(Quantity::new(number, dim))
    }

    // MARK: Logical functions

    pub fn safe_not(ctx: &Context, a: Quantity) -> Result<Quantity, Exception> {
        let (number, _) = a.into_tuple();
        let number = Number::safe_not(ctx, number)?;
        Ok(Quantity::new(number, Dim::none()))
    }

    pub fn safe_and(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, a_dim) = a.into_tuple();
        let (b, b_dim) = b.into_tuple();

        Dim::unify(ctx, a_dim, b_dim)?;
        let number = Number::safe_and(ctx, a, b)?;
        Ok(Quantity::new(number, Dim::none()))
    }

    pub fn safe_or(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, a_dim) = a.into_tuple();
        let (b, b_dim) = b.into_tuple();

        Dim::unify(ctx, a_dim, b_dim)?;
        let number = Number::safe_or(ctx, a, b)?;
        Ok(Quantity::new(number, Dim::none()))
    }

    // MARK: Comparison Functions

    pub fn safe_eq(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, a_dim) = a.into_tuple();
        let (b, b_dim) = b.into_tuple();

        Dim::unify(ctx, a_dim, b_dim)?;
        let number = Number::safe_eq(ctx, a, b)?;
        Ok(Quantity::new(number, Dim::none()))
    }

    pub fn safe_ne(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, a_dim) = a.into_tuple();
        let (b, b_dim) = b.into_tuple();

        Dim::unify(ctx, a_dim, b_dim)?;
        let number = Number::safe_ne(ctx, a, b)?;
        Ok(Quantity::new(number, Dim::none()))
    }

    pub fn safe_lt(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, a_dim) = a.into_tuple();
        let (b, b_dim) = b.into_tuple();

        Dim::unify(ctx, a_dim, b_dim)?;
        let number = Number::safe_lt(ctx, a, b)?;
        Ok(Quantity::new(number, Dim::none()))
    }

    pub fn safe_le(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, a_dim) = a.into_tuple();
        let (b, b_dim) = b.into_tuple();

        Dim::unify(ctx, a_dim, b_dim)?;
        let number = Number::safe_le(ctx, a, b)?;
        Ok(Quantity::new(number, Dim::none()))
    }

    pub fn safe_gt(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, a_dim) = a.into_tuple();
        let (b, b_dim) = b.into_tuple();

        Dim::unify(ctx, a_dim, b_dim)?;
        let number = Number::safe_gt(ctx, a, b)?;
        Ok(Quantity::new(number, Dim::none()))
    }

    pub fn safe_ge(ctx: &Context, a: Quantity, b: Quantity) -> Result<Quantity, Exception> {
        let (a, a_dim) = a.into_tuple();
        let (b, b_dim) = b.into_tuple();

        Dim::unify(ctx, a_dim, b_dim)?;
        let number = Number::safe_ge(ctx, a, b)?;
        Ok(Quantity::new(number, Dim::none()))
    }
}

impl Neg for Quantity {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self.number {
            Number::Int(v) => Self::int(-v, self.dim),
            Number::Float(v) => Self::float(-v, self.dim),
        }
    }
}

impl PartialEq for Quantity {
    fn eq(&self, other: &Self) -> bool {
        match &self.number {
            Number::Int(lhs) => match &other.number {
                Number::Int(rhs) => lhs == rhs,
                Number::Float(rhs) => todo!(),
            },
            Number::Float(lhs) => match &other.number {
                Number::Int(rhs) => todo!(),
                Number::Float(rhs) => lhs == rhs,
            },
        }
    }
}

impl PartialOrd for Quantity {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match &self.number {
            Number::Int(lhs) => match &other.number {
                Number::Int(rhs) => lhs.partial_cmp(rhs),
                Number::Float(rhs) => todo!(),
            },
            Number::Float(lhs) => match &other.number {
                Number::Int(rhs) => todo!(),
                Number::Float(rhs) => lhs.partial_cmp(rhs),
            },
        }
    }
}

impl Default for Quantity {
    fn default() -> Self {
        Self {
            number: Number::Int(Integer::new()),
            dim: Dim::none(),
        }
    }
}

impl PrettyPrint<Context> for Quantity {
    fn pretty_print<Output: std::io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> std::io::Result<()> {
        if let Some((unit, scale)) = &self.dim.unit {
            let value = Number::safe_div(ctx, self.number.clone(), scale.clone())
                .unwrap_or_else(|_| Number::nan(ctx.config.float_precision));

            value.pretty_print(out, ctx, level)?;
            write!(out, " {UNIT}{}{RESET}", unit)
        } else if !self.dim.is_none() {
            self.number.pretty_print(out, ctx, level)?;
            write!(out, " {LBRAC}{}{RBRAC}", self.dim.expr.to_string())
        } else {
            self.number.pretty_print(out, ctx, level)
        }
    }
}

impl From<Number> for Quantity {
    fn from(v: Number) -> Self {
        Self::new(v, Dim::none())
    }
}

impl From<Integer> for Quantity {
    fn from(v: Integer) -> Self {
        Self::int(v, Dim::none())
    }
}

impl From<Float> for Quantity {
    fn from(v: Float) -> Self {
        Self::float(v, Dim::none())
    }
}

impl From<(Number, Dim)> for Quantity {
    fn from((number, dim): (Number, Dim)) -> Self {
        Self::new(number, dim)
    }
}

macro_rules! impl_from_integer {
    ($t:ty) => {
        impl From<$t> for Quantity {
            fn from(v: $t) -> Self {
                Self::int(Integer::from(v), Dim::none())
            }
        }
    };
}

impl_from_integer!(i8);
impl_from_integer!(i16);
impl_from_integer!(i32);
impl_from_integer!(i64);
impl_from_integer!(i128);
impl_from_integer!(u8);
impl_from_integer!(u16);
impl_from_integer!(u32);
impl_from_integer!(u64);
impl_from_integer!(u128);
