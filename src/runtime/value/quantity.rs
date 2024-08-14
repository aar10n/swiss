use super::{Context, Dim, DimExpr, Exception, Number, Ty, Value};

use crate::print::ansi::{NUMBER, RESET};
use crate::print::{PrettyPrint, PrettyString};

use rug::ops::Pow;
use rug::{Float, Integer};
use std::cmp::Ordering;
use std::ops::Neg;
use ustr::Ustr;

// MARK: Quantity

/// Quantity is a numeric value which may have a dimension.
#[derive(Clone, Debug)]
pub struct Quantity {
    pub number: Number,
    pub dim: Dim,
}

impl Quantity {
    pub fn zero() -> Self {
        Self::int(Integer::new(), Dim::none())
    }

    pub fn one() -> Self {
        Self::int(Integer::from(1), Dim::none())
    }

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

    //

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

    pub fn is_dimless(&self) -> bool {
        self.dim.is_none()
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

    pub fn into_tuple(self) -> (Number, Dim) {
        (self.number, self.dim)
    }

    pub fn into_tuple_with_ty(self) -> (Number, Dim, Ty) {
        let ty = self.ty();
        (self.number, self.dim, ty)
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
                Number::Float(rhs) => Float::with_val(0, lhs) == *rhs,
            },
            Number::Float(lhs) => match &other.number {
                Number::Int(rhs) => *lhs == Float::with_val(0, rhs),
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
                Number::Float(rhs) => Float::with_val(0, lhs).partial_cmp(rhs),
            },
            Number::Float(lhs) => match &other.number {
                Number::Int(rhs) => lhs.partial_cmp(&Float::with_val(0, rhs)),
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

impl ToString for Quantity {
    fn to_string(&self) -> String {
        match &self.number {
            Number::Int(v) => v.to_string(),
            Number::Float(v) => v.to_string(),
        }
    }
}

impl<Ctx> PrettyPrint<Ctx> for Quantity {
    fn pretty_print<Output: std::io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        level: usize,
    ) -> std::io::Result<()> {
        match &self.number {
            Number::Int(v) => write!(out, "{NUMBER}{}{RESET}", v),
            Number::Float(v) => write!(out, "{NUMBER}{}{RESET}", v),
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
