use super::{Dim, Exception, Number, Quantity, Ty};

use crate::print::ansi::{NUMBER, RESET};
use crate::print::{PrettyPrint, PrettyString};

// MARK: Value

#[derive(Clone, Debug)]
pub enum Value {
    Quantity(Quantity),
}

impl Value {
    pub fn is_zero(&self) -> bool {
        match &self {
            Self::Quantity(q) => q.is_zero(),
        }
    }

    pub fn is_truthy(&self) -> bool {
        match &self {
            Self::Quantity(q) => q.is_truthy(),
        }
    }

    pub fn is_float(&self) -> bool {
        match &self {
            Self::Quantity(q) => q.is_float(),
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match &self {
            Self::Quantity(q) => q.is_int(),
            _ => false,
        }
    }

    pub fn ty(&self) -> Ty {
        match &self {
            Self::Quantity(q) => {
                if !q.dim.is_none() {
                    Ty::Dim(q.dim.clone())
                } else if q.number.is_int() {
                    Ty::Int
                } else {
                    Ty::Float
                }
            }
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::Quantity(Quantity::default())
    }
}

impl ToString for Value {
    fn to_string(&self) -> String {
        match &self {
            Self::Quantity(q) => q.to_string(),
        }
    }
}

impl<T: Into<Quantity>> From<T> for Value {
    fn from(value: T) -> Self {
        Self::Quantity(value.into())
    }
}

impl<Ctx> PrettyPrint<Ctx> for Value {
    fn pretty_print<Output: std::io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        level: usize,
    ) -> std::io::Result<()> {
        match &self {
            Self::Quantity(q) => q.pretty_print(out, ctx, level),
        }
    }
}
