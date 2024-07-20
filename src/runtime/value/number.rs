use super::{Context, Exception, Ty, Value};

use crate::print::ansi::{NUMBER, RESET};
use crate::print::PrettyPrint;

use rug::{Float, Integer};

/// Number is a numeric value.
#[derive(Clone, Debug)]
pub enum Number {
    Int(Integer),
    Float(Float),
}

impl Number {
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
}

impl ToString for Number {
    fn to_string(&self) -> String {
        match self {
            Number::Int(v) => v.to_string(),
            Number::Float(v) => v.to_string(),
        }
    }
}

impl<Ctx> PrettyPrint<Ctx> for Number {
    fn pretty_print<Output: std::io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        level: usize,
    ) -> std::io::Result<()> {
        match self {
            Number::Int(v) => write!(out, "{NUMBER}{}{RESET}", v),
            Number::Float(v) => write!(out, "{NUMBER}{}{RESET}", v),
        }
    }
}
