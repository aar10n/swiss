use super::{Context, Exception, Number};

use crate::ast::{UnitPreference, P};
use crate::print::{PrettyPrint, PrettyString};

use static_init::dynamic;
use std::cmp::Ordering;
use ustr::Ustr;

#[dynamic]
pub static NONE_DIM: Dim = Dim::new(DimExpr::one(), None);

// MARK: Dim

/// A dimension contains a dimensional expression and optional unit information.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Dim {
    pub expr: DimExpr,
    pub unit: Option<(Ustr, Number)>,
}

impl Dim {
    pub fn new(expr: DimExpr, unit: Option<(Ustr, Number)>) -> Self {
        Self {
            expr: expr.normalize(),
            unit,
        }
    }

    pub fn none() -> Self {
        NONE_DIM.clone()
    }

    pub fn none_ref() -> &'static Self {
        &NONE_DIM
    }

    pub fn is_none(&self) -> bool {
        self.expr == DimExpr::one() && self.unit.is_none()
    }
}

impl Dim {
    pub fn unify(ctx: &Context, a: Dim, b: Dim) -> Result<Dim, Exception> {
        if a.is_none() {
            return Ok(b);
        } else if b.is_none() {
            return Ok(a);
        } else if a.expr != b.expr {
            return Err(Exception::new(
                "TypeError: dimension mismatch",
                format!("{} != {}", a.pretty_string(ctx), b.pretty_string(ctx)),
            )
            .with_backtrace(ctx.backtrace()));
        }

        match ctx.config.unit_preference {
            UnitPreference::Left => {
                if a.unit.is_some() {
                    Ok(a)
                } else {
                    Ok(b)
                }
            }
            UnitPreference::Right => {
                if b.unit.is_some() {
                    Ok(b)
                } else {
                    Ok(a)
                }
            }
        }
    }
}

impl ToString for Dim {
    fn to_string(&self) -> String {
        match &self.unit {
            Some(unit) => format!("{}", unit.0),
            None => format!("[{}]", self.expr.to_string()),
        }
    }
}

impl From<DimExpr> for Dim {
    fn from(expr: DimExpr) -> Self {
        Self::new(expr, None)
    }
}

impl From<(DimExpr, Ustr, Number)> for Dim {
    fn from((expr, unit, scale): (DimExpr, Ustr, Number)) -> Self {
        Self::new(expr, Some((unit, scale)))
    }
}

impl PrettyPrint<Context> for Dim {
    fn pretty_print<Output: std::io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> std::io::Result<()> {
        match &self.unit {
            Some(unit) => write!(out, "{}", unit.0),
            None => self.expr.pretty_print(out, ctx, level),
        }
    }
}

// MARK: DimExpr

/// A dimension expression.
#[derive(Clone, Debug)]
pub enum DimExpr {
    Mul(P<DimExpr>, P<DimExpr>),
    Div(P<DimExpr>, P<DimExpr>),
    Pow(P<DimExpr>, P<DimExpr>),
    Neg(P<DimExpr>),
    Dimension(Ustr),
    Number(Number),
}

impl DimExpr {
    pub fn one() -> DimExpr {
        DimExpr::Number(Number::Int(1.into()))
    }

    pub fn to_display_string(&self, ctx: &Context) -> String {
        self.pretty_string(ctx)
    }

    pub fn normalize(self) -> DimExpr {
        match self.clone() {
            DimExpr::Mul(a, b) => {
                let mut a = a.normalize();
                let mut b = b.normalize();
                if a > b {
                    std::mem::swap(&mut a, &mut b);
                }
                DimExpr::Mul(P::new(a), P::new(b))
            }
            DimExpr::Div(a, b) => DimExpr::Div(a.normalize().into(), b.normalize().into()),
            DimExpr::Pow(a, b) => DimExpr::Pow(a.normalize().into(), b.normalize().into()),
            DimExpr::Neg(a) => DimExpr::Neg(a.normalize().into()),
            other => other,
        }
    }

    pub fn normalized(&self) -> DimExpr {
        self.clone().normalize()
    }
}

impl Eq for DimExpr {}

// Define the PartialOrd and Ord traits for DimExpr to allow comparison
impl PartialOrd for DimExpr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for DimExpr {
    fn cmp(&self, other: &Self) -> Ordering {
        use DimExpr::*;
        match (self, other) {
            (Number(n1), Number(n2)) => n1.to_string().cmp(&n2.to_string()),
            (Dimension(d1), Dimension(d2)) => d1.cmp(d2),
            (Neg(a1), Neg(a2)) => a1.cmp(a2),
            (Pow(a1, b1), Pow(a2, b2)) => a1.cmp(a2).then(b1.cmp(b2)),
            (Div(a1, b1), Div(a2, b2)) => a1.cmp(a2).then(b1.cmp(b2)),
            (Mul(a1, b1), Mul(a2, b2)) => a1.cmp(a2).then(b1.cmp(b2)),
            (Number(_), _) => Ordering::Less,
            (_, Number(_)) => Ordering::Greater,
            (Dimension(_), _) => Ordering::Less,
            (_, Dimension(_)) => Ordering::Greater,
            (Neg(_), _) => Ordering::Less,
            (_, Neg(_)) => Ordering::Greater,
            (Pow(_, _), _) => Ordering::Less,
            (_, Pow(_, _)) => Ordering::Greater,
            (Div(_, _), _) => Ordering::Less,
            (_, Div(_, _)) => Ordering::Greater,
        }
    }
}

impl PartialEq for DimExpr {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl ToString for DimExpr {
    fn to_string(&self) -> String {
        match self {
            DimExpr::Mul(lhs, rhs) => format!("{} * {}", lhs.to_string(), rhs.to_string()),
            DimExpr::Div(lhs, rhs) => format!("{} / {}", lhs.to_string(), rhs.to_string()),
            DimExpr::Pow(lhs, rhs) => format!("{} ^ {}", lhs.to_string(), rhs.to_string()),
            DimExpr::Neg(expr) => format!("-{}", expr.to_string()),
            DimExpr::Dimension(dim) => dim.to_string(),
            DimExpr::Number(num) => num.to_string(),
        }
    }
}

impl PrettyPrint<Context> for DimExpr {
    fn pretty_print<Output: std::io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> std::io::Result<()> {
        match self {
            DimExpr::Mul(lhs, rhs) => write!(
                out,
                "{} * {}",
                lhs.pretty_string(ctx),
                rhs.pretty_string(ctx)
            ),
            DimExpr::Div(lhs, rhs) => write!(
                out,
                "{} / {}",
                lhs.pretty_string(ctx),
                rhs.pretty_string(ctx)
            ),
            DimExpr::Pow(lhs, rhs) => write!(
                out,
                "{} ^ {}",
                lhs.pretty_string(ctx),
                rhs.pretty_string(ctx)
            ),
            DimExpr::Neg(expr) => write!(out, "-{}", expr.pretty_string(ctx)),
            DimExpr::Dimension(dim) => write!(out, "{}", dim),
            DimExpr::Number(num) => write!(out, "{}", num.pretty_string(ctx)),
        }
    }
}
