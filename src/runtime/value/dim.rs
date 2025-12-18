use super::super::Conversion;
use super::{Context, Exception, Number};

use crate::ast::{UnitPreference, P};
use crate::print::{PrettyPrint, PrettyString};

use static_init::dynamic;
use std::cmp::Ordering;
use ustr::Ustr;

#[dynamic]
pub static NONE_DIM: Dim = Dim::new(DimExpr::one(), None);

// MARK: UnitInfo

/// Unit information that can be simple or compound
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnitInfo {
    /// A simple unit with a name and conversion
    Simple(Ustr, Conversion),
    /// A compound unit: lhs * rhs
    Mul(Box<UnitInfo>, Box<UnitInfo>),
    /// A compound unit: lhs / rhs
    Div(Box<UnitInfo>, Box<UnitInfo>),
    /// A compound unit: base ^ exponent
    Pow(Box<UnitInfo>, i32),
}

impl UnitInfo {
    /// Get the unit name for simple units, or None for compound units
    pub fn simple_unit_name(&self) -> Option<Ustr> {
        match self {
            UnitInfo::Simple(name, _) => Some(*name),
            _ => None,
        }
    }

    /// Get the conversion for simple units
    pub fn simple_conversion(&self) -> Option<&Conversion> {
        match self {
            UnitInfo::Simple(_, conv) => Some(conv),
            _ => None,
        }
    }

    /// Simplify compound units by combining repeated units into powers
    pub fn simplify(self) -> Self {
        use std::collections::HashMap;

        // Helper to collect units with their exponents
        // Maps unit name -> (count, conversion)
        fn collect_units(
            unit: &UnitInfo,
            numerator: &mut HashMap<Ustr, (i32, Conversion)>,
            denominator: &mut HashMap<Ustr, (i32, Conversion)>,
            in_denominator: bool,
        ) {
            match unit {
                UnitInfo::Simple(name, conv) => {
                    if name.is_empty() {
                        return; // Skip empty placeholder units
                    }
                    if in_denominator {
                        let entry = denominator.entry(*name).or_insert((0, conv.clone()));
                        entry.0 += 1;
                    } else {
                        let entry = numerator.entry(*name).or_insert((0, conv.clone()));
                        entry.0 += 1;
                    }
                }
                UnitInfo::Mul(a, b) => {
                    collect_units(a, numerator, denominator, in_denominator);
                    collect_units(b, numerator, denominator, in_denominator);
                }
                UnitInfo::Div(a, b) => {
                    collect_units(a, numerator, denominator, in_denominator);
                    collect_units(b, numerator, denominator, !in_denominator);
                }
                UnitInfo::Pow(base, exp) => {
                    // Flatten powers
                    if let UnitInfo::Simple(name, conv) = base.as_ref() {
                        if name.is_empty() {
                            return;
                        }
                        if in_denominator {
                            let entry = denominator.entry(*name).or_insert((0, conv.clone()));
                            entry.0 += exp;
                        } else {
                            let entry = numerator.entry(*name).or_insert((0, conv.clone()));
                            entry.0 += exp;
                        }
                    } else {
                        // Complex nested power - just collect recursively
                        for _ in 0..*exp {
                            collect_units(base, numerator, denominator, in_denominator);
                        }
                    }
                }
            }
        }

        // Collect all units
        let mut numerator = HashMap::new();
        let mut denominator = HashMap::new();
        collect_units(&self, &mut numerator, &mut denominator, false);

        // Compute net exponents (numerator - denominator)
        let mut net_exponents = HashMap::new();
        for (name, (count, conv)) in numerator {
            let denom_count = denominator.get(&name).map(|(c, _)| *c).unwrap_or(0);
            let net = count - denom_count;
            if net != 0 {
                net_exponents.insert(name, (net, conv));
            }
        }
        for (name, (count, conv)) in denominator {
            if !net_exponents.contains_key(&name) && count != 0 {
                net_exponents.insert(name, (-count, conv));
            }
        }

        // Build simplified unit
        let mut positive_units = Vec::new();
        let mut negative_units = Vec::new();

        for (name, (exp, conv)) in net_exponents {
            let unit = UnitInfo::Simple(name, conv);
            if exp > 0 {
                positive_units.push((unit, exp));
            } else if exp < 0 {
                negative_units.push((unit, -exp));
            }
        }

        // Sort for consistent output
        positive_units.sort_by(|a, b| a.0.simple_unit_name().cmp(&b.0.simple_unit_name()));
        negative_units.sort_by(|a, b| a.0.simple_unit_name().cmp(&b.0.simple_unit_name()));

        // Build the result
        let numerator_unit = Self::build_product(positive_units);
        let denominator_unit = Self::build_product(negative_units);

        match (numerator_unit, denominator_unit) {
            (Some(num), Some(denom)) => UnitInfo::Div(Box::new(num), Box::new(denom)),
            (Some(num), None) => num,
            (None, Some(denom)) => UnitInfo::Div(
                Box::new(UnitInfo::Simple(Ustr::from(""), Conversion::Scale(Number::Int(1.into())))),
                Box::new(denom)
            ),
            (None, None) => UnitInfo::Simple(Ustr::from(""), Conversion::Scale(Number::Int(1.into()))),
        }
    }

    /// Build a product of units with exponents
    fn build_product(units: Vec<(UnitInfo, i32)>) -> Option<UnitInfo> {
        if units.is_empty() {
            return None;
        }

        let mut result = None;
        for (unit, exp) in units {
            let powered = if exp == 1 {
                unit
            } else {
                UnitInfo::Pow(Box::new(unit), exp)
            };

            result = Some(match result {
                None => powered,
                Some(acc) => UnitInfo::Mul(Box::new(acc), Box::new(powered)),
            });
        }
        result
    }
}

impl ToString for UnitInfo {
    fn to_string(&self) -> String {
        match self {
            UnitInfo::Simple(name, _) => name.to_string(),
            UnitInfo::Mul(a, b) => {
                // Space-separated for multiplication
                format!("{} {}", a.to_string(), b.to_string())
            }
            UnitInfo::Div(a, b) => {
                match (a.as_ref(), b.as_ref()) {
                    (UnitInfo::Simple(name, _), _) if name.is_empty() => {
                        // Handle 1 / unit case
                        format!("1 / {}", b.to_string())
                    }
                    _ => format!("{} / {}", a.to_string(), b.to_string())
                }
            }
            UnitInfo::Pow(base, exp) => {
                // Use superscript notation for exponents
                format!("{}^{}", base.to_string(), exp)
            }
        }
    }
}

// MARK: Dim

/// A dimension contains a dimensional expression and optional unit information.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Dim {
    pub expr: DimExpr,
    pub unit: Option<UnitInfo>,
}

impl Dim {
    pub fn new(expr: DimExpr, unit: Option<UnitInfo>) -> Self {
        Self {
            expr: expr.normalize(),
            unit,
        }
    }

    pub fn simple(expr: DimExpr, unit: Ustr, conversion: Conversion) -> Self {
        Self::new(expr, Some(UnitInfo::Simple(unit, conversion)))
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

    /// Multiply two dimensions, creating a compound dimension
    pub fn mul(a: Dim, b: Dim) -> Self {
        // If both are dimensionless, result is dimensionless
        if a.is_none() && b.is_none() {
            return Dim::none();
        }

        let expr = DimExpr::Mul(P::new(a.expr), P::new(b.expr));
        let unit = match (a.unit, b.unit) {
            (Some(a_unit), Some(b_unit)) => {
                // Create compound and simplify
                let compound = UnitInfo::Mul(Box::new(a_unit), Box::new(b_unit));
                Some(compound.simplify())
            }
            (Some(u), None) | (None, Some(u)) => Some(u),
            (None, None) => None,
        };
        Self::new(expr, unit)
    }

    /// Divide two dimensions, creating a compound dimension
    pub fn div(a: Dim, b: Dim) -> Self {
        // If both are dimensionless, result is dimensionless
        if a.is_none() && b.is_none() {
            return Dim::none();
        }

        let expr = DimExpr::Div(P::new(a.expr), P::new(b.expr));
        let unit = match (a.unit, b.unit) {
            (Some(a_unit), Some(b_unit)) => {
                // Create compound and simplify
                let compound = UnitInfo::Div(Box::new(a_unit), Box::new(b_unit));
                Some(compound.simplify())
            }
            (Some(u), None) => Some(u),
            (None, Some(u)) => {
                let compound = UnitInfo::Div(
                    Box::new(UnitInfo::Simple(Ustr::from(""), Conversion::Scale(Number::Int(1.into())))),
                    Box::new(u)
                );
                Some(compound.simplify())
            }
            (None, None) => None,
        };
        Self::new(expr, unit)
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
                "TypeError",
                format!(
                    "dimension mismatch: {} != {}",
                    a.pretty_string(ctx),
                    b.pretty_string(ctx)
                ),
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
            Some(unit) => unit.to_string(),
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
        Self::new(expr, Some(UnitInfo::Simple(unit, Conversion::Scale(scale))))
    }
}

impl From<(DimExpr, Ustr, Conversion)> for Dim {
    fn from((expr, unit, conv): (DimExpr, Ustr, Conversion)) -> Self {
        Self::new(expr, Some(UnitInfo::Simple(unit, conv)))
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
            Some(unit) => write!(out, "{}", unit.to_string()),
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
