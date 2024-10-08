use super::context::Context;
use super::value::{DimExpr, Number};
use super::Exception;

pub use crate::ast::{NodeId, UnitKind};
use crate::print::ansi::{RESET, UNIT};
use crate::print::PrettyPrint;
use crate::source::{SourceSpan, Spanned};

use std::collections::HashMap;
use std::ops::Index;
use ustr::Ustr;

/// A registered unit.
#[derive(Clone, Debug)]
pub struct Unit {
    pub kind: UnitKind,
    pub name: Spanned<Ustr>,
    pub suffixes: Vec<Spanned<Ustr>>,
    pub dim_expr: DimExpr,
    pub scale: Number,
}

impl Unit {
    pub fn new(
        kind: UnitKind,
        name: Spanned<Ustr>,
        suffixes: Vec<Spanned<Ustr>>,
        dim_expr: DimExpr,
        scale: Number,
    ) -> Self {
        Self {
            kind,
            name,
            suffixes,
            dim_expr,
            scale,
        }
    }

    pub fn is_base(&self) -> bool {
        matches!(self.kind, UnitKind::BaseUnit)
    }

    pub fn compatible_with(&self, other: &Option<Unit>) -> bool {
        match other {
            Some(other) => self.dim_expr.normalized() == other.dim_expr.normalized(),
            None => true,
        }
    }

    pub fn normalize(self) -> Self {
        let dim_expr = self.dim_expr.normalized();
        Self { dim_expr, ..self }
    }

    pub fn normalized(&self) -> Self {
        self.clone().normalize()
    }
}

/// A table that tracks base and sub-units.
#[derive(Clone, Debug)]
pub struct UnitTable {
    units: HashMap<Ustr, Unit>,
    dimexprs: HashMap<String, Ustr>,
    suffixes: HashMap<Ustr, Ustr>,
}

impl UnitTable {
    pub fn new() -> Self {
        Self {
            units: HashMap::new(),
            dimexprs: HashMap::new(),
            suffixes: HashMap::new(),
        }
    }

    pub fn insert(&mut self, unit: Unit) {
        self.units.insert(unit.name.raw, unit.clone());
        self.suffixes.insert(unit.name.raw, unit.name.raw);
        for suffix in &unit.suffixes {
            self.suffixes.insert(suffix.raw.clone(), unit.name.raw);
        }

        if unit.is_base() {
            self.dimexprs
                .insert(unit.dim_expr.normalized().to_string(), unit.name.raw);
        }
    }

    pub fn get(&self, name: Ustr) -> Option<&Unit> {
        self.units.get(&name)
    }

    pub fn resolve_suffix(&self, suffix: Ustr) -> Option<&Unit> {
        self.units.get(self.suffixes.get(&suffix)?)
    }

    pub fn resolve_dimexpr(&self, expr: &DimExpr) -> Option<&Unit> {
        self.units
            .get(self.dimexprs.get(&expr.normalized().to_string())?)
    }
}

impl Index<Ustr> for UnitTable {
    type Output = Unit;

    fn index(&self, name: Ustr) -> &Self::Output {
        self.get(name).expect("unit not found")
    }
}
