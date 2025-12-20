use super::{Context, Dim, DimExpr, Exception, Number, VarId};
use crate::id::ModuleId;

use crate::ast::ListNode;
pub use crate::ast::{NodeId, UnitKind};
use crate::interp::{InterpError, Interpreter, Value};
use crate::print::ansi::{RESET, UNIT};
use crate::print::{PrettyPrint, PrettyString};
use crate::runtime::Function;
use crate::source::{SourceSpan, Spanned};

use either::{Either, Left, Right};
use std::cell::{Ref, RefCell};
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
    pub conversion: Conversion,
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
            conversion: Conversion::Scale(scale),
        }
    }

    pub fn with_conversion(
        kind: UnitKind,
        name: Spanned<Ustr>,
        suffixes: Vec<Spanned<Ustr>>,
        dim_expr: DimExpr,
        conversion: Conversion,
    ) -> Self {
        Self {
            kind,
            name,
            suffixes,
            dim_expr,
            conversion,
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

    pub fn as_dim(&self) -> Dim {
        Dim::simple(
            self.dim_expr.clone(),
            self.name.raw,
            self.conversion.clone(),
        )
    }

    pub fn normalize(self) -> Self {
        let dim_expr = self.dim_expr.normalized();
        Self { dim_expr, ..self }
    }

    pub fn normalized(&self) -> Self {
        self.clone().normalize()
    }
}

/// A units conversion strategy.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Conversion {
    Scale(Number),
    Impl(UnitImpl),
}

impl Conversion {
    pub fn to_base(&self, ctx: &mut Context, x: Number) -> Result<Number, InterpError> {
        match self {
            Conversion::Scale(s) => Number::safe_mul(ctx, x, s.clone()).map_err(|e| e.into()),
            Conversion::Impl(i) => i.to_base(ctx, x),
        }
    }

    pub fn from_base(&self, ctx: &mut Context, x: Number) -> Result<Number, InterpError> {
        match self {
            Conversion::Scale(s) => Number::safe_div(ctx, x, s.clone()).map_err(|e| e.into()),
            Conversion::Impl(i) => i.from_base(ctx, x),
        }
    }
}

/// A units conversion implementation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnitImpl {
    pub module_id: ModuleId,
    pub to_base: VarId,
    pub from_base: VarId,
    pub display_name: Option<VarId>,
}

impl UnitImpl {
    pub fn new(
        module_id: ModuleId,
        to_base: VarId,
        from_base: VarId,
        display_name: Option<VarId>,
    ) -> Self {
        Self {
            module_id,
            to_base,
            from_base,
            display_name,
        }
    }

    fn to_base(&self, ctx: &mut Context, x: Number) -> Result<Number, InterpError> {
        Context::with_active_module(ctx, self.module_id, |ctx| {
            let mut interp = Interpreter::new(ctx);
            let value = interp.call_by_id(self.to_base, vec![Value::Quantity(x.into())])?;
            match value {
                Value::Quantity(q) => Ok(q.number),
                _ => Err(InterpError::from(Exception::new(
                    "TypeError",
                    format!(
                        "unit conversion function did not return a number, got {}",
                        value.pretty_string(&interp.ctx)
                    ),
                ))),
            }
        })
    }

    fn from_base(&self, ctx: &mut Context, x: Number) -> Result<Number, InterpError> {
        Context::with_active_module(ctx, self.module_id, |ctx| {
            let mut interp = Interpreter::new(ctx);
            let value = interp.call_by_id(self.from_base, vec![Value::Quantity(x.into())])?;
            match value {
                Value::Quantity(q) => Ok(q.number),
                _ => Err(InterpError::from(Exception::new(
                    "TypeError",
                    format!(
                        "unit conversion function did not return a number, got {}",
                        value.pretty_string(&interp.ctx)
                    ),
                ))),
            }
        })
    }

    pub fn display_name(&self, ctx: &mut Context) -> Result<Option<String>, InterpError> {
        match self.display_name {
            Some(display_name_id) => Context::with_active_module(ctx, self.module_id, |ctx| {
                let mut interp = Interpreter::new(ctx);
                let value = interp.call_by_id(display_name_id, vec![])?;
                match value {
                    Value::String(s) => Ok(Some(s.to_string())),
                    _ => Err(InterpError::from(Exception::new(
                        "TypeError",
                        format!(
                            "unit display_name function did not return a string, got {}",
                            value.pretty_string(&interp.ctx)
                        ),
                    ))),
                }
            }),
            None => Ok(None),
        }
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

    pub fn iter(&self) -> impl Iterator<Item = &Unit> {
        self.units.values()
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
