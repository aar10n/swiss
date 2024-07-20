use super::dimension::{Dimension, DimensionTable};
use super::name::{Constant, Function, Name, NameResult, NameTable, Param};
use super::operator::{OpAssoc, OpKind, Operator, OperatorTable};
use super::unit::{Unit, UnitKind, UnitTable};
use super::{DeclError, NameError};

use crate::ast::*;
use crate::id::module_id;
pub use crate::id::ModuleId;
use crate::source::{source_id, SourceFile, SourceId, SourceSpan, Spanned};

use smallvec::{smallvec, SmallVec};
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::ops::{Index, IndexMut};
use std::rc::Rc;
use ustr::Ustr;

// MARK: Module

pub struct Module {
    pub id: ModuleId,
    pub name: Ustr,
    pub names: NameTable,
    pub dimensions: DimensionTable,
    pub operators: OperatorTable,
    pub units: UnitTable,
}

impl Module {
    pub fn new(name: Ustr) -> Self {
        Self {
            id: module_id::next(),
            name,
            names: NameTable::new(),
            dimensions: DimensionTable::new(),
            operators: OperatorTable::new(),
            units: UnitTable::new(),
        }
    }

    // Item Registration

    pub fn register_constant(&mut self, constant: Constant) -> Result<(), DeclError> {
        self.names.insert_constant(constant)
    }

    pub fn register_function(&mut self, func: Function) -> Result<(), DeclError> {
        self.names.insert_function(func)
    }

    pub fn with_function(&mut self, func: Function) -> &mut Self {
        self.register_function(func).unwrap();
        self
    }

    pub fn with_operator(&mut self, op: Operator) -> &mut Self {
        self.register_operator(op).unwrap();
        self
    }

    pub fn register_dimension(&mut self, dim: Dimension) -> Result<(), DeclError> {
        if let Some(existing) = self.dimensions.get(dim.name.raw) {
            return Err(DeclError::new(
                "dimension",
                Spanned::new(dim.name.to_string(), dim.name.span),
                existing.name.span,
            ));
        }

        self.dimensions.insert(dim);
        Ok(())
    }

    pub fn register_operator(&mut self, op: Operator) -> Result<(), DeclError> {
        if let Some(existing) = self.operators.get(op.kind, op.name.raw).cloned() {
            return Err(DeclError::new(
                "operator",
                op.name.to_string_inner(),
                existing.name.span,
            ));
        }

        self.operators.insert(op);
        Ok(())
    }

    pub fn register_unit(&mut self, unit: Unit) -> Result<(), DeclError> {
        if let Some(existing) = self.units.get(unit.name.raw) {
            return Err(DeclError::new(
                "unit",
                unit.name.to_string_inner(),
                existing.name.span,
            ));
        }
        for suffix in &unit.suffixes {
            if let Some(existing) = self.units.resolve_suffix(suffix.raw) {
                return Err(DeclError::new(
                    "unit",
                    suffix.to_string_inner(),
                    existing.name.span,
                ));
            }
        }

        self.units.insert(unit);
        Ok(())
    }

    // Item Resolution

    pub fn resolve_constant(&self, name: Spanned<Ustr>) -> Result<&Constant, NameError> {
        match self.names.resolve(&name.raw) {
            NameResult::Constant(constant) => Ok(constant),
            NameResult::Function(_) => {
                Err(NameError::new("expected constant", name.to_string_inner())
                    .with_extra("found function".to_owned()))
            }
            NameResult::Ambiguous(_) => {
                Err(NameError::new("expected constant", name.to_string_inner()))
            }
            NameResult::None => Err(NameError::new("undefined", name.to_string_inner())),
        }
    }

    pub fn resolve_function(&self, name: Spanned<Ustr>) -> Result<&Function, NameError> {
        match self.names.resolve(&name.raw) {
            NameResult::Function(func) => Ok(func),
            NameResult::Constant(_) => {
                Err(NameError::new("expected function", name.to_string_inner())
                    .with_extra("found constant".to_owned()))
            }
            NameResult::Ambiguous(funcs) => {
                Err(NameError::new("expected function", name.to_string_inner())
                    .with_extra(format!("found {} candidates", funcs.len())))
            }
            NameResult::None => Err(NameError::new("undefined", name.to_string_inner())),
        }
    }

    pub fn resolve_dimension(&self, name: Spanned<Ustr>) -> Result<&Dimension, NameError> {
        self.dimensions
            .get(name.raw)
            .ok_or_else(|| NameError::new("undefined dimension", name.to_string_inner()))
    }

    pub fn resolve_operator(
        &self,
        kind: OpKind,
        name: Spanned<Ustr>,
    ) -> Result<&Operator, NameError> {
        self.operators
            .get(kind, name.raw)
            .ok_or_else(|| NameError::new("undefined operator", name.to_string_inner()))
    }

    pub fn resolve_unit(&self, name: Ident) -> Result<&Unit, NameError> {
        self.units
            .get(name.raw)
            .ok_or_else(|| NameError::new("undefined unit", name.as_spanned_string()))
    }

    pub fn resolve_unit_suffix(&self, suffix: Ident) -> Result<&Unit, NameError> {
        self.units
            .resolve_suffix(suffix.raw)
            .ok_or_else(|| NameError::new("undefined unit", suffix.as_spanned_string()))
    }
}

impl From<Ustr> for Module {
    fn from(name: Ustr) -> Self {
        Self::new(name)
    }
}
