use super::conversion::ConversionGraph;
use super::dimension::{DimExpr, Dimension, DimensionTable};
use super::interface::{Interface, InterfaceTable};
use super::name::{Constant, Function, Name, NameResult, NameTable, Param};
use super::operator::{OpAssoc, OpKind, Operator, OperatorTable};
use super::path::{PathLike, PathTree};
use super::unit::{Unit, UnitKind, UnitTable};
use super::{DeclError, NameError};

use crate::id::{module_id, var_id, VarId};
use crate::source::{source_id, SourceFile, SourceId, SourceSpan, Spanned};

pub use crate::id::ModuleId;

use smallvec::{smallvec, SmallVec};
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::ops::{Index, IndexMut};
use std::rc::Rc;
use ustr::Ustr;

// MARK: Module

pub struct Module {
    pub id: ModuleId,
    pub index: usize,

    pub name: Ustr,
    pub prelude_applied: bool,
    /// Modules whose declarations are visible for lookup (e.g., preludes).
    pub opened: Vec<ModuleId>,
    /// Aliases to other modules (imported sub-modules).
    pub module_aliases: BTreeMap<Ustr, (ModuleId, SourceSpan)>,
    pub names: NameTable,
    pub dimensions: DimensionTable,
    pub interfaces: InterfaceTable,
    pub operators: OperatorTable,
    pub units: UnitTable,
    pub conversion_graph: ConversionGraph,
    pub unnamed: BTreeMap<VarId, Function>,
}

impl Module {
    pub fn new(index: usize, name: Ustr) -> Self {
        Self {
            id: module_id::next(),
            index,
            name,
            prelude_applied: false,
            opened: Vec::new(),
            module_aliases: BTreeMap::new(),
            names: NameTable::new(),
            dimensions: DimensionTable::new(),
            interfaces: InterfaceTable::new(),
            operators: OperatorTable::new_with_builtins(),
            units: UnitTable::new(),
            conversion_graph: ConversionGraph::new(),
            unnamed: BTreeMap::new(),
        }
    }

    // Item Registration

    pub fn register_constant(&mut self, constant: Constant) -> Result<(), DeclError> {
        self.names.insert_constant(constant)
    }

    pub fn register_function(&mut self, func: Function) -> Result<(), DeclError> {
        self.names.insert_function(func)
    }

    pub fn with_constant(&mut self, constant: Constant) -> &mut Self {
        self.register_constant(constant).unwrap();
        self
    }

    pub fn with_function(&mut self, func: Function) -> &mut Self {
        self.register_function(func).unwrap();
        self
    }

    pub fn with_interface(&mut self, interface: Interface) -> &mut Self {
        self.register_interface(interface).unwrap();
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

        self.dimensions.insert(dim.clone());
        Ok(())
    }

    pub fn register_interface(&mut self, interface: Interface) -> Result<(), DeclError> {
        if let Some(existing) = self.interfaces.get(interface.name.raw) {
            return Err(DeclError::new(
                "interface",
                interface.name.to_string_inner(),
                existing.name.span,
            ));
        }

        self.interfaces.insert(interface.clone());
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

        self.operators.insert(op.clone());
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

        // Register in conversion graph
        self.conversion_graph.register_unit(
            unit.name.raw,
            unit.dim_expr.clone(),
            unit.conversion.clone(),
            unit.is_base(),
        );

        self.units.insert(unit.clone());
        Ok(())
    }

    pub fn register_module_alias(
        &mut self,
        name: Spanned<Ustr>,
        module_id: ModuleId,
    ) -> Result<(), DeclError> {
        if let Some((existing_id, existing_span)) = self.module_aliases.get(&name.raw) {
            if *existing_id == module_id {
                return Ok(());
            }
            return Err(DeclError::new(
                "module",
                name.to_string_inner(),
                *existing_span,
            ));
        }

        self.module_aliases
            .insert(name.raw, (module_id, name.span));
        Ok(())
    }

    pub fn resolve_module_alias(&self, name: Ustr) -> Option<ModuleId> {
        self.module_aliases.get(&name).map(|(id, _)| *id)
    }

    /// Update an existing unit (used during interpretation to replace placeholder units)
    pub fn update_unit(&mut self, unit: Unit) {
        // Register in conversion graph
        self.conversion_graph.register_unit(
            unit.name.raw,
            unit.dim_expr.clone(),
            unit.conversion.clone(),
            unit.is_base(),
        );

        self.units.insert(unit);
    }

    pub fn register_unnamed_function(&mut self, func: Function) -> VarId {
        let id = var_id::next();
        self.unnamed.insert(id, func);
        id
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

    pub fn resolve_unit_suffix(&self, suffix: Spanned<Ustr>) -> Result<&Unit, NameError> {
        self.units
            .resolve_suffix(suffix.raw)
            .ok_or_else(|| NameError::new("undefined unit", suffix.to_spanned_string()))
    }

    pub fn resolve_unit_expr(&self, expr: &DimExpr) -> Result<&Unit, NameError> {
        self.units
            .resolve_dimexpr(expr)
            .ok_or_else(|| NameError::new("undefined unit", expr.to_string().into()))
    }

    pub fn get_unnamed_function(&self, id: VarId) -> Option<&Function> {
        self.unnamed.get(&id)
    }

    pub fn get_interface(&self, name: &str) -> Option<&Interface> {
        self.interfaces.get(name.into())
    }
}

impl From<(usize, Ustr)> for Module {
    fn from((index, name): (usize, Ustr)) -> Self {
        Self::new(index, name)
    }
}

// MARK: ModuleMap

pub struct ModuleMap {
    module_tree: PathTree<Module>,
    module_to_index: BTreeMap<ModuleId, usize>,
    module_paths: Vec<SmallVec<[Spanned<Ustr>; 4]>>,
}

impl ModuleMap {
    pub fn new() -> Self {
        Self {
            module_tree: PathTree::new(),
            module_to_index: BTreeMap::new(),
            module_paths: Vec::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.module_tree.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Module> {
        self.module_tree.items().iter()
    }

    pub fn get_module(&self, path: impl PathLike) -> Result<&Module, NameError> {
        self.module_tree
            .get(path)
            .map(|(module, _)| module)
            .map_err(|spanned| NameError::new("invalid module", spanned))
    }

    pub fn get_module_mut(&mut self, path: impl PathLike) -> Result<&mut Module, NameError> {
        self.module_tree
            .get_mut(path)
            .map(|(module, _)| module)
            .map_err(|spanned| NameError::new("invalid module", spanned))
    }

    pub fn new_module(&mut self, path: impl PathLike) -> Result<&mut Module, NameError> {
        let index = self.module_tree.len();
        let parts = path.parts();
        let module = self
            .module_tree
            .insert(path)
            .map_err(|spanned| NameError::new("invalid module", spanned))?;

        self.module_to_index.insert(module.id, index);
        self.module_paths.push(parts);
        Ok(module)
    }

    pub fn get_or_add_module(&mut self, path: impl PathLike) -> Result<&mut Module, NameError> {
        let next_index = self.module_tree.len();
        let parts = path.parts();
        let (module, index) = self
            .module_tree
            .get_or_insert(path)
            .map_err(|spanned| NameError::new("invalid module", spanned))?;
        if index == next_index {
            self.module_to_index.insert(module.id, index);
            self.module_paths.push(parts);
        }
        Ok(module)
    }

    pub fn module_path(&self, module_id: ModuleId) -> &SmallVec<[Spanned<Ustr>; 4]> {
        let index = self.module_to_index[&module_id];
        &self.module_paths[index]
    }

    // Overlay resolution that honors a module's opened list.
    pub fn resolve_constant_in(
        &self,
        module_id: ModuleId,
        name: Spanned<Ustr>,
    ) -> Result<&Constant, NameError> {
        let module = &self[module_id];
        match module.names.resolve(&name.raw) {
            NameResult::Constant(c) => Ok(c),
            NameResult::Function(_) => {
                Err(NameError::new("expected constant", name.to_string_inner())
                    .with_extra("found function".to_owned()))
            }
            NameResult::Ambiguous(_) => {
                Err(NameError::new("expected constant", name.to_string_inner()))
            }
            NameResult::None => {
                for mid in &module.opened {
                    let other = &self[*mid];
                    if let NameResult::Constant(c) = other.names.resolve(&name.raw) {
                        return Ok(c);
                    }
                }
                Err(NameError::new("undefined", name.to_string_inner()))
            }
        }
    }

    pub fn resolve_function_in(
        &self,
        module_id: ModuleId,
        name: Spanned<Ustr>,
    ) -> Result<&Function, NameError> {
        let module = &self[module_id];
        match module.names.resolve(&name.raw) {
            NameResult::Function(f) => Ok(f),
            NameResult::Constant(_) => {
                Err(NameError::new("expected function", name.to_string_inner())
                    .with_extra("found constant".to_owned()))
            }
            NameResult::Ambiguous(funcs) => {
                Err(NameError::new("expected function", name.to_string_inner())
                    .with_extra(format!("found {} candidates", funcs.len())))
            }
            NameResult::None => {
                for mid in &module.opened {
                    let other = &self[*mid];
                    if let NameResult::Function(f) = other.names.resolve(&name.raw) {
                        return Ok(f);
                    }
                }
                Err(NameError::new("undefined", name.to_string_inner()))
            }
        }
    }

    pub fn resolve_dimension_in(
        &self,
        module_id: ModuleId,
        name: Spanned<Ustr>,
    ) -> Result<&Dimension, NameError> {
        let module = &self[module_id];
        if let Some(dim) = module.dimensions.get(name.raw) {
            return Ok(dim);
        }
        for mid in &module.opened {
            let other = &self[*mid];
            if let Some(dim) = other.dimensions.get(name.raw) {
                return Ok(dim);
            }
        }
        Err(NameError::new(
            "undefined dimension",
            name.to_string_inner(),
        ))
    }

    pub fn resolve_operator_in(
        &self,
        module_id: ModuleId,
        kind: OpKind,
        name: Spanned<Ustr>,
    ) -> Result<&Operator, NameError> {
        let module = &self[module_id];
        if let Some(op) = module.operators.get(kind, name.raw) {
            return Ok(op);
        }
        for mid in &module.opened {
            let other = &self[*mid];
            if let Some(op) = other.operators.get(kind, name.raw) {
                return Ok(op);
            }
        }
        Err(NameError::new("undefined operator", name.to_string_inner()))
    }

    pub fn resolve_unit_suffix_in(
        &self,
        module_id: ModuleId,
        suffix: Spanned<Ustr>,
    ) -> Result<&Unit, NameError> {
        let module = &self[module_id];
        if let Some(unit) = module.units.resolve_suffix(suffix.raw) {
            return Ok(unit);
        }
        for mid in &module.opened {
            let other = &self[*mid];
            if let Some(unit) = other.units.resolve_suffix(suffix.raw) {
                return Ok(unit);
            }
        }
        Err(NameError::new("undefined unit", suffix.to_spanned_string()))
    }

    pub fn resolve_unit_expr_in(
        &self,
        module_id: ModuleId,
        expr: &DimExpr,
    ) -> Result<&Unit, NameError> {
        let module = &self[module_id];
        if let Some(unit) = module.units.resolve_dimexpr(expr) {
            return Ok(unit);
        }
        for mid in &module.opened {
            let other = &self[*mid];
            if let Some(unit) = other.units.resolve_dimexpr(expr) {
                return Ok(unit);
            }
        }
        Err(NameError::new("undefined unit", expr.to_string().into()))
    }
}

impl Index<ModuleId> for ModuleMap {
    type Output = Module;

    fn index(&self, id: ModuleId) -> &Self::Output {
        let index = self.module_to_index[&id];
        &self.module_tree[index]
    }
}

impl IndexMut<ModuleId> for ModuleMap {
    fn index_mut(&mut self, id: ModuleId) -> &mut Self::Output {
        let index = self.module_to_index[&id];
        &mut self.module_tree[index]
    }
}
