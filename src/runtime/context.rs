use super::builtin;
use super::exception::StackFrame;
use super::module::{Module, ModuleId};
use super::operator::{OpAssoc, OpKind, Operator, OperatorTable};
use super::path::{PathLike, PathTree};
use super::unit::{Unit, UnitKind, UnitTable};
use super::NameError;

use crate::ast::{BinaryCoercion, Coercion, FloatConversion, NodeId, Path};
use crate::source::{SourceId, SourceMap, SourceProvider, SourceSpan, Spanned};

use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::io;
use std::rc::Rc;
use ustr::{Ustr, UstrMap};

/// Shared context for a program.
pub struct Context {
    pub config: RuntimeConfig,
    pub sources: SourceMap,

    modules: PathTree<Module>,
    module_to_index: BTreeMap<ModuleId, usize>,
    source_to_index: BTreeMap<SourceId, usize>,

    active_module: Option<usize>,
    call_stack: Vec<StackFrame>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            config: RuntimeConfig::default(),
            sources: SourceMap::new(),

            modules: PathTree::new(),
            module_to_index: BTreeMap::new(),
            source_to_index: BTreeMap::new(),

            active_module: None,
            call_stack: Vec::new(),
        }
    }

    pub fn new_module(&mut self, path: impl PathLike) -> Result<&mut Module, NameError> {
        let index = self.modules.len();
        let module = self
            .modules
            .insert(path)
            .map_err(|spanned| NameError::new("invalid module", spanned))?;

        self.module_to_index.insert(module.id, index);
        Ok(module)
    }

    pub fn get_or_create_module(&mut self, path: impl PathLike) -> Result<&mut Module, NameError> {
        self.modules
            .get_or_insert(path)
            .map(|(module, _)| module)
            .map_err(|spanned| NameError::new("invalid module", spanned))
    }

    pub fn get_module(&mut self, path: impl PathLike) -> Result<&mut Module, NameError> {
        self.modules
            .get_mut(path)
            .map(|(module, _)| module)
            .map_err(|spanned| NameError::new("invalid module", spanned))
    }

    pub fn get_module_index(&self, module_id: ModuleId) -> usize {
        self.module_to_index[&module_id]
    }

    pub fn module_by_id(&mut self, module_id: ModuleId) -> &mut Module {
        let index = self.module_to_index.get(&module_id).copied().unwrap();
        &mut self.modules[index]
    }

    pub fn active_module(&mut self) -> Option<&mut Module> {
        self.active_module.map(|index| &mut self.modules[index])
    }

    pub fn with_active_module<T>(&mut self, index: usize, f: impl FnOnce(&mut Self) -> T) -> T {
        let prev_module = self.active_module;
        self.active_module = Some(index);
        let result = f(self);
        self.active_module = prev_module;
        result
    }

    pub fn backtrace(&self) -> Vec<StackFrame> {
        self.call_stack.clone()
    }
}

impl SourceProvider for Context {
    fn sources(&self) -> &SourceMap {
        &self.sources
    }
}

/// Runtime config options.
pub struct RuntimeConfig {
    // The precision for floating-point numbers.
    pub precision: u32,
    /// Coercion behavior.
    pub coercion: Coercion,
    /// Binary operand coercion behavior.
    pub binary_coercion: BinaryCoercion,
    /// Float to integer conversion behavior.
    pub float_conversion: FloatConversion,
}

impl Default for RuntimeConfig {
    fn default() -> Self {
        Self {
            precision: 53,
            coercion: Coercion::default(),
            binary_coercion: BinaryCoercion::default(),
            float_conversion: FloatConversion::default(),
        }
    }
}
