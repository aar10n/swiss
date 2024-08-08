use super::builtin;
use super::exception::StackFrame;
use super::module::{Module, ModuleId, ModuleMap};
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
    pub modules: ModuleMap,

    active_module: Option<ModuleId>,
    call_stack: Vec<StackFrame>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            config: RuntimeConfig::default(),
            sources: SourceMap::new(),
            modules: ModuleMap::new(),

            active_module: None,
            call_stack: Vec::new(),
        }
    }

    pub fn active_module(&mut self) -> Option<&mut Module> {
        self.active_module
            .map(|module_id| &mut self.modules[module_id])
    }

    pub fn with_active_module<T>(
        &mut self,
        module_id: ModuleId,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let prev_module = self.active_module;
        self.active_module = Some(module_id);
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
