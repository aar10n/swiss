use super::exception::StackFrame;
use super::module::{Module, ModuleId, ModuleMap};
use super::operator::{OpAssoc, OpKind, Operator, OperatorTable};
use super::path::{PathLike, PathTree};
use super::unit::{Unit, UnitKind, UnitTable};
use super::value::Value;
use super::{builtin, Function, NameError};

use crate::ast::{BinaryCoercion, Coercion, FloatConversion, NodeId, Path, P};
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
    local_scopes: Vec<LocalScope>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            config: RuntimeConfig::default(),
            sources: SourceMap::new(),
            modules: ModuleMap::new(),

            active_module: None,
            call_stack: Vec::new(),
            local_scopes: Vec::new(),
        }
    }

    pub fn active_module(&self) -> Option<&Module> {
        self.active_module.map(|module_id| &self.modules[module_id])
    }

    pub fn active_module_mut(&mut self) -> Option<&mut Module> {
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

    pub fn push_frame(&mut self, frame: StackFrame) {
        self.call_stack.push(frame);
    }

    pub fn pop_frame(&mut self) {
        self.call_stack.pop();
    }

    pub fn local_scopes(&self) -> &[LocalScope] {
        &self.local_scopes
    }

    pub fn push_local_scope(&mut self, scope: LocalScope) {
        self.local_scopes.push(scope);
    }

    pub fn pop_local_scope(&mut self) {
        self.local_scopes.pop();
    }
}

impl Context {
    pub fn get_module(&self, path: impl PathLike) -> Result<&Module, NameError> {
        if path.is_empty() {
            Ok(self.active_module().unwrap())
        } else {
            self.modules.get_module(path)
        }
    }

    pub fn get_module_mut(&mut self, path: impl PathLike) -> Result<&mut Module, NameError> {
        if path.is_empty() {
            Ok(self.active_module_mut().unwrap())
        } else {
            self.modules.get_module_mut(path)
        }
    }

    pub fn resolve_value(&self, path: impl PathLike) -> Result<&Value, NameError> {
        if path.len() == 1 {
            let name = path.base_part();
            for scope in self.local_scopes.iter().rev() {
                if let Some(value) = scope.vars.get(&name.raw) {
                    return Ok(value);
                }
            }

            self.active_module()
                .unwrap()
                .resolve_constant(name)
                .map(|c| &c.value)
        } else {
            self.modules
                .get_module(path.dir_parts())?
                .resolve_constant(path.base_part())
                .map(|c| &c.value)
        }
    }

    pub fn resolve_function(&self, path: impl PathLike) -> Result<&Function, NameError> {
        if path.len() == 1 {
            self.active_module()
                .unwrap()
                .resolve_function(path.base_part())
        } else {
            self.modules
                .get_module(path.dir_parts())?
                .resolve_function(path.base_part())
        }
    }
}

impl SourceProvider for Context {
    fn sources(&self) -> &SourceMap {
        &self.sources
    }
}

/// Runtime options.
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

// MARK: LocalScope

pub struct LocalScope {
    vars: UstrMap<Value>,
}

impl LocalScope {
    pub fn new() -> Self {
        Self {
            vars: UstrMap::default(),
        }
    }

    pub fn insert(&mut self, name: Ustr, value: Value) {
        self.vars.insert(name, value);
    }
}
