use super::exception::StackFrame;
use super::module::{Module, ModuleId, ModuleMap};
use super::operator::{OpAssoc, OpKind, Operator, OperatorTable};
use super::path::{PathLike, PathTree};
use super::unit::{Unit, UnitKind, UnitTable};
use super::value::Value;
use super::{builtin, Function, NameError};

use crate::ast::{BinaryCoercion, Coercion, FloatConversion, NodeId, Path, UnitPreference, P};
use crate::source::{SourceId, SourceMap, SourceProvider, SourceSpan, Spanned};

use smallvec::SmallVec;
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

    pub fn backtrace(&self) -> Vec<StackFrame> {
        self.call_stack.clone()
    }

    pub fn last_frame(&self) -> Option<&StackFrame> {
        self.call_stack.last()
    }

    pub fn local_scopes(&self) -> &[LocalScope] {
        &self.local_scopes
    }

    pub fn current_scope_mut(&mut self) -> Option<&mut LocalScope> {
        self.local_scopes.last_mut()
    }

    pub fn push_local_scope(&mut self, scope: LocalScope) {
        self.local_scopes.push(scope);
    }

    pub fn pop_local_scope(&mut self) {
        self.local_scopes.pop();
    }

    //

    pub fn with_active_module<T, Ctx>(
        ctx: &mut Ctx,
        module_id: ModuleId,
        f: impl FnOnce(&mut Ctx) -> T,
    ) -> T
    where
        Ctx: ContextProvider,
    {
        let prev_module = ctx.context().active_module;
        ctx.context_mut().active_module = Some(module_id);
        let result = f(ctx);
        ctx.context_mut().active_module = prev_module;
        result
    }

    pub fn with_scope<T, Ctx>(ctx: &mut Ctx, scope: LocalScope, f: impl FnOnce(&mut Ctx) -> T) -> T
    where
        Ctx: ContextProvider,
    {
        ctx.context_mut().push_local_scope(scope);
        let result = f(ctx);
        ctx.context_mut().pop_local_scope();
        result
    }

    pub fn with_fn_call<T, Ctx>(
        ctx: &mut Ctx,
        frame: StackFrame,
        scope: LocalScope,
        f: impl FnOnce(&mut Ctx) -> T,
    ) -> T
    where
        Ctx: ContextProvider,
    {
        ctx.context_mut().call_stack.push(frame);
        ctx.context_mut().push_local_scope(scope);
        let scope_offset = ctx.context_mut().local_scopes.len() - 1;
        let result = f(ctx);

        // pop any remaining scopes pushed during the function call
        assert!(ctx.context().local_scopes.len() >= scope_offset);
        while ctx.context().local_scopes().len() > scope_offset {
            ctx.context_mut().pop_local_scope();
        }

        ctx.context_mut().call_stack.pop();
        result
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
    /// Binary operand coercion behavior.
    pub binary_coercion: BinaryCoercion,
    /// Coercion behavior.
    pub coercion: Coercion,
    /// Number of decimal places to display for floating-point numbers.
    /// If `None`, the number is automatically formatted.
    pub decimal_places: Option<u32>,
    /// The precision used internally for floating-point numbers.
    pub float_precision: u32,
    /// Float to integer conversion behavior.
    pub float_conversion: FloatConversion,
    /// Result unit selection for binary operations.
    pub unit_preference: UnitPreference,
}

impl Default for RuntimeConfig {
    fn default() -> Self {
        Self {
            binary_coercion: BinaryCoercion::default(),
            coercion: Coercion::default(),
            decimal_places: None,
            float_precision: 53,
            float_conversion: FloatConversion::default(),
            unit_preference: UnitPreference::Left,
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

    pub fn extend<I: Iterator<Item = (Ustr, Value)>>(&mut self, vars: I) {
        self.vars.extend(vars);
    }
}

impl<I: Iterator<Item = (Ustr, Value)>> From<I> for LocalScope {
    fn from(vars: I) -> Self {
        Self {
            vars: vars.collect(),
        }
    }
}

// MARK: ContextProvider

pub trait ContextProvider {
    fn context(&self) -> &Context;
    fn context_mut(&mut self) -> &mut Context;
}

impl ContextProvider for Context {
    fn context(&self) -> &Context {
        self
    }

    fn context_mut(&mut self) -> &mut Context {
        self
    }
}
