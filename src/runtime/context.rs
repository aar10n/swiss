use super::encoding::EncodingRegistry;
use super::exception::StackFrame;
use super::module::{Module, ModuleId, ModuleMap};
use super::operator::{OpAssoc, OpKind, Operator, OperatorTable};
use super::path::{PathLike, PathTree};
use super::unit::{Unit, UnitKind, UnitTable};
use super::value::{VRef, Value, ValueRef, VarId};
use super::{builtin, DeclError, Function, NameError};

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
    pub prelude_modules: Vec<ModuleId>,

    // Captured output from formatters. When set, the driver will prefer printing
    // this buffer over the raw value display.
    pub pending_output: Option<String>,
    pub default_formatter: Option<Ustr>,
    // Last evaluated expression value (used as a fallback when interpretation
    // returns None).
    pub last_value: Option<Value>,
    pub encodings: EncodingRegistry,

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
            prelude_modules: Vec::new(),

            pending_output: None,
            default_formatter: None,
            last_value: None,
            encodings: EncodingRegistry::new(),

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

    pub fn take_pending_output(&mut self) -> Option<String> {
        self.pending_output.take()
    }

    pub fn set_pending_output(&mut self, output: String) {
        self.pending_output = Some(output);
    }

    pub fn take_last_value(&mut self) -> Option<Value> {
        self.last_value.take()
    }

    pub fn set_last_value(&mut self, value: Value) {
        self.last_value = Some(value);
    }

    pub fn set_default_formatter(&mut self, name: Ustr) {
        self.default_formatter = Some(name);
    }

    pub fn backtrace(&self) -> Vec<StackFrame> {
        self.call_stack.clone()
    }

    pub fn last_frame(&self) -> Option<&StackFrame> {
        self.call_stack.last()
    }

    /// Apply the cached prelude declarations to the given module, seeding
    /// operator/unit/dimension/interface tables so parsing works the same
    /// across modules.
    pub fn apply_preludes_to_module(&mut self, module_id: ModuleId) -> Result<(), DeclError> {
        if self.modules[module_id].prelude_applied {
            return Ok(());
        }

        // Snapshot prelude syntax declarations before we mutably borrow the
        // target module.
        let prelude_ids = self.prelude_modules.clone();
        let snapshots = prelude_ids
            .iter()
            .map(|pid| {
                let prelude = &self.modules[*pid];
                (
                    prelude.units.iter().cloned().collect::<Vec<_>>(),
                    prelude.operators.iter().cloned().collect::<Vec<_>>(),
                )
            })
            .collect::<Vec<_>>();

        let module = &mut self.modules[module_id];
        if module.prelude_applied {
            return Ok(());
        }

        for pid in &self.prelude_modules {
            if !module.opened.contains(pid) {
                module.opened.push(*pid);
            }
        }

        // For parsing support, copy only syntax-level declarations (operators and
        // units/suffixes) from preludes so the parser can recognize them in this
        // module's source.
        for (prelude_units, prelude_ops) in snapshots {
            for unit in prelude_units {
                if module.units.get(unit.name.raw).is_none() {
                    module.register_unit(unit)?;
                }
            }

            for op in prelude_ops {
                if module.operators.get(op.kind, op.name.raw).is_none() {
                    module.register_operator(op)?;
                }
            }
        }

        module.prelude_applied = true;
        Ok(())
    }

    // MARK: Local  Scopes

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

    pub fn resolve_variable(&mut self, path: impl PathLike) -> Result<ValueRef, NameError> {
        let name = path.base_part();
        let module_id = if path.len() == 1 {
            for scope in self.local_scopes.iter().rev() {
                if let Some(vref) = scope.vars.get(&name.raw).cloned() {
                    return Ok(vref);
                }
            }
            self.active_module().unwrap().id
        } else {
            self.modules.get_module(path.dir_parts())?.id
        };

        match self.modules.resolve_constant_in(module_id, name.clone()) {
            Ok(c) => Ok(c.value.clone()),
            Err(_) => {
                if let Ok(func) = self.modules.resolve_function_in(module_id, name.clone()) {
                    Ok(ValueRef::new_const(Value::Function(func.clone())))
                } else {
                    Err(NameError::new("undefined", name.to_string_inner()))
                }
            }
        }
    }

    pub fn resolve_function(&self, path: impl PathLike) -> Result<&Function, NameError> {
        let (module_id, name) = if path.len() == 1 {
            (self.active_module().unwrap().id, path.base_part())
        } else {
            (
                self.modules.get_module(path.dir_parts())?.id,
                path.base_part(),
            )
        };

        self.modules.resolve_function_in(module_id, name)
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

    /// Prelude files to load automatically.
    pub prelude_files: Vec<String>,
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
            prelude_files: Vec::new(),
        }
    }
}

// MARK: LocalScope

pub struct LocalScope {
    vars: UstrMap<ValueRef>,
}

impl LocalScope {
    pub fn new() -> Self {
        Self {
            vars: UstrMap::default(),
        }
    }

    pub fn get(&self, name: Ustr) -> Option<ValueRef> {
        self.vars.get(&name).cloned()
    }

    pub fn insert(&mut self, name: Ustr, value: Value) {
        self.vars.insert(name, value.into_ref());
    }

    pub fn extend<I: Iterator<Item = (Ustr, Value)>>(&mut self, vars: I) {
        for (name, value) in vars {
            self.insert(name, value);
        }
    }
}

impl<T: Into<Ustr>, I: Iterator<Item = (T, Value)>> From<I> for LocalScope {
    fn from(vars: I) -> Self {
        Self {
            vars: vars
                .map(|(name, value)| (name.into(), value.into_ref()))
                .collect(),
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
