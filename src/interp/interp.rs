use super::{InterpError, InterpResult, NameError, TypeError, Value};

use crate::ast::*;
use crate::id::VarId;
use crate::interp::Exception;
use crate::print::{PrettyPrint, PrettyString};
use crate::runtime::{
    self as rt, CastInto, Constant, Context, ContextProvider, Function, LRValue, List, LocalScope,
    ModuleId, PathLike, StackFrame, Tuple, ValueRef,
};
use crate::source::{SourceId, SourceSpan, Spanned};
use crate::{lexer, parser};

use either::{Either, Left, Right};
use rug::{Float, Integer};
use smallvec::{smallvec, SmallVec};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use ustr::Ustr;

const TABWIDTH: &str = "    ";
const MAX_FLOAT_PRECISION: u32 = 1024;
const MAX_SIGNIFICANT_FIGURES: u32 = 1024;

fn module_scope_ids(ctx: &Context, module_id: ModuleId) -> Vec<ModuleId> {
    let opened = ctx.modules[module_id].opened.clone();
    let mut module_ids = Vec::with_capacity(1 + opened.len());
    module_ids.push(module_id);
    module_ids.extend(opened);
    module_ids
}

macro_rules! trace {
    ($self:ident, $intrp:expr, $msg:expr, $expr:expr) => {{
        let tab = TABWIDTH.repeat($intrp.trace_level);
        if $intrp.trace_on {
            eprintln!(
                "[TRACE] {{{}}} {tab}{}: {}",
                $intrp.trace_level,
                $msg,
                $self.pretty_string(&())
            );
        }

        $intrp.trace_level += 1;
        let result = $expr;
        $intrp.trace_level -= 1;

        if $intrp.trace_on {
            eprintln!(
                "[TRACE] {{{}}} {tab}{}: {}",
                $intrp.trace_level,
                $msg,
                result.pretty_string($intrp.ctx)
            );
        }
        result
    }};
}

macro_rules! no_trace {
    (self, $intrp:expr, $msg:expr, $expr:expr) => {
        $expr
    };
}

struct CaptureCollector<'a> {
    ctx_scopes: &'a [LocalScope],
    params: HashSet<Ustr>,
    locals: Vec<HashSet<Ustr>>,
    captures: HashMap<Ustr, ValueRef>,
}

impl<'a> CaptureCollector<'a> {
    fn new(params: &[Param], ctx_scopes: &'a [LocalScope]) -> Self {
        let params = params.iter().map(|p| p.name.raw).collect();
        Self {
            ctx_scopes,
            params,
            locals: vec![HashSet::new()],
            captures: HashMap::new(),
        }
    }

    fn into_captures(self) -> Vec<(Ustr, ValueRef)> {
        self.captures.into_iter().collect()
    }

    fn is_local(&self, name: Ustr) -> bool {
        if self.params.contains(&name) {
            return true;
        }
        self.locals.iter().any(|scope| scope.contains(&name))
    }

    fn lookup_outer(&self, name: Ustr) -> Option<ValueRef> {
        for scope in self.ctx_scopes.iter().rev() {
            if let Some(vref) = scope.get(name) {
                return Some(vref);
            }
        }
        None
    }

    fn capture_read(&mut self, name: Ustr) {
        if self.is_local(name) {
            return;
        }
        if let Some(vref) = self.lookup_outer(name) {
            self.captures.entry(name).or_insert(vref);
        }
    }

    fn capture_assignment(&mut self, name: Ustr) {
        if self.is_local(name) {
            return;
        }
        if let Some(vref) = self.lookup_outer(name) {
            self.captures.entry(name).or_insert(vref);
        } else if let Some(scope) = self.locals.last_mut() {
            scope.insert(name);
        }
    }

    fn push_scope(&mut self) {
        self.locals.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        self.locals.pop();
    }

    fn bind_pat_names(pat: &BindPat, out: &mut Vec<Ustr>) {
        match &pat.kind {
            BindPatKind::Ignored => {}
            BindPatKind::Var(ident) => {
                if ident.raw != "_" {
                    out.push(ident.raw);
                }
            }
            BindPatKind::Tuple(items) => {
                for item in items.iter() {
                    Self::bind_pat_names(item, out);
                }
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Break | StmtKind::Continue => {}
            StmtKind::Expr(expr) | StmtKind::Return(expr) => self.visit_expr(expr),
        }
    }

    fn visit_block(&mut self, body: &ListNode<Stmt>) {
        for stmt in body.items.iter() {
            self.visit_stmt(stmt);
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Assign(lhs, rhs) => {
                let mut bound = Vec::new();
                Self::bind_pat_names(lhs, &mut bound);
                self.visit_expr(rhs);
                for name in bound {
                    self.capture_assignment(name);
                }
            }
            ExprKind::IndexAssign(base, idx, value) => {
                self.visit_expr(base);
                self.visit_expr(idx);
                self.visit_expr(value);
            }
            ExprKind::Slice(container, start, stop) => {
                self.visit_expr(container);
                if let Some(start) = start {
                    self.visit_expr(start);
                }
                if let Some(stop) = stop {
                    self.visit_expr(stop);
                }
            }
            ExprKind::InfixOp(op, lhs, rhs) => {
                if op.raw.as_str() == "." {
                    self.visit_expr(lhs);
                    match &rhs.kind {
                        ExprKind::FnCall(_, args) => {
                            for arg in args.items.iter() {
                                self.visit_expr(arg);
                            }
                        }
                        ExprKind::Ident(_) | ExprKind::Path(_) => {}
                        _ => self.visit_expr(rhs),
                    }
                } else {
                    self.visit_expr(lhs);
                    self.visit_expr(rhs);
                }
            }
            ExprKind::PrefixOp(_, expr) => self.visit_expr(expr),
            ExprKind::PostfixOp(expr, _) => self.visit_expr(expr),
            ExprKind::UnitCast(expr, _) => self.visit_expr(expr),
            ExprKind::If(if_expr) => {
                for branch in if_expr.branches.iter() {
                    self.visit_expr(&branch.cond);
                    self.visit_block(&branch.body);
                }
                if let Some(else_) = &if_expr.else_branch {
                    self.visit_block(else_);
                }
            }
            ExprKind::Try(try_expr) => {
                match &try_expr.body {
                    TryBody::Expr(expr) => self.visit_expr(expr),
                    TryBody::Block(body) => self.visit_block(body),
                }
                if let Some(catch) = &try_expr.catch {
                    self.push_scope();
                    if let Some(binding) = &catch.binding {
                        if let Some(scope) = self.locals.last_mut() {
                            scope.insert(binding.raw);
                        }
                    }
                    self.visit_block(&catch.body);
                    self.pop_scope();
                }
            }
            ExprKind::ForRange(pat, iter, body) => {
                self.visit_expr(iter);
                self.push_scope();
                let mut bound = Vec::new();
                Self::bind_pat_names(pat, &mut bound);
                for name in bound {
                    if let Some(scope) = self.locals.last_mut() {
                        scope.insert(name);
                    }
                }
                self.visit_block(body);
                self.pop_scope();
            }
            ExprKind::FnCall(_, args) => {
                for arg in args.items.iter() {
                    self.visit_expr(arg);
                }
            }
            ExprKind::Splat(expr) => self.visit_expr(expr),
            ExprKind::List(list) => {
                for item in list.items.iter() {
                    self.visit_expr(item);
                }
            }
            ExprKind::Tuple(tuple) => {
                for item in tuple.items.iter() {
                    self.visit_expr(item);
                }
            }
            ExprKind::Object(object) => {
                for field in object.items.iter() {
                    self.visit_expr(&field.value);
                }
            }
            ExprKind::Lambda(_) => {}
            ExprKind::Ident(ident) => self.capture_read(ident.raw),
            ExprKind::Path(_)
            | ExprKind::Empty
            | ExprKind::Number(_)
            | ExprKind::String(_)
            | ExprKind::Boolean(_)
            | ExprKind::Unit(_)
            | ExprKind::Type(_) => {}
        }
    }
}

fn validate_params(params: &[Param]) -> InterpResult<()> {
    let mut seen_optional = false;
    let mut seen_variadic = false;
    for (i, param) in params.iter().enumerate() {
        if param.is_variadic && i != params.len() - 1 {
            return Err(TypeError::simple(
                param
                    .span()
                    .into_spanned("variadic parameter must be last".to_string()),
            )
            .into());
        }
        if param.is_variadic {
            seen_variadic = true;
        }
        if param.is_optional {
            if param.is_variadic {
                return Err(TypeError::simple(
                    param
                        .span()
                        .into_spanned("variadic parameter cannot be optional".to_string()),
                )
                .into());
            }
            if seen_variadic {
                return Err(TypeError::simple(param.span().into_spanned(
                    "optional parameter cannot follow variadic parameter".to_string(),
                ))
                .into());
            }
            seen_optional = true;
        } else if seen_optional {
            return Err(TypeError::simple(
                param.span().into_spanned(
                    "required parameter cannot follow optional parameter".to_string(),
                ),
            )
            .into());
        }
    }
    Ok(())
}

// MARK: Interpreter

pub struct Interpreter<'ctx> {
    pub ctx: &'ctx mut Context,

    pub current_source: Option<crate::source::SourceId>,
    pub(crate) trace_on: bool,
    trace_level: usize,
}

impl<'ctx> Interpreter<'ctx> {
    pub fn new(ctx: &'ctx mut Context) -> Interpreter<'ctx> {
        Interpreter {
            ctx,

            current_source: None,
            trace_on: std::env::var("TRACE_INTERP").is_ok(),
            trace_level: 0,
        }
    }

    pub fn with_source(mut self, source_id: SourceId) -> Self {
        self.current_source = Some(source_id);
        self
    }

    pub fn active_module(&mut self) -> &mut rt::Module {
        self.ctx.active_module_mut().unwrap()
    }

    fn trace_debug(&self, msg: &str) {
        let tab = TABWIDTH.repeat(self.trace_level);
        if self.trace_on {
            eprintln!("[TRACE] {{{}}} {tab}{}", self.trace_level, msg);
        }
    }

    /// Resolve or load the source file backing an import path.
    fn resolve_import_source(
        &mut self,
        module_path: &SmallVec<[Spanned<Ustr>; 4]>,
    ) -> InterpResult<SourceId> {
        // First, see if a source with the same module path is already loaded.
        if let Some((id, _)) = self
            .ctx
            .sources
            .iter()
            .find(|(_, file)| paths_match(module_path, &file.module_path()))
        {
            return Ok(*id);
        }

        // Fall back to loading from disk using SWISSPATH search roots.
        let relative = module_path
            .iter()
            .map(|part| part.raw.as_str())
            .collect::<Vec<_>>()
            .join("/");
        for base_dir in crate::source::swisspath_dirs() {
            let mut candidate = base_dir.join(&relative);
            candidate.set_extension("ch");
            let path_str = candidate.to_string_lossy().to_string();
            if let Ok(source_id) = self
                .ctx
                .sources
                .load_source_with_base(&path_str, Some(base_dir.clone()))
            {
                return Ok(source_id);
            }
        }

        let err = NameError::new("module not found", module_path.to_spanned_string());
        Err(InterpError::from(err))
    }

    fn load_module_from_source(
        &mut self,
        module_path: SmallVec<[Spanned<Ustr>; 4]>,
        source_id: SourceId,
    ) -> InterpResult<ModuleId> {
        let module_id = {
            let module = self
                .ctx
                .modules
                .get_or_add_module(module_path.clone())
                .map_err(InterpError::from)?;
            module.id
        };

        // Seed the new module with prelude declarations before parsing.
        self.ctx.apply_preludes_to_module(module_id)?;

        let tokens = lexer::lex(source_id, self.ctx.sources[source_id].raw())?;
        let ast_module = {
            let module = &mut self.ctx.modules[module_id];
            parser::parse(module, &tokens)?
        };

        // Evaluate the imported module in its own context first.
        crate::interp::interpret(self.ctx, &ast_module)?;
        Ok(module_id)
    }

    fn import_module(&mut self, path: &Path) -> InterpResult<ModuleId> {
        let module_path = path.path_parts();

        if let Ok(module) = self.ctx.modules.get_module(module_path.clone()) {
            return Ok(module.id);
        }

        let full_result = self.resolve_import_source(&module_path);
        if let Ok(source_id) = full_result {
            return self.load_module_from_source(module_path, source_id);
        }

        if module_path.len() > 1 {
            for prefix_len in (1..module_path.len()).rev() {
                let prefix: SmallVec<[Spanned<Ustr>; 4]> =
                    module_path.iter().take(prefix_len).cloned().collect();
                if self.ctx.modules.get_module(prefix.clone()).is_err() {
                    if let Ok(source_id) = self.resolve_import_source(&prefix) {
                        self.load_module_from_source(prefix, source_id)?;
                    }
                }

                if let Ok(module) = self.ctx.modules.get_module(module_path.clone()) {
                    return Ok(module.id);
                }
            }
        }

        Err(InterpError::from(NameError::new(
            "module not found",
            module_path.to_spanned_string(),
        )))
    }

    fn import_member(&mut self, module_id: ModuleId, member: Ident) -> InterpResult<()> {
        enum ImportItem {
            Constant(Constant),
            Functions(Vec<Function>),
            Type(rt::UserTypeDef),
            Dimension(rt::Dimension),
            Unit(rt::Unit),
            Interface(rt::Interface),
        }

        let name = member.as_spanned_ustr();
        if let Some(prev) = {
            let active = self.ctx.active_module().unwrap();
            if let Some((_, span)) = active.module_aliases.get(&name.raw) {
                Some(*span)
            } else if let Some(ty) = active.types.get(&name.raw) {
                Some(ty.name.span)
            } else if let Some(dim) = active.dimensions.get(name.raw) {
                Some(dim.name.span)
            } else if let Some(unit) = active.units.get(name.raw) {
                Some(unit.name.span)
            } else if let Some(interface) = active.interfaces.get(name.raw) {
                Some(interface.name.span)
            } else {
                match active.names.resolve(&name.raw) {
                    rt::NameResult::Constant(c) => Some(c.name.span),
                    rt::NameResult::Function(f) => Some(f.name.span),
                    rt::NameResult::Ambiguous(funcs) => funcs.first().map(|f| f.name.span),
                    rt::NameResult::None => None,
                }
            }
        } {
            let name_str = name.map_ref(|raw| raw.to_string());
            return Err(InterpError::from(rt::DeclError::new(
                "import", name_str, prev,
            )));
        }

        let item = {
            let module = &self.ctx.modules[module_id];
            match module.names.resolve(&name.raw) {
                rt::NameResult::Constant(c) => Some(ImportItem::Constant(c.clone())),
                rt::NameResult::Function(f) => Some(ImportItem::Functions(vec![f.clone()])),
                rt::NameResult::Ambiguous(funcs) => Some(ImportItem::Functions(funcs.clone())),
                rt::NameResult::None => None,
            }
            .or_else(|| module.types.get(&name.raw).cloned().map(ImportItem::Type))
            .or_else(|| {
                module
                    .dimensions
                    .get(name.raw)
                    .cloned()
                    .map(ImportItem::Dimension)
            })
            .or_else(|| module.units.get(name.raw).cloned().map(ImportItem::Unit))
            .or_else(|| {
                module
                    .interfaces
                    .get(name.raw)
                    .cloned()
                    .map(ImportItem::Interface)
            })
        };

        let Some(item) = item else {
            let module_path = self.ctx.modules.module_path(module_id);
            let module_path_str = module_path
                .iter()
                .map(|part| part.raw.as_str())
                .collect::<Vec<_>>()
                .join("::");
            let name_str = name.map_ref(|raw| raw.to_string());
            return Err(InterpError::from(
                NameError::new("undefined", name_str)
                    .with_extra(format!("in module '{}'", module_path_str)),
            ));
        };

        match item {
            ImportItem::Constant(constant) => self.active_module().register_constant(constant)?,
            ImportItem::Functions(funcs) => {
                for func in funcs {
                    self.active_module().register_function_alias(func)?;
                }
            }
            ImportItem::Type(ty) => self.active_module().register_type(ty)?,
            ImportItem::Dimension(dim) => self.active_module().register_dimension(dim)?,
            ImportItem::Unit(unit) => self.active_module().register_unit(unit)?,
            ImportItem::Interface(interface) => {
                self.active_module().register_interface(interface)?
            }
        }

        Ok(())
    }
}

fn paths_match(left: &SmallVec<[Spanned<Ustr>; 4]>, right: &SmallVec<[Spanned<Ustr>; 4]>) -> bool {
    left.len() == right.len() && left.iter().zip(right.iter()).all(|(a, b)| a.raw == b.raw)
}

fn constructor_type_name(ty: &rt::Ty) -> Option<Ustr> {
    match ty {
        rt::Ty::Any => Some(Ustr::from("any")),
        rt::Ty::Bool => Some(Ustr::from("bool")),
        rt::Ty::Int => Some(Ustr::from("int")),
        rt::Ty::Float => Some(Ustr::from("float")),
        rt::Ty::Num => Some(Ustr::from("num")),
        rt::Ty::Str => Some(Ustr::from("str")),
        rt::Ty::Function => Some(Ustr::from("fn")),
        rt::Ty::Iter => Some(Ustr::from("iter")),
        rt::Ty::List => Some(Ustr::from("list")),
        rt::Ty::Object => Some(Ustr::from("object")),
        rt::Ty::Tuple(_) => Some(Ustr::from("tuple")),
        rt::Ty::Unit => Some(Ustr::from("unit")),
        rt::Ty::Type => Some(Ustr::from("type")),
        rt::Ty::UserType(name) => Some(*name),
        _ => None,
    }
}

fn is_builtin_type_name(name: Ustr) -> bool {
    matches!(
        name.as_str(),
        "any"
            | "bool"
            | "int"
            | "float"
            | "num"
            | "str"
            | "fn"
            | "iter"
            | "list"
            | "object"
            | "tuple"
            | "unit"
            | "type"
            | "io"
    )
}

impl<'ctx> Interpreter<'ctx> {
    pub fn call_by_id(&mut self, func_id: VarId, values: Vec<Value>) -> InterpResult<Value> {
        let f = self
            .active_module()
            .get_unnamed_function(func_id)
            .ok_or_else(|| {
                Exception::new(
                    "RuntimeError",
                    format!("undefined function with id {:?}", func_id),
                )
            })?
            .clone();

        use rt::FunctionKind;
        self.trace_debug(&format!(
            "call_function {}({} args)",
            f.name.raw,
            values.len()
        ));

        let is_variadic = f.params.last().map_or(false, |p| p.is_variadic());
        let expected_fixed_args = if is_variadic {
            f.params.len() - 1
        } else {
            f.params.len()
        };

        // Check argument count for non-variadic functions
        if !is_variadic && values.len() != f.params.len() {
            return Err(InterpError::TypeError(TypeError {
                expected: Some(format!(
                    "function {} expects {} argument(s)",
                    f.name.raw,
                    f.params.len()
                )),
                found: SourceSpan::default().into_spanned(format!("{} argument(s)", values.len())),
                context: None,
            }));
        }

        // Check minimum argument count for variadic functions
        if is_variadic && values.len() < expected_fixed_args {
            return Err(InterpError::TypeError(TypeError {
                expected: Some(format!(
                    "function {} expects at least {} argument(s)",
                    f.name.raw, expected_fixed_args
                )),
                found: SourceSpan::default().into_spanned(format!("{} argument(s)", values.len())),
                context: None,
            }));
        }

        // Apply type coercion for fixed parameters
        let mut coerced_values = Vec::new();
        for (i, param) in f.params[..expected_fixed_args].iter().enumerate() {
            let val = values[i].clone();
            let ty = param.ty.clone().map_or(rt::Ty::Any, |ty| ty.raw);

            // Type coercion (except for references which are already values)
            if !ty.is_ref() && ty != rt::Ty::Unit {
                coerced_values.push(rt::coerce::to_ty(self.ctx, val, ty));
            } else {
                coerced_values.push(val);
            }
        }

        // Handle variadic arguments
        if is_variadic {
            let variadic_args: Vec<Value> = values[expected_fixed_args..].to_vec();

            // For native functions, spread the variadic args directly
            // For source functions, wrap them in a list
            if matches!(&f.kind, rt::FunctionKind::Native(_)) {
                coerced_values.extend(variadic_args);
            } else {
                coerced_values.push(Value::list(variadic_args));
            }
        }

        // Create a dummy call site span since we don't have source information
        let call_site = SourceSpan::default();

        let run_call = |intrp: &mut Interpreter<'ctx>, coerced_values: Vec<Value>| match &f.kind {
            FunctionKind::Native(builtin) => {
                let frame = StackFrame::new(f.name, call_site);
                let scope = LocalScope::new();
                Context::with_fn_call(intrp, frame, scope, |intrp| {
                    builtin(intrp.ctx, coerced_values).map_err(InterpError::from)
                })
            }
            FunctionKind::Source(block) => {
                let frame = StackFrame::new(f.name, call_site);
                let scope = LocalScope::from(
                    f.params
                        .iter()
                        .map(|p| p.name.raw)
                        .zip(coerced_values.into_iter()),
                );

                Context::with_fn_call(intrp, frame, scope, |intrp| {
                    Interp::<Value>::eval(block, intrp)
                })
            }
            FunctionKind::Lambda { body, captures } => {
                let frame = StackFrame::new(f.name, call_site);
                let mut scopes = Vec::with_capacity(2);
                scopes.push(LocalScope::from_refs(captures.iter().cloned()));
                scopes.push(LocalScope::from(
                    f.params
                        .iter()
                        .map(|p| p.name.raw)
                        .zip(coerced_values.into_iter()),
                ));

                Context::with_lambda_call(intrp, frame, scopes, |intrp| {
                    Interp::<Value>::eval(body, intrp)
                })
            }
            FunctionKind::Lambda { body, captures } => {
                let frame = StackFrame::new(f.name, call_site);
                let mut scopes = Vec::with_capacity(2);
                scopes.push(LocalScope::from_refs(captures.iter().cloned()));
                scopes.push(LocalScope::from(
                    f.params
                        .iter()
                        .map(|p| p.name.raw)
                        .zip(coerced_values.into_iter()),
                ));

                Context::with_lambda_call(intrp, frame, scopes, |intrp| {
                    Interp::<Value>::eval(body, intrp)
                })
            }
            FunctionKind::BuiltinWrapper {
                target_name,
                target_params,
                target_fn,
                args,
            } => {
                let scope = LocalScope::from(
                    f.params
                        .iter()
                        .map(|p| p.name.raw)
                        .zip(coerced_values.into_iter()),
                );
                let target_values = Context::with_scope(intrp, scope, |intrp| {
                    intrp.eval_call_args(
                        target_params,
                        true,
                        args.clone(),
                        call_site,
                        target_name.raw,
                    )
                })?;

                let frame = StackFrame::new(target_name.clone(), call_site);
                let scope = LocalScope::new();
                Context::with_fn_call(intrp, frame, scope, |intrp| {
                    target_fn(intrp.ctx, target_values).map_err(InterpError::from)
                })
            }
        };

        // Invoke the function in its defining module, if known.
        let module_id = match &f.kind {
            FunctionKind::Native(_) => None,
            _ => f.module_id,
        };
        let result = if let Some(module_id) = module_id {
            Context::with_active_module(self, module_id, |intrp| run_call(intrp, coerced_values))
        } else {
            run_call(self, coerced_values)
        };

        match result {
            Ok(value) => Ok(value),
            Err(InterpError::Return(value)) => Ok(value),
            Err(err) => Err(err),
        }
    }

    fn eval_arg_for_param(&mut self, param: &rt::Param, arg: &Expr) -> InterpResult<Value> {
        let ty = param.ty.clone().map_or(rt::Ty::Any, |ty| ty.raw);

        if param.is_optional() && matches!(arg.kind, ExprKind::Empty) {
            return Ok(Value::Empty);
        }

        if ty.is_ref() {
            let vref = Interp::<ValueRef>::eval(arg, self)?;
            return Ok(vref.into_value());
        }

        if ty == rt::Ty::Unit {
            let val = match &arg.kind {
                ExprKind::Ident(ident) => {
                    if let Some(vref) = self
                        .ctx
                        .local_scopes()
                        .iter()
                        .rev()
                        .find_map(|scope| scope.get(ident.raw))
                    {
                        vref.get()
                    } else if let Ok(constant) = self
                        .ctx
                        .active_module()
                        .unwrap()
                        .resolve_constant(ident.as_spanned_ustr())
                    {
                        constant.value.get()
                    } else {
                        Value::Unit(ident.raw)
                    }
                }
                ExprKind::Path(path) if path.parts.len() == 1 => {
                    let ident = &path.parts[0];
                    if let Some(vref) = self
                        .ctx
                        .local_scopes()
                        .iter()
                        .rev()
                        .find_map(|scope| scope.get(ident.raw))
                    {
                        vref.get()
                    } else if let Ok(constant) = self
                        .ctx
                        .active_module()
                        .unwrap()
                        .resolve_constant(ident.as_spanned_ustr())
                    {
                        constant.value.get()
                    } else {
                        Value::Unit(ident.raw)
                    }
                }
                _ => Interp::<Value>::eval(arg, self)?,
            };

            if param.is_optional() {
                match &val {
                    Value::Empty => return Ok(Value::Empty),
                    Value::Ref(r) if matches!(&*r.borrow(), Value::Empty) => {
                        return Ok(Value::Empty);
                    }
                    _ => {}
                }
            }

            let unit_value = match val {
                Value::Unit(u) => Value::Unit(u),
                Value::Ref(r) => match r.borrow().clone() {
                    Value::Unit(u) => Value::Unit(u),
                    other => {
                        let u = rt::CastInto::<ustr::Ustr>::cast(self.ctx, other)?;
                        Value::Unit(u)
                    }
                },
                other => {
                    let u = rt::CastInto::<ustr::Ustr>::cast(self.ctx, other)?;
                    Value::Unit(u)
                }
            };

            if !self.value_matches_ty(&unit_value, &ty) {
                return Err(TypeError::mismatch(
                    ty.pretty_string(self.ctx),
                    arg.span()
                        .into_spanned(unit_value.ty().pretty_string(self.ctx)),
                )
                .into());
            }

            return Ok(unit_value);
        }

        let val = Interp::<Value>::eval(arg, self)?;
        if param.is_optional() && matches!(val, Value::Empty) {
            return Ok(Value::Empty);
        }

        let coerced = rt::coerce::to_ty(self.ctx, val, ty.clone());
        if ty != rt::Ty::Any && !self.value_matches_ty(&coerced, &ty) {
            return Err(TypeError::mismatch(
                ty.pretty_string(self.ctx),
                arg.span()
                    .into_spanned(coerced.ty().pretty_string(self.ctx)),
            )
            .into());
        }

        Ok(coerced)
    }

    fn eval_call_args(
        &mut self,
        params: &[rt::Param],
        is_native: bool,
        args: ListNode<Expr>,
        call_site: SourceSpan,
        fn_name: Ustr,
    ) -> InterpResult<Vec<Value>> {
        let is_variadic = params.last().map_or(false, |p| p.is_variadic());
        let fixed_param_count = if is_variadic {
            params.len() - 1
        } else {
            params.len()
        };

        let mut values: Vec<Option<Value>> = vec![None; fixed_param_count];
        let mut variadic = vec![];
        let mut saw_named = false;
        let mut next_positional = 0;

        for arg in args.iter() {
            let mut handled_named = false;
            if let ExprKind::Assign(bind, expr) = &arg.kind {
                if let BindPatKind::Var(ident) = &bind.kind {
                    saw_named = true;
                    handled_named = true;

                    if let Some((index, param)) = params
                        .iter()
                        .enumerate()
                        .find(|(_, param)| param.name.raw == ident.raw)
                    {
                        if index >= fixed_param_count {
                            return Err(TypeError::simple(
                                ident
                                    .span()
                                    .into_spanned("variadic parameter cannot be named".to_string()),
                            )
                            .into());
                        }
                        if values[index].is_some() {
                            return Err(TypeError::simple(
                                ident.span().into_spanned("duplicate argument".to_string()),
                            )
                            .into());
                        }

                        let value = self.eval_arg_for_param(param, expr)?;
                        values[index] = Some(value);
                    } else {
                        return Err(TypeError::simple(
                            ident
                                .span()
                                .into_spanned(format!("unknown parameter: {}", ident.raw)),
                        )
                        .into());
                    }
                }
            }

            if handled_named {
                continue;
            }

            if saw_named {
                return Err(TypeError::simple(
                    arg.span()
                        .into_spanned("positional argument after named argument".to_string()),
                )
                .into());
            }

            if next_positional < fixed_param_count {
                let param = &params[next_positional];
                if let ExprKind::Splat(_) = &arg.kind {
                    return Err(TypeError::mismatch(
                        "regular argument".to_string(),
                        arg.span().into_spanned(
                            "splat can only be used for variadic parameters".to_string(),
                        ),
                    )
                    .into());
                }
                let value = self.eval_arg_for_param(param, arg)?;
                values[next_positional] = Some(value);
                next_positional += 1;
            } else if is_variadic {
                if let ExprKind::Splat(inner) = &arg.kind {
                    let inner_val = inner.eval(self)?;
                    match inner_val {
                        Value::List(list) => {
                            variadic.extend(list.borrow_slice().iter().cloned());
                        }
                        Value::Tuple(tuple) => {
                            for item in tuple.iter() {
                                variadic.push((**item).clone());
                            }
                        }
                        _ => {
                            return Err(TypeError::mismatch(
                                "list or tuple".to_string(),
                                arg.span().into_spanned(
                                    "splat can only be applied to lists or tuples".to_string(),
                                ),
                            )
                            .into());
                        }
                    }
                } else {
                    variadic.push(arg.eval(self)?);
                }
            } else {
                return Err(TypeError::mismatch(
                    format!("function {} expects {} argument(s)", fn_name, params.len(),),
                    arg.span().into_spanned("unexpected".to_string()),
                )
                .into());
            }
        }

        if !is_variadic {
            for arg in args.iter() {
                if let ExprKind::Splat(_) = &arg.kind {
                    return Err(TypeError::mismatch(
                        "regular argument".to_string(),
                        arg.span().into_spanned(
                            "splat can only be used for variadic parameters".to_string(),
                        ),
                    )
                    .into());
                }
            }
        }

        let mut coerced_values = Vec::new();
        for (index, param) in params[..fixed_param_count].iter().enumerate() {
            let value = if let Some(value) = values[index].take() {
                value
            } else if param.is_optional() {
                Value::Empty
            } else {
                return Err(TypeError::simple(
                    call_site.into_spanned(format!("missing argument: {}", param.name.raw)),
                )
                .into());
            };
            coerced_values.push(value);
        }

        if is_variadic {
            if is_native {
                coerced_values.extend(variadic);
            } else {
                coerced_values.push(Value::list(variadic));
            }
        }

        Ok(coerced_values)
    }

    fn invoke(
        &mut self,
        f: &Function,
        args: ListNode<Expr>,
        call_site: SourceSpan,
    ) -> InterpResult<Value> {
        use rt::FunctionKind;
        self.trace_debug(&format!(
            "invoke function {}({})",
            f.name.raw,
            args.pretty_string(&())
        ));

        let is_variadic = f.params.last().map_or(false, |p| p.is_variadic());
        let fixed_param_count = if is_variadic {
            f.params.len() - 1
        } else {
            f.params.len()
        };

        let mut values: Vec<Option<Value>> = vec![None; fixed_param_count];
        let mut variadic = vec![];
        let mut saw_named = false;
        let mut next_positional = 0;

        for arg in args.iter() {
            let mut handled_named = false;
            if let ExprKind::Assign(bind, expr) = &arg.kind {
                if let BindPatKind::Var(ident) = &bind.kind {
                    saw_named = true;
                    handled_named = true;

                    if let Some((index, param)) = f
                        .params
                        .iter()
                        .enumerate()
                        .find(|(_, param)| param.name.raw == ident.raw)
                    {
                        if index >= fixed_param_count {
                            return Err(TypeError::simple(
                                ident
                                    .span()
                                    .into_spanned("variadic parameter cannot be named".to_string()),
                            )
                            .into());
                        }
                        if values[index].is_some() {
                            return Err(TypeError::simple(
                                ident.span().into_spanned("duplicate argument".to_string()),
                            )
                            .into());
                        }

                        let value = self.eval_arg_for_param(param, expr)?;
                        values[index] = Some(value);
                    } else {
                        return Err(TypeError::simple(
                            ident
                                .span()
                                .into_spanned(format!("unknown parameter: {}", ident.raw)),
                        )
                        .into());
                    }
                }
            }

            if handled_named {
                continue;
            }

            if saw_named {
                return Err(TypeError::simple(
                    arg.span()
                        .into_spanned("positional argument after named argument".to_string()),
                )
                .into());
            }

            if next_positional < fixed_param_count {
                let param = &f.params[next_positional];
                if let ExprKind::Splat(_) = &arg.kind {
                    return Err(TypeError::mismatch(
                        "regular argument".to_string(),
                        arg.span().into_spanned(
                            "splat can only be used for variadic parameters".to_string(),
                        ),
                    )
                    .into());
                }
                let value = self.eval_arg_for_param(param, arg)?;
                values[next_positional] = Some(value);
                next_positional += 1;
            } else if is_variadic {
                if let ExprKind::Splat(inner) = &arg.kind {
                    let inner_val = inner.eval(self)?;
                    match inner_val {
                        Value::List(list) => {
                            variadic.extend(list.borrow_slice().iter().cloned());
                        }
                        Value::Tuple(tuple) => {
                            for item in tuple.iter() {
                                variadic.push((**item).clone());
                            }
                        }
                        _ => {
                            return Err(TypeError::mismatch(
                                "list or tuple".to_string(),
                                arg.span().into_spanned(
                                    "splat can only be applied to lists or tuples".to_string(),
                                ),
                            )
                            .into());
                        }
                    }
                } else {
                    variadic.push(arg.eval(self)?);
                }
            } else {
                return Err(TypeError::mismatch(
                    format!(
                        "function {} expects {} argument(s)",
                        f.name.raw,
                        f.params.len(),
                    ),
                    arg.span().into_spanned("unexpected".to_string()),
                )
                .into());
            }
        }

        let mut coerced_values = Vec::new();
        for (index, param) in f.params[..fixed_param_count].iter().enumerate() {
            let value = if let Some(value) = values[index].take() {
                value
            } else if param.is_optional() {
                Value::Empty
            } else {
                return Err(TypeError::simple(
                    call_site.into_spanned(format!("missing argument: {}", param.name.raw)),
                )
                .into());
            };
            coerced_values.push(value);
        }

        if is_variadic {
            if matches!(&f.kind, rt::FunctionKind::Native(_)) {
                coerced_values.extend(variadic);
            } else {
                coerced_values.push(Value::list(variadic));
            }
        } else {
            for arg in args.iter() {
                if let ExprKind::Splat(_) = &arg.kind {
                    return Err(TypeError::mismatch(
                        "regular argument".to_string(),
                        arg.span().into_spanned(
                            "splat can only be used for variadic parameters".to_string(),
                        ),
                    )
                    .into());
                }
            }
        }

        // invoke the function
        let run_call = |intrp: &mut Interpreter<'ctx>, coerced_values: Vec<Value>| match &f.kind {
            FunctionKind::Native(builtin) => {
                let frame = StackFrame::new(f.name, call_site);
                let scope = LocalScope::new();
                Context::with_fn_call(intrp, frame, scope, |intrp| {
                    builtin(intrp.ctx, coerced_values).map_err(InterpError::from)
                })
            }
            FunctionKind::Source(block) => {
                let frame = StackFrame::new(f.name, call_site);
                let scope = LocalScope::from(
                    f.params
                        .iter()
                        .map(|p| p.name.raw)
                        .zip(coerced_values.into_iter()),
                );

                Context::with_fn_call(intrp, frame, scope, |intrp| {
                    Interp::<Value>::eval(block, intrp)
                })
            }
            FunctionKind::Lambda { body, captures } => {
                let frame = StackFrame::new(f.name, call_site);
                let mut scopes = Vec::with_capacity(2);
                scopes.push(LocalScope::from_refs(captures.iter().cloned()));
                scopes.push(LocalScope::from(
                    f.params
                        .iter()
                        .map(|p| p.name.raw)
                        .zip(coerced_values.into_iter()),
                ));

                Context::with_lambda_call(intrp, frame, scopes, |intrp| {
                    Interp::<Value>::eval(body, intrp)
                })
            }
            FunctionKind::BuiltinWrapper {
                target_name,
                target_params,
                target_fn,
                args,
            } => {
                let scope = LocalScope::from(
                    f.params
                        .iter()
                        .map(|p| p.name.raw)
                        .zip(coerced_values.into_iter()),
                );
                let target_values = Context::with_scope(intrp, scope, |intrp| {
                    intrp.eval_call_args(
                        target_params,
                        true,
                        args.clone(),
                        call_site,
                        target_name.raw,
                    )
                })?;

                let frame = StackFrame::new(target_name.clone(), call_site);
                let scope = LocalScope::new();
                Context::with_fn_call(intrp, frame, scope, |intrp| {
                    target_fn(intrp.ctx, target_values).map_err(InterpError::from)
                })
            }
        };

        let module_id = match &f.kind {
            FunctionKind::Native(_) => None,
            _ => f.module_id,
        };
        let result = if let Some(module_id) = module_id {
            Context::with_active_module(self, module_id, |intrp| run_call(intrp, coerced_values))
        } else {
            run_call(self, coerced_values)
        };

        match result {
            Ok(value) => self.coerce_and_check_return(f, value, call_site),
            Err(InterpError::Return(value)) => self.coerce_and_check_return(f, value, call_site),
            Err(err) => Err(err),
        }
    }

    fn invoke_method(
        &mut self,
        recv: Value,
        method_name: Ustr,
        args: Vec<Value>,
        call_site: SourceSpan,
    ) -> InterpResult<Value> {
        let recv_value = match recv {
            Value::Ref(r) => r.borrow().clone(),
            other => other,
        };

        let module_id = self
            .ctx
            .active_module()
            .map(|module| module.id)
            .ok_or_else(|| {
                InterpError::Exception(
                    Exception::new("RuntimeError", "no active module".to_string())
                        .with_backtrace(self.ctx.backtrace()),
                )
            })?;

        let func = match &recv_value {
            Value::UserType(user_ty) => {
                let type_name = user_ty.tag();
                self.ctx
                    .modules
                    .resolve_type_method_in(
                        module_id,
                        Spanned::new(type_name, call_site),
                        Spanned::new(method_name, call_site),
                    )
                    .map_err(InterpError::from)?
            }
            other => {
                let ty = other.ty();
                let type_name = builtin_type_name(&ty).ok_or_else(|| {
                    TypeError::simple(call_site.into_spanned(format!(
                        "expected user type or builtin type on lhs of '.', found {}",
                        ty.pretty_string(self.ctx)
                    )))
                })?;

                self.ctx
                    .modules
                    .resolve_builtin_type_method_in(
                        module_id,
                        Spanned::new(type_name, call_site),
                        Spanned::new(method_name, call_site),
                    )
                    .map_err(InterpError::from)?
            }
        };

        let mut call_args = Vec::with_capacity(1 + args.len());
        call_args.push(recv_value);
        call_args.extend(args);

        self.invoke_with_values(&func, call_args, call_site)
    }

    /// Invoke a function using already-evaluated argument values.
    pub fn invoke_with_values(
        &mut self,
        f: &Function,
        mut values: Vec<Value>,
        call_site: SourceSpan,
    ) -> InterpResult<Value> {
        use rt::FunctionKind;

        let is_variadic = f.params.last().map_or(false, |p| p.is_variadic());
        let expected_fixed_args = if is_variadic {
            f.params.len() - 1
        } else {
            f.params.len()
        };
        let required_fixed_args = f.params[..expected_fixed_args]
            .iter()
            .take_while(|param| !param.is_optional())
            .count();

        if values.len() < required_fixed_args {
            return Err(InterpError::TypeError(TypeError {
                expected: Some(format!(
                    "function {} expects at least {} argument(s)",
                    f.name.raw, required_fixed_args,
                )),
                found: call_site.into_spanned(format!("found {}", values.len())),
                context: None,
            }));
        }
        if !is_variadic && values.len() > expected_fixed_args {
            return Err(InterpError::TypeError(TypeError {
                expected: Some(format!(
                    "function {} expects {} argument(s)",
                    f.name.raw, expected_fixed_args,
                )),
                found: call_site.into_spanned(format!("found {}", values.len())),
                context: None,
            }));
        }

        if values.len() < expected_fixed_args {
            values.extend(std::iter::repeat(Value::Empty).take(expected_fixed_args - values.len()));
        }

        let mut coerced_values = Vec::new();
        for (i, param) in f.params.iter().enumerate() {
            if param.is_variadic() {
                let rest = values.split_off(i);
                if matches!(&f.kind, FunctionKind::Native(_)) {
                    coerced_values.extend(rest);
                } else {
                    coerced_values.push(Value::List(List::new(rest)));
                }
                break;
            }

            let mut v = values.get(i).cloned().unwrap_or(Value::Empty);

            if param.is_optional() && matches!(v, Value::Empty) {
                coerced_values.push(Value::Empty);
                continue;
            }

            let ty = param.ty.clone().map_or(rt::Ty::Any, |ty| ty.raw);
            if ty.is_ref() {
                match v {
                    Value::Ref(_) => coerced_values.push(v),
                    _ => {
                        return Err(InterpError::TypeError(TypeError {
                            expected: Some("reference".to_string()),
                            found: call_site.into_spanned(v.ty().pretty_string(self.ctx)),
                            context: Some(param.name.to_string_inner()),
                        }))
                    }
                }
            } else {
                let coerced = rt::coerce::to_ty(self.ctx, v, ty.clone());
                if ty != rt::Ty::Any && !self.value_matches_ty(&coerced, &ty) {
                    return Err(TypeError::mismatch(
                        ty.pretty_string(self.ctx),
                        call_site.into_spanned(coerced.ty().pretty_string(self.ctx)),
                    )
                    .into());
                }
                coerced_values.push(coerced);
            }
        }

        let run_call = |intrp: &mut Interpreter<'ctx>, coerced_values: Vec<Value>| match &f.kind {
            FunctionKind::Native(builtin) => {
                builtin(intrp.ctx, coerced_values).map_err(InterpError::from)
            }
            FunctionKind::Source(body) => {
                let frame = StackFrame::new(f.name.clone(), call_site);
                let scope = LocalScope::from(
                    f.params
                        .iter()
                        .map(|p| p.name.raw)
                        .zip(coerced_values.into_iter()),
                );

                Context::with_fn_call(intrp, frame, scope, |intrp| {
                    Interp::<Value>::eval(body, intrp)
                })
            }
            FunctionKind::Lambda { body, captures } => {
                let frame = StackFrame::new(f.name.clone(), call_site);
                let mut scopes = Vec::with_capacity(2);
                scopes.push(LocalScope::from_refs(captures.iter().cloned()));
                scopes.push(LocalScope::from(
                    f.params
                        .iter()
                        .map(|p| p.name.raw)
                        .zip(coerced_values.into_iter()),
                ));

                Context::with_lambda_call(intrp, frame, scopes, |intrp| {
                    Interp::<Value>::eval(body, intrp)
                })
            }
            FunctionKind::BuiltinWrapper {
                target_name,
                target_params,
                target_fn,
                args,
            } => {
                let scope = LocalScope::from(
                    f.params
                        .iter()
                        .map(|p| p.name.raw)
                        .zip(coerced_values.into_iter()),
                );
                let target_values = Context::with_scope(intrp, scope, |intrp| {
                    intrp.eval_call_args(
                        target_params,
                        true,
                        args.clone(),
                        call_site,
                        target_name.raw,
                    )
                })?;

                let frame = StackFrame::new(target_name.clone(), call_site);
                let scope = LocalScope::new();
                Context::with_fn_call(intrp, frame, scope, |intrp| {
                    target_fn(intrp.ctx, target_values).map_err(InterpError::from)
                })
            }
        };

        let module_id = match &f.kind {
            FunctionKind::Native(_) => None,
            _ => f.module_id,
        };
        let result = if let Some(module_id) = module_id {
            Context::with_active_module(self, module_id, |intrp| run_call(intrp, coerced_values))
        } else {
            run_call(self, coerced_values)
        };

        match result {
            Ok(value) => self.coerce_and_check_return(f, value, call_site),
            Err(InterpError::Return(value)) => self.coerce_and_check_return(f, value, call_site),
            Err(err) => Err(err),
        }
    }

    fn coerce_and_check_return(
        &mut self,
        f: &Function,
        value: Value,
        call_site: SourceSpan,
    ) -> InterpResult<Value> {
        let ret_ty = match &f.ret {
            Some(ret) => ret.raw.clone(),
            None => return Ok(value),
        };

        if f.ret_optional {
            let is_empty = match &value {
                Value::Empty => true,
                Value::Ref(r) if matches!(&*r.borrow(), Value::Empty) => true,
                _ => false,
            };
            if is_empty {
                return Ok(value);
            }
        }

        let value = rt::coerce::to_ty(self.ctx, value, ret_ty.clone());
        if self.value_matches_ty(&value, &ret_ty) {
            Ok(value)
        } else {
            Err(InterpError::TypeError(
                TypeError::mismatch(
                    ret_ty.pretty_string(self.ctx),
                    call_site.into_spanned(value.ty().pretty_string(self.ctx)),
                )
                .with_context(f.name.to_string_inner()),
            ))
        }
    }

    fn value_matches_ty(&self, value: &Value, ty: &rt::Ty) -> bool {
        match ty {
            rt::Ty::Any => true,
            rt::Ty::Empty => {
                matches!(value, Value::Empty)
                    || matches!(value, Value::Ref(r) if matches!(&*r.borrow(), Value::Empty))
            }
            rt::Ty::Bool => matches!(value.ty(), rt::Ty::Bool),
            rt::Ty::Float => matches!(value.ty(), rt::Ty::Float),
            rt::Ty::Int => matches!(value.ty(), rt::Ty::Int),
            rt::Ty::Str => matches!(value.ty(), rt::Ty::Str),
            rt::Ty::Num => matches!(value.ty(), rt::Ty::Int | rt::Ty::Float | rt::Ty::Dim(_)),
            rt::Ty::Function => matches!(value.ty(), rt::Ty::Function),
            rt::Ty::Iter => matches!(
                value.ty(),
                rt::Ty::List | rt::Ty::Tuple(_) | rt::Ty::Object | rt::Ty::Str | rt::Ty::Iter
            ),
            rt::Ty::UserType(tag) => {
                matches!(value.ty(), rt::Ty::UserType(found) if &found == tag)
            }
            rt::Ty::Dim(target) => {
                let dim_matches = |dim: &rt::Dim| {
                    if dim.expr != target.expr {
                        return false;
                    }

                    if target.unit.is_some() {
                        dim == target
                    } else {
                        true
                    }
                };

                match value {
                    Value::Quantity(q) => dim_matches(&q.dim),
                    Value::Ref(r) => {
                        let borrowed = r.borrow();
                        match &*borrowed {
                            Value::Quantity(q) => dim_matches(&q.dim),
                            _ => false,
                        }
                    }
                    _ => false,
                }
            }
            rt::Ty::List => matches!(value.ty(), rt::Ty::List),
            rt::Ty::Object => matches!(value.ty(), rt::Ty::Object),
            rt::Ty::Tuple(expected) => match value {
                Value::Tuple(items) => {
                    if expected.is_empty() {
                        return true;
                    }
                    if items.len() != expected.len() {
                        return false;
                    }

                    for (item, expected_ty) in items.iter().zip(expected.iter()) {
                        if !self.value_matches_ty(item, expected_ty) {
                            return false;
                        }
                    }

                    true
                }
                Value::Ref(r) => {
                    let borrowed = r.borrow();
                    match &*borrowed {
                        Value::Tuple(items) => {
                            if expected.is_empty() {
                                return true;
                            }
                            if items.len() != expected.len() {
                                return false;
                            }

                            for (item, expected_ty) in items.iter().zip(expected.iter()) {
                                if !self.value_matches_ty(item, expected_ty) {
                                    return false;
                                }
                            }

                            true
                        }
                        _ => false,
                    }
                }
                _ => false,
            },
            rt::Ty::Unit => matches!(value.ty(), rt::Ty::Unit),
            rt::Ty::Type => matches!(value.ty(), rt::Ty::Type),
            rt::Ty::Ref(inner) => match value {
                Value::Ref(r) => self.value_matches_ty(&r.borrow(), inner),
                _ => false,
            },
        }
    }
}

fn type_error_to_exception(ctx: &Context, err: TypeError) -> Exception {
    let message = match &err.expected {
        Some(expected) => format!(
            "expected '{}', found '{}'",
            expected,
            err.found.raw.as_str()
        ),
        None => err.found.raw.as_str().to_string(),
    };

    let mut exception = Exception::new("TypeError", message)
        .with_primary_span(err.found.span)
        .with_backtrace(ctx.backtrace());

    if let Some(context) = err.context {
        exception = exception.with_extra(Spanned::new(
            format!("in function '{}'", context.raw.as_str()),
            context.span,
        ));
    }

    exception
}

fn builtin_type_name(ty: &rt::Ty) -> Option<Ustr> {
    match ty {
        rt::Ty::Any => Some(Ustr::from("any")),
        rt::Ty::Bool => Some(Ustr::from("bool")),
        rt::Ty::Int => Some(Ustr::from("int")),
        rt::Ty::Float => Some(Ustr::from("float")),
        rt::Ty::Num => Some(Ustr::from("num")),
        rt::Ty::Str => Some(Ustr::from("str")),
        rt::Ty::Function => Some(Ustr::from("fn")),
        rt::Ty::Iter => Some(Ustr::from("iter")),
        rt::Ty::List => Some(Ustr::from("list")),
        rt::Ty::Object => Some(Ustr::from("object")),
        rt::Ty::Unit => Some(Ustr::from("unit")),
        rt::Ty::Type => Some(Ustr::from("type")),
        _ => None,
    }
}

/// Convenience for calling a function value from builtin code.
pub fn call_function(
    ctx: &mut Context,
    f: &Function,
    values: Vec<Value>,
) -> Result<Value, Exception> {
    let mut intrp = Interpreter::new(ctx);
    match intrp.invoke_with_values(f, values, SourceSpan::default()) {
        Ok(v) => Ok(v),
        Err(InterpError::Return(v)) => Ok(v),
        Err(InterpError::Exception(e)) => Err(e),
        Err(InterpError::TypeError(err)) => Err(type_error_to_exception(ctx, err)),
        Err(other) => {
            Err(Exception::new("RuntimeError", other.to_string()).with_backtrace(ctx.backtrace()))
        }
    }
}

impl ContextProvider for Interpreter<'_> {
    fn context(&self) -> &Context {
        self.ctx
    }

    fn context_mut(&mut self) -> &mut Context {
        self.ctx
    }
}

//
// MARK: Traits
//

/// A trait for nodes that can be evaluated by an `Interpreter`.
pub(super) trait Interp<'ctx, T> {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<T>;
}

impl<'ctx, T: Interp<'ctx, U>, U> Interp<'ctx, U> for Box<T> {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<U> {
        self.as_ref().eval(intrp)
    }
}

impl<'ctx, T, U> Interp<'ctx, Vec<U>> for ListNode<T>
where
    T: Interp<'ctx, U> + PrettyPrint<()>,
    U: PrettyPrint<Context>,
{
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Vec<U>> {
        no_trace! {self, intrp, "Interp::<U>::ListNode", {
            self.iter().map(|item| item.eval(intrp)).collect::<InterpResult<Vec<U>>>()
        }}
    }
}

impl<'ctx> Interp<'ctx, Value> for ListNode<Stmt> {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Value> {
        trace! {self, intrp, "Interp::<Value>::ListNode<Stmt>", {
            for item in &self.items[..self.items.len() - 1] {
                match &item.kind {
                    StmtKind::Break => {
                        return Err(InterpError::Break);
                    }
                    StmtKind::Continue => {
                        return Err(InterpError::Continue);
                    }
                    StmtKind::Expr(expr) => Interp::<Value>::eval(expr, intrp)?,
                    StmtKind::Return(expr) => {
                        return Err(InterpError::Return(expr.eval(intrp)?));
                    }
                };
            }

            Ok(match &self.items.last().unwrap().kind {
                StmtKind::Break => return Err(InterpError::Break),
                StmtKind::Continue => return Err(InterpError::Continue),
                StmtKind::Expr(expr) => expr.eval(intrp)?,
                StmtKind::Return(expr) => return Err(InterpError::Return(expr.eval(intrp)?)),
            })
        }}
    }
}

impl<'ctx> Interp<'ctx, Value> for ListNode<Expr> {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Value> {
        trace! {self, intrp, "Interp::<Value>::ListNode<Expr>", {
            for item in &self.items[..self.items.len() - 1] {
                Interp::<Value>::eval(item, intrp)?;
            }
            self.items.last().unwrap().eval(intrp)
        }}
    }
}

impl<'ctx> Interp<'ctx, Option<Value>> for Item {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Option<Value>> {
        match &self.kind {
            ItemKind::Import(import) => match import {
                Import::Path(path) => {
                    match intrp.import_module(path) {
                        Ok(module_id) => {
                            intrp
                                .active_module()
                                .register_module_alias(path.name_part(), module_id)?;
                        }
                        Err(module_err) => {
                            if path.parts.len() < 2 {
                                return Err(module_err);
                            }

                            let (module_parts, member) = path.parts.split_at(path.parts.len() - 1);
                            let module_path = Path::new(module_parts.to_vec());
                            let module_id = intrp.import_module(&module_path)?;
                            intrp.import_member(module_id, member[0])?;
                        }
                    }
                    Ok(None)
                }
                Import::Members { module, members } => {
                    let module_id = intrp.import_module(module)?;
                    for member in members {
                        intrp.import_member(module_id, *member)?;
                    }
                    Ok(None)
                }
            },
            ItemKind::Directive(d) => d.eval(intrp).map(|_| None),
            ItemKind::DimDecl(decl) => decl.eval(intrp).map(|_| None),
            ItemKind::UnitDecl(decl) => decl.eval(intrp).map(|_| None),
            ItemKind::OpDecl(decl) => decl.eval(intrp).map(|_| None),
            ItemKind::ConstDecl(decl) => decl.eval(intrp).map(|_| None),
            ItemKind::FnDecl(decl) => Interp::<()>::eval(decl, intrp).map(|_| None),
            ItemKind::TypeDecl(decl) => Interp::<()>::eval(decl, intrp).map(|_| None),
            ItemKind::ModuleDecl(decl) => {
                let parent_id = intrp.ctx.active_module().unwrap().id;
                let existing_name = decl.name.as_spanned_string();
                let module_ids = module_scope_ids(intrp.ctx, parent_id);

                for module_id in module_ids {
                    let module = &intrp.ctx.modules[module_id];
                    match module.names.resolve(&decl.name.raw) {
                        rt::NameResult::Constant(constant) => {
                            return Err(InterpError::from(rt::DeclError::new(
                                "module",
                                existing_name.clone(),
                                constant.name.span(),
                            )));
                        }
                        rt::NameResult::Function(func) => {
                            return Err(InterpError::from(rt::DeclError::new(
                                "module",
                                existing_name.clone(),
                                func.name.span(),
                            )));
                        }
                        rt::NameResult::Ambiguous(funcs) => {
                            if let Some(func) = funcs.first() {
                                return Err(InterpError::from(rt::DeclError::new(
                                    "module",
                                    existing_name.clone(),
                                    func.name.span(),
                                )));
                            }
                        }
                        rt::NameResult::None => {}
                    }
                    if let Some(existing) = module.types.get(&decl.name.raw) {
                        return Err(InterpError::from(rt::DeclError::new(
                            "module",
                            existing_name.clone(),
                            existing.name.span(),
                        )));
                    }
                }
                let mut module_path = intrp.ctx.modules.module_path(parent_id).clone();
                module_path.push(decl.name.as_spanned_ustr());

                let mut opened = intrp.ctx.active_module().unwrap().opened.clone();
                if !opened.contains(&parent_id) {
                    opened.push(parent_id);
                }

                let parent_graph = intrp.ctx.active_module().unwrap().conversion_graph.clone();
                let module_id = {
                    let module = intrp
                        .ctx
                        .modules
                        .new_module(module_path)
                        .map_err(InterpError::from)?;
                    module.opened = opened;
                    module.conversion_graph = parent_graph;
                    module.id
                };

                let source_id = intrp.current_source;
                Context::with_active_module(intrp.ctx, module_id, |ctx| {
                    let mut nested = Interpreter::new(ctx);
                    if let Some(source_id) = source_id {
                        nested = nested.with_source(source_id);
                    }
                    for item in &decl.items {
                        item.eval(&mut nested)?;
                    }
                    Ok::<(), InterpError>(())
                })?;
                Ok(None)
            }
            ItemKind::Expr(expr) => {
                let v = Interp::<Value>::eval(expr, intrp)?;
                intrp.ctx.set_last_value(v.clone());
                Ok(Some(v))
            }
        }
    }
}

//
// MARK: Item Impls
//

impl<'ctx> Interp<'ctx, ()> for Directive {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<()> {
        no_trace! {self, intrp, "Interp::<()>::Directive", {
            match &self.kind {
                &DirectiveKind::FloatPrecision(prec) => {
                    if prec == 0 || prec > MAX_FLOAT_PRECISION {
                        let msg = format!(
                            "float_precision must be between 1 and {}, got {}",
                            MAX_FLOAT_PRECISION, prec
                        );
                        return Err(
                            Exception::new("ValueError", msg)
                                .with_primary_span(self.span())
                                .into(),
                        );
                    }
                    intrp.ctx.config.float_precision = prec
                }
                DirectiveKind::SignificantFigures(places) => {
                    if let Some(places) = places {
                        let places = *places;
                        if places == 0 || places > MAX_SIGNIFICANT_FIGURES {
                            let msg = format!(
                                "significant_figures must be between 1 and {}, got {}",
                                MAX_SIGNIFICANT_FIGURES, places
                            );
                            return Err(
                                Exception::new("ValueError", msg)
                                    .with_primary_span(self.span())
                                    .into(),
                            );
                        }
                        intrp.ctx.config.decimal_places = Some(places);
                    } else {
                        intrp.ctx.config.decimal_places = None;
                    }
                }
                DirectiveKind::DefaultFormatter(name) => {
                    intrp.ctx.set_default_formatter(name.raw);
                }
                _ => (), // nothing to do for other directives, as they are used during parsing
            }
            Ok(())
        }}
    }
}

impl<'ctx> Interp<'ctx, ()> for DimDecl {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<()> {
        no_trace! {self, intrp, "Interp::<()>::DimDecl", {
            let name = self.name.as_spanned_ustr();
            let dim_expr = match &self.dimension {
                Some(expr) => expr.eval(intrp)?,
                None => rt::DimExpr::Dimension(self.name.raw),
            };

            let dimension = if let Some(ref label) = self.label {
                rt::Dimension::with_label(name, dim_expr, label.as_spanned_ustr())
            } else {
                rt::Dimension::new(name, dim_expr)
            };

            intrp
                .active_module()
                .register_dimension(dimension)
                .map_err(InterpError::from)
        }}
    }
}

impl<'ctx> Interp<'ctx, ()> for UnitDecl {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<()> {
        no_trace! {self, intrp, "Interp::<()>::UnitDecl", {
            let kind = self.kind;
            let name = self.name.as_spanned_ustr();
            let suffixes = self.suffixes.iter().map(|s| s.as_spanned_ustr()).collect();

            let unit = match (&self.dimension, &self.value) {
                // Expression-based unit: dimension is None, compute from expression
                (None, Some(Left(expr))) => {
                    let value = Interp::<Value>::eval(expr, intrp)?;

                    // Extract dimension and conversion factor from the evaluated expression
                    match value {
                        Value::Quantity(q) => {
                            let dim_expr = q.dim.expr.clone();
                            let num = q.number.clone();
                            rt::Unit::new(kind, name, suffixes, dim_expr, num)
                        }
                        _ => {
                            // Scalar value - treat as dimensionless
                            let num = match CastInto::<rt::Number>::cast(intrp.ctx, value) {
                                Ok(num) => num,
                                Err(_) => {
                                    return Err(TypeError::mismatch(
                                        format!("expected scalar or quantity value in unit declaration"),
                                        expr.span().into_spanned("given value".to_string()),
                                    )
                                    .into());
                                }
                            };
                            rt::Unit::new(kind, name, suffixes, rt::DimExpr::one(), num)
                        }
                    }
                }

                // Standard units with explicit dimension
                (Some(dimension), value_opt) => {
                    let dim_expr = dimension.eval(intrp)?;

                    match value_opt {
                        Some(value) => match value {
                            Left(expr) => {
                                let value = Interp::<Value>::eval(expr, intrp)?;
                                let num = match CastInto::<rt::Number>::cast(intrp.ctx, value) {
                                    Ok(num) => num,
                                    Err(_) => {
                                        return Err(TypeError::mismatch(
                                            format!("expected scalar value in unit declaration"),
                                            expr.span().into_spanned("given value".to_string()),
                                        )
                                        .into());
                                    }
                                };
                                rt::Unit::new(kind, name, suffixes, dim_expr, num)
                            }
                            Right(unit_impl) => {
                                let impl_obj = Interp::<rt::UnitImpl>::eval(unit_impl, intrp)?;
                                let conversion = rt::Conversion::Impl(impl_obj);
                                rt::Unit::with_conversion(kind, name, suffixes, dim_expr, conversion)
                            },
                        },
                        None => rt::Unit::new(kind, name, suffixes, dim_expr, rt::Number::Int(1.into()))
                    }
                }

                // Invalid: dimension None with unit_impl
                (None, Some(Right(_))) => {
                    return Err(TypeError::simple(
                        self.name.span().into_spanned("unit implementation requires explicit dimension annotation".to_string())
                    ).into());
                }

                // Invalid: neither dimension nor value
                (None, None) => {
                    return Err(TypeError::simple(
                        self.name.span().into_spanned("unit declaration requires either dimension or value expression".to_string())
                    ).into());
                }
            };

            // Use update_unit instead of register_unit to replace placeholder units registered during parsing
            intrp.active_module().update_unit(unit);
            Ok(())
        }}
    }
}

impl<'ctx> Interp<'ctx, rt::UnitImpl> for UnitImpl {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<rt::UnitImpl> {
        no_trace! {self, intrp, "Interp::<rt::UnitImpl>::UnitImpl", {
            // Evaluate ALL functions to get Function objects
            let all_funcs: Result<Vec<_>, _> = self.functions.iter()
                .map(|decl| Interp::<Function>::eval(decl, intrp))
                .collect();
            let all_funcs = all_funcs?;

            // Validate against UnitImpl interface from builtin module
            let builtin_module = intrp.ctx.modules.get_module("builtin")
                .expect("builtin module should exist");
            if let Some(interface) = builtin_module.get_interface("UnitImpl").cloned() {
                // Validate all implemented functions
                // This will check that:
                // 1. All required functions are present
                // 2. All provided functions match expected signatures
                // 3. No extra functions are defined that aren't part of the interface
                interface.validate(intrp.ctx, all_funcs.iter().collect())?;
            }

            // Extract the specific functions we need
            let to_base_func = all_funcs.iter().find(|f| f.name.raw == "to_base").unwrap().clone();
            let from_base_func = all_funcs.iter().find(|f| f.name.raw == "from_base").unwrap().clone();
            let display_name_func = all_funcs.iter().find(|f| f.name.raw == "display_name").cloned();

            // Get module ID for storing with the UnitImpl
            let module_id = intrp.active_module().id;

            // Register as unnamed functions and get their IDs
            let to_base_id = intrp.active_module().register_unnamed_function(to_base_func);
            let from_base_id = intrp.active_module().register_unnamed_function(from_base_func);
            let display_name_id = display_name_func.map(|f| intrp.active_module().register_unnamed_function(f));

            // Create and return the runtime UnitImpl
            Ok(rt::UnitImpl::new(module_id, to_base_id, from_base_id, display_name_id))
        }}
    }
}

impl<'ctx> Interp<'ctx, ()> for OpDecl {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<()> {
        no_trace! {self, intrp, "Interp::<()>::FnDecl", {
            let name = self.name.as_spanned_ustr();
            let params = self.params.eval(intrp)?;
            let func = match &self.body {
                Left(path) => Interp::<Function>::eval(path, intrp)?,
                Right(exprs) => {
                    // if the operator declaration has the function defined inline, we need to lift it
                    // into a proper registered function so that path resolution works.
                    let func = Function::source(name, params, exprs.clone());
                    intrp
                        .active_module()
                        .register_function(func)
                        .map_err(InterpError::from);

                    intrp.active_module().resolve_function(name).unwrap().clone()
                },
            };

            let expected_params = match &self.kind {
                OpKind::Prefix | OpKind::Postfix => 1,
                OpKind::Infix => 2,
            };
            if func.params.len() != expected_params {
                return Err(TypeError::mismatch(
                    format!(
                        "expected function that takes {} parameter(s)",
                        expected_params
                    ),
                    Spanned::new(
                        format!("given function accepts {}", func.params.len()),
                        self.body.span(),
                    ),
                )
                .into());
            }

            Ok(())
        }}
    }
}

impl<'ctx> Interp<'ctx, ()> for ConstDecl {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<()> {
        no_trace! {self, intrp, "Interp::<()>::ConstDecl", {
            let name = self.name.as_spanned_ustr();
            let value = Interp::<Value>::eval(&self.value, intrp)?;

            let module_id = intrp.ctx.active_module().unwrap().id;
            let existing_name = self.name.as_spanned_string();
            for module_id in module_scope_ids(intrp.ctx, module_id) {
                let module = &intrp.ctx.modules[module_id];
                if let Some(existing) = module.types.get(&name.raw) {
                    return Err(InterpError::from(rt::DeclError::new(
                        "constant",
                        existing_name.clone(),
                        existing.name.span(),
                    )));
                }
            }

            let constant = Constant::new(name, value);
            intrp
                .active_module()
                .register_constant(constant)
                .map_err(InterpError::from)
        }}
    }
}

impl<'ctx> Interp<'ctx, ()> for TypeDecl {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<()> {
        no_trace! {self, intrp, "Interp::<()>::TypeDecl", {
            let module_id = intrp.ctx.active_module().unwrap().id;
            let existing_name = self.name.as_spanned_string();
            for module_id in module_scope_ids(intrp.ctx, module_id) {
                let module = &intrp.ctx.modules[module_id];
                match module.names.resolve(&self.name.raw) {
                    rt::NameResult::Constant(constant) => {
                        return Err(InterpError::from(rt::DeclError::new(
                            "type",
                            existing_name.clone(),
                            constant.name.span(),
                        )));
                    }
                    rt::NameResult::Function(func) => {
                        return Err(InterpError::from(rt::DeclError::new(
                            "type",
                            existing_name.clone(),
                            func.name.span(),
                        )));
                    }
                    rt::NameResult::Ambiguous(funcs) => {
                        if let Some(func) = funcs.first() {
                            return Err(InterpError::from(rt::DeclError::new(
                                "type",
                                existing_name.clone(),
                                func.name.span(),
                            )));
                        }
                    }
                    rt::NameResult::None => {}
                }
            }

            let name = self.name.as_spanned_ustr();
            let target = intrp
                .ctx
                .resolve_type(self.target.path_parts())
                .map_err(InterpError::from)?;
            let ty = target.clone_with_name(name);
            intrp
                .active_module()
                .register_type(ty)
                .map_err(InterpError::from)
        }}
    }
}

impl<'ctx> Interp<'ctx, Function> for FnDecl {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Function> {
        no_trace! {self, intrp, "Interp::<Function>::FnDecl", {
            validate_params(self.params.items.as_slice())?;

            let name = self.name.as_spanned_ustr();
            let is_constructor = name.raw == Ustr::from("new");

            let receiver_ty = if let Some(receiver) = &self.receiver {
                let ty = receiver.eval(intrp)?;
                if !is_constructor {
                    match ty {
                        rt::Ty::UserType(type_name) => {
                            if intrp.active_module().types.get(&type_name).is_none() {
                                return Err(TypeError::simple(
                                    receiver
                                        .span()
                                        .into_spanned(format!("undefined type '{}'", type_name)),
                                )
                                .into());
                            }
                        }
                        _ => {
                            let type_name = constructor_type_name(&ty).ok_or_else(|| {
                                TypeError::simple(
                                    receiver
                                        .span()
                                        .into_spanned("methods require a named type".to_string()),
                                )
                            })?;
                            if !is_builtin_type_name(type_name) {
                                return Err(TypeError::simple(
                                    receiver.span().into_spanned(
                                        "methods can only be declared on user types or builtin types"
                                            .to_string(),
                                    ),
                                )
                                .into());
                            }
                        }
                    }
                } else if let rt::Ty::UserType(type_name) = ty {
                    if intrp.active_module().types.get(&type_name).is_none() {
                        return Err(TypeError::simple(
                            receiver
                                .span()
                                .into_spanned(format!("undefined type '{}'", type_name)),
                        )
                        .into());
                    }
                }
                Some(ty)
            } else {
                None
            };

            let params = self.params.eval(intrp)?;
            if let Some(receiver_ty) = &receiver_ty {
                if !is_constructor {
                    let param = self.params.items.get(0).ok_or_else(|| {
                        TypeError::simple(
                            self.span()
                                .into_spanned("methods must take the receiver as the first parameter".to_string()),
                        )
                    })?;
                    let param_ty = params
                        .get(0)
                        .and_then(|param| param.ty.as_ref())
                        .map(|ty| &ty.raw)
                        .ok_or_else(|| {
                            TypeError::simple(
                                param
                                    .span()
                                    .into_spanned("first parameter must be typed for a method".to_string()),
                            )
                        })?;
                    if param_ty != receiver_ty {
                        return Err(TypeError::mismatch(
                            receiver_ty.pretty_string(intrp.ctx),
                            param.span().into_spanned(format!(
                                "expected '{}' as first parameter type",
                                receiver_ty.pretty_string(intrp.ctx)
                            )),
                        )
                        .into());
                    }
                }
            }
            let (ret, ret_optional) = match &self.ret {
                Some(Left(dim_node)) => {
                    let ty = if let DimExprKind::Ident(ident) = &dim_node.kind {
                        let module_id = intrp.ctx.active_module().unwrap().id;
                        if let Ok(unit) = intrp
                            .ctx
                            .modules
                            .resolve_unit_suffix_in(module_id, ident.as_spanned_ustr())
                        {
                            rt::Ty::Dim(rt::Dim::simple(
                                unit.dim_expr.clone(),
                                ident.raw,
                                unit.conversion.clone(),
                            ))
                        } else {
                            rt::Ty::Dim(rt::Dim::from(dim_node.eval(intrp)?))
                        }
                    } else {
                        rt::Ty::Dim(rt::Dim::from(dim_node.eval(intrp)?))
                    };
                    (Some(dim_node.span().into_spanned(ty)), false)
                }
                Some(Right(ty_node)) => {
                    let span = ty_node.span();
                    let mut raw_ty = ty_node;
                    let mut ret_optional = false;
                    loop {
                        match &raw_ty.kind {
                            TyKind::Optional(inner) => {
                                ret_optional = true;
                                raw_ty = inner;
                            }
                            _ => break,
                        }
                    }
                    let ty = raw_ty.eval(intrp)?;
                    (Some(span.into_spanned(ty)), ret_optional)
                }
                None => (None, false),
            };
            let kind = if self.is_builtin_wrapper {
                if self.body.items.len() != 1 {
                    return Err(TypeError::simple(
                        self.body
                            .span()
                            .into_spanned("builtin wrappers must contain one call expression".to_string()),
                    )
                    .into());
                }

                let stmt = &self.body.items[0];
                let call_expr = match &stmt.kind {
                    StmtKind::Expr(expr) => expr.as_ref(),
                    StmtKind::Return(expr) => expr.as_ref(),
                    _ => {
                        return Err(TypeError::simple(
                            stmt.span()
                                .into_spanned("builtin wrappers must call a builtin function".to_string()),
                        )
                        .into())
                    }
                };

                let (target_path, target_args) = match &call_expr.kind {
                    ExprKind::FnCall(path, args) => (path, args),
                    _ => {
                        return Err(TypeError::simple(
                            call_expr
                                .span()
                                .into_spanned("builtin wrappers must call a builtin function".to_string()),
                        )
                        .into())
                    }
                };

                let target = Interp::<Function>::eval(target_path, intrp)?;
                let (target_name, target_params, target_fn) = match target.kind {
                    rt::FunctionKind::Native(func) => (target.name, target.params, func),
                    _ => {
                        return Err(TypeError::simple(
                            target_path
                                .span()
                                .into_spanned("builtin wrappers must target a builtin function".to_string()),
                        )
                        .into())
                    }
                };

                rt::FunctionKind::BuiltinWrapper {
                    target_name,
                    target_params,
                    target_fn,
                    args: target_args.clone(),
                }
            } else {
                rt::FunctionKind::Source(self.body.clone())
            };

            Ok(Function::new(name, params, kind, ret, ret_optional))
        }}
    }
}

impl<'ctx> Interp<'ctx, ()> for FnDecl {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<()> {
        let mut func = Interp::<Function>::eval(self, intrp)?;
        let receiver_ty = if let Some(receiver) = &self.receiver {
            Some(receiver.eval(intrp)?)
        } else {
            None
        };
        if let Some(receiver_ty) = &receiver_ty {
            func.module_id = Some(intrp.active_module().id);
            if func.name.raw == Ustr::from("new") {
                match receiver_ty {
                    rt::Ty::UserType(type_name) => {
                        let type_name = *type_name;
                        let module = intrp.active_module();
                        let target = module.types.get_mut(&type_name).ok_or_else(|| {
                            TypeError::simple(
                                self.span()
                                    .into_spanned(format!("undefined type '{}'", type_name)),
                            )
                        })?;
                        target.register_constructor(func).map_err(InterpError::from)
                    }
                    _ => {
                        let name = constructor_type_name(receiver_ty).ok_or_else(|| {
                            TypeError::simple(
                                self.span()
                                    .into_spanned("constructors require a named type".to_string()),
                            )
                        })?;
                        let span = self
                            .receiver
                            .as_ref()
                            .map(|receiver| receiver.span())
                            .unwrap_or_else(SourceSpan::default);
                        intrp
                            .active_module()
                            .register_constructor(Spanned::new(name, span), func)
                            .map_err(InterpError::from)
                    }
                }
            } else {
                match receiver_ty {
                    rt::Ty::UserType(type_name) => {
                        let type_name = *type_name;
                        let module = intrp.active_module();
                        let target = module.types.get_mut(&type_name).ok_or_else(|| {
                            TypeError::simple(
                                self.span()
                                    .into_spanned(format!("undefined type '{}'", type_name)),
                            )
                        })?;
                        target.register_method(func).map_err(InterpError::from)
                    }
                    _ => {
                        let type_name = constructor_type_name(receiver_ty).ok_or_else(|| {
                            TypeError::simple(
                                self.span()
                                    .into_spanned("methods require a named type".to_string()),
                            )
                        })?;
                        if !is_builtin_type_name(type_name) {
                            return Err(TypeError::simple(
                                self.span().into_spanned(
                                    "methods can only be declared on user types or builtin types"
                                        .to_string(),
                                ),
                            )
                            .into());
                        }
                        let span = self
                            .receiver
                            .as_ref()
                            .map(|receiver| receiver.span())
                            .unwrap_or_else(SourceSpan::default);
                        intrp
                            .active_module()
                            .register_builtin_type_method(Spanned::new(type_name, span), func)
                            .map_err(InterpError::from)
                    }
                }
            }
        } else {
            let module_id = intrp.ctx.active_module().unwrap().id;
            let existing_name = self.name.as_spanned_string();
            for module_id in module_scope_ids(intrp.ctx, module_id) {
                let module = &intrp.ctx.modules[module_id];
                if let Some(existing) = module.types.get(&func.name.raw) {
                    return Err(InterpError::from(rt::DeclError::new(
                        "function",
                        existing_name.clone(),
                        existing.name.span(),
                    )));
                }
            }
            intrp
                .active_module()
                .register_function(func)
                .map_err(InterpError::from)
        }
    }
}

impl<'ctx> Interp<'ctx, rt::Param> for Param {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<rt::Param> {
        no_trace! {self, intrp, "Interp::<Param>::Param", {
            if self.is_variadic {
                assert!(self.anno.is_none());
                return Ok(rt::Param::variadic(self.name.as_spanned_ustr()));
            }

            let ty = match &self.anno {
                Some(Left(dim_node)) => {
                    // Check if this is a simple identifier that might be a unit constraint (e.g., [rad])
                    let ty = if let DimExprKind::Ident(ident) = &dim_node.kind {
                            // Try to resolve as a unit suffix first
                        let module_id = intrp.ctx.active_module().unwrap().id;
                        if let Ok(unit) = intrp
                            .ctx
                            .modules
                            .resolve_unit_suffix_in(module_id, ident.as_spanned_ustr())
                        {
                            // This is a unit constraint - create a Dim with unit info
                            rt::Ty::Dim(rt::Dim::simple(unit.dim_expr.clone(), ident.raw, unit.conversion.clone()))
                        } else {
                            // Not a unit, treat as a regular dimension expression
                            rt::Ty::Dim(rt::Dim::from(dim_node.eval(intrp)?))
                        }
                    } else {
                        // Complex dimension expression
                        rt::Ty::Dim(rt::Dim::from(dim_node.eval(intrp)?))
                    };
                    Some(dim_node.span().into_spanned(ty))
                },
                Some(Right(ty_node)) => {
                    let ty = ty_node.eval(intrp)?;
                    Some(ty_node.span().into_spanned(ty))
                },
                None => None,
            };
            if self.is_optional {
                Ok(rt::Param::optional(self.name.as_spanned_ustr(), ty))
            } else {
                Ok(rt::Param::new(self.name.as_spanned_ustr(), ty))
            }
        }}
    }
}

//
// MARK: Expr Impls
//

impl<'ctx> Interp<'ctx, rt::DimExpr> for DimExpr {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<rt::DimExpr> {
        use rt::DimExpr;
        Ok(match &self.kind {
            DimExprKind::Mul(lhs, rhs) => {
                let lhs = lhs.eval(intrp)?;
                let rhs = rhs.eval(intrp)?;
                DimExpr::Mul(lhs.into(), rhs.into())
            }
            DimExprKind::Div(lhs, rhs) => {
                let lhs = lhs.eval(intrp)?;
                let rhs = rhs.eval(intrp)?;
                DimExpr::Div(lhs.into(), rhs.into())
            }
            DimExprKind::Pow(lhs, rhs) => {
                let lhs = lhs.eval(intrp)?;
                let rhs = rhs.eval(intrp)?;
                DimExpr::Pow(lhs.into(), rhs.into())
            }
            DimExprKind::Neg(expr) => {
                let expr = expr.eval(intrp)?;
                DimExpr::Neg(expr.into())
            }
            DimExprKind::Ident(name) => {
                let module_id = intrp.ctx.active_module().unwrap().id;
                let dim = intrp
                    .ctx
                    .modules
                    .resolve_dimension_in(module_id, name.as_spanned_ustr())
                    .map_err(InterpError::from)?;

                dim.expr.clone()
            }
            DimExprKind::Number(num) => {
                let number = num.eval(intrp)?;
                DimExpr::Number(number)
            }
            DimExprKind::Unit(suffix) => {
                // For unit constraints like [rad], we look up the unit and return its dimension
                let module_id = intrp.ctx.active_module().unwrap().id;
                let unit = intrp
                    .ctx
                    .modules
                    .resolve_unit_suffix_in(module_id, suffix.as_spanned_ustr())
                    .map_err(InterpError::from)?;

                unit.dim_expr.clone()
            }
        })
    }
}

impl<'ctx> Interp<'ctx, LRValue> for Expr {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<LRValue> {
        trace! {self, intrp, "Interp::<Value>::Expr", {
            match &self.kind {
                ExprKind::Assign(lhs, rhs) => {
                    let pat = lhs.eval(intrp)?;
                    let value = Interp::<Value>::eval(rhs, intrp)?;
                    let bindings = pat.bind_with(intrp.ctx, value.clone())?;

                    let mut new_bindings = vec![];
                    for (name, value) in bindings.into_iter() {
                        match intrp.ctx.resolve_variable(name) {
                            Ok(vref) if vref.is_mut() => {
                                vref.set(intrp.ctx, value)
                                    .map_err(InterpError::from)?
                            }
                            Ok(_) => new_bindings.push((name, value)), // allow overshadowing the constant
                            Err(_) => new_bindings.push((name, value)), // no existing variable, create one
                        }
                    }

                    if !new_bindings.is_empty() {
                        let scope = LocalScope::from(new_bindings.into_iter());
                        intrp.ctx.push_local_scope(scope);
                    }
                    Ok(LRValue::R(value))
                }
                ExprKind::InfixOp(op, lhs, rhs) => {
                    // Special-case method dispatch via the (.) operator so the RHS
                    // method name/args are preserved instead of eagerly evaluated.
                    if op.raw.as_str() == "." {
                        let recv = Interp::<Value>::eval(lhs, intrp)?;

                        let (method_name, arg_values) = match &rhs.kind {
                            ExprKind::FnCall(func_path, args) => {
                                let name = match func_path.parts.as_slice() {
                                    [ident] => ident.raw,
                                    _ => {
                                        return Err(InterpError::TypeError(TypeError::mismatch(
                                            "method name".to_string(),
                                            rhs.span().into_spanned(
                                                "expected simple method name on RHS of '.'".to_string(),
                                            ),
                                        )))
                                    }
                                };

                                let values = args
                                    .iter()
                                    .map(|arg| Interp::<Value>::eval(arg, intrp))
                                    .collect::<InterpResult<Vec<_>>>()?;
                                (name, values)
                            }
                            ExprKind::Ident(ident) => (ident.raw, Vec::new()),
                            ExprKind::Path(path) if path.parts.len() == 1 => {
                                (path.parts[0].raw, Vec::new())
                            }
                            _ => {
                                return Err(InterpError::TypeError(TypeError::mismatch(
                                    "method call".to_string(),
                                    rhs.span().into_spanned(
                                        "expected method call on RHS of '.'".to_string(),
                                    ),
                                )))
                            }
                        };

                        let result =
                            intrp.invoke_method(recv, method_name, arg_values, self.span())?;
                        Ok(LRValue::R(result))
                    } else {
                        let func = op.eval(intrp)?;
                        let args = ListNode::from(vec![*lhs.clone(), *rhs.clone()]);
                        Ok(LRValue::R(intrp.invoke(&func, args, op.span())?))
                    }
                }
                ExprKind::Empty => Ok(LRValue::R(Value::Empty)),
                ExprKind::IndexAssign(container, index, value) => {
                    // Evaluate container and index refs (container must be mutable for list/object)
                    let container_ref = Interp::<ValueRef>::eval(container, intrp)?;
                    let idx_val = Interp::<Value>::eval(index, intrp)?;
                    let new_val = Interp::<Value>::eval(value, intrp)?;

                    let mut container_mut = container_ref
                        .borrow_mut(intrp.ctx)
                        .map_err(InterpError::from)?;
                    match &mut *container_mut {
                        Value::List(list) => {
                            let idx_usize = match idx_val {
                                Value::Quantity(q) if q.is_dimless() => q
                                    .number
                                    .into_int(intrp.ctx)
                                    .map_err(InterpError::from)?
                                    .to_usize()
                                    .ok_or_else(|| {
                                        InterpError::Exception(Exception::new(
                                            "IndexError",
                                            "index must be non-negative".to_string(),
                                        )
                                        .with_backtrace(intrp.ctx.backtrace()))
                                    })?,
                                _ => {
                                    return Err(InterpError::Exception(Exception::new(
                                        "TypeError",
                                        "list index must be an integer".to_string(),
                                    )
                                    .with_backtrace(intrp.ctx.backtrace())));
                                }
                            };

                            if list.set(idx_usize, new_val).is_none() {
                                return Err(InterpError::Exception(
                                    Exception::new(
                                        "IndexError",
                                        format!("list index out of range: {}", idx_usize),
                                    )
                                    .with_backtrace(intrp.ctx.backtrace()),
                                ));
                            }
                            Ok(LRValue::R(Value::Empty))
                        }
                        Value::Object(object) => {
                            let key = match idx_val {
                                Value::String(s) => s,
                                _ => {
                                    return Err(InterpError::Exception(
                                        Exception::new(
                                            "TypeError",
                                            "object indices must be strings".to_string(),
                                        )
                                        .with_backtrace(intrp.ctx.backtrace()),
                                    ));
                                }
                            };

                            let key_ustr = Ustr::from(&key);
                            let mut fields = object.borrow_mut();
                            if let Some((_, val)) = fields.iter_mut().find(|(k, _)| *k == key_ustr) {
                                *val = new_val;
                            } else {
                                fields.push((key_ustr, new_val));
                            }
                            Ok(LRValue::R(Value::Empty))
                        }
                        Value::Tuple(_) => Err(InterpError::Exception(
                            Exception::new("TypeError", "cannot assign into tuple".to_string())
                                .with_backtrace(intrp.ctx.backtrace()),
                        )),
                        other => Err(InterpError::Exception(
                            Exception::new(
                                "TypeError",
                                format!(
                                    "cannot assign to index of type: {}",
                                    other.ty().pretty_string(intrp.ctx)
                                ),
                            )
                            .with_backtrace(intrp.ctx.backtrace()),
                        )),
                    }
                }
                ExprKind::PrefixOp(op, expr) => {
                    let func = op.eval(intrp)?;
                    let args = ListNode::from(vec![*expr.clone()]);
                    Ok(LRValue::R(intrp.invoke(&func, args, op.span())?))
                }
                ExprKind::PostfixOp(expr, op) => {
                    let func = op.eval(intrp)?;
                    let args = ListNode::from(vec![*expr.clone()]);
                    Ok(LRValue::R(intrp.invoke(&func, args, op.span())?))
                }
                ExprKind::UnitCast(expr, unit) => {
                    let value = Interp::<Value>::eval(expr, intrp)?;

                    use rt::Number;
                    match value {
                        Value::Quantity(q) => {
                            let (name, conv, expr) = {
                                let module_id = intrp.ctx.active_module().unwrap().id;
                                let unit = intrp
                                    .ctx
                                    .modules
                                    .resolve_unit_suffix_in(
                                        module_id,
                                        unit.span().into_spanned(unit.name),
                                    )
                                    .map_err(InterpError::from)?;

                                (unit.name.raw, unit.conversion.clone(), unit.dim_expr.clone())
                            };

                            // Delayed conversion: keep value in display units, not base units
                            // Only convert to base when mixing units or explicitly requested
                            let number = q.number.clone();
                            let dim = (expr, name, conv).into();
                            let quantity: rt::Quantity = (number, dim).into();
                            Ok(LRValue::R(Value::Quantity(quantity)))
                        }
                        _ => Err(TypeError::mismatch(
                            format!("expected number"),
                            expr.span().into_spanned("given value".to_string()),
                        )
                        .into()),
                    }
                }
                ExprKind::If(if_expr) => {
                    for branch in if_expr.branches.iter() {
                        let cond = Interp::<Value>::eval(&branch.cond, intrp)?;
                        if !cond.is_zero() {
                            let value = Interp::<Value>::eval(&branch.body, intrp)?;
                            return Ok(LRValue::R(value));
                        }
                    }

                    if let Some(else_) = &if_expr.else_branch {
                        let value = Interp::<Value>::eval(else_, intrp)?;
                        Ok(LRValue::R(value))
                    } else {
                        Ok(LRValue::R(Value::Empty))
                    }
                }
                ExprKind::Try(try_expr) => {
                    let result = match &try_expr.body {
                        TryBody::Expr(expr) => Interp::<Value>::eval(expr, intrp),
                        TryBody::Block(body) => Interp::<Value>::eval(body, intrp),
                    };

                    match result {
                        Ok(value) => Ok(LRValue::R(value)),
                        Err(InterpError::Exception(err)) => {
                            if let Some(catch) = &try_expr.catch {
                                let err_value = Value::String(err.to_string());
                                if let Some(binding) = &catch.binding {
                                    let scope = LocalScope::from(
                                        std::iter::once((binding.raw, err_value)),
                                    );
                                    let value = Context::with_scope(intrp, scope, |intrp| {
                                        Interp::<Value>::eval(&catch.body, intrp)
                                    })?;
                                    Ok(LRValue::R(value))
                                } else {
                                    let value = Interp::<Value>::eval(&catch.body, intrp)?;
                                    Ok(LRValue::R(value))
                                }
                            } else {
                                Ok(LRValue::R(Value::Empty))
                            }
                        }
                        Err(err) => Err(err),
                    }
                }
                ExprKind::ForRange(pat, iter, body) => {
                    let pat = pat.eval(intrp)?;
                    let mut iter = Interp::<ValueRef>::eval(iter, intrp)?.try_into_iter(intrp.ctx)?;
                    loop {
                        match iter.as_mut().next(intrp.ctx)? {
                            Some(value) => {
                                let scope = LocalScope::from(pat.bind_with(intrp.ctx, value)?.into_iter());
                                match Context::with_scope(intrp, scope, |intrp| Interp::<Value>::eval(body, intrp)) {
                                    Ok(_) => {}
                                    Err(InterpError::Continue) => continue,
                                    Err(InterpError::Break) => break,
                                    Err(e) => return Err(e),
                                }
                            }
                            None => break,
                        }
                    }
                    Ok(LRValue::R(Value::Empty))
                }
                ExprKind::FnCall(func, args) => {
                    let span = func.span().union_with(args.span());
                    match Interp::<Function>::eval(func, intrp) {
                        Ok(func) => Ok(LRValue::R(intrp.invoke(&func, args.clone(), span)?)),
                        Err(InterpError::NameError(err)) => {
                            match intrp.ctx.resolve_type(func.path_parts()) {
                                Ok(ty) => {
                                    if let Some(ctor) = ty.get_constructor() {
                                        Ok(LRValue::R(intrp.invoke(&ctor, args.clone(), span)?))
                                    } else {
                                        let name = func.path_parts().to_spanned_string();
                                        Err(InterpError::NameError(NameError::new(
                                            "undefined constructor",
                                            name,
                                        )))
                                    }
                                }
                                Err(_) => {
                                    if func.parts.len() == 1
                                        && is_builtin_type_name(func.name_part().raw)
                                    {
                                        let module_id = intrp.ctx.active_module().unwrap().id;
                                        let ctor = intrp
                                            .ctx
                                            .modules
                                            .resolve_constructor_in(module_id, func.name_part())
                                            .map(|ctor| ctor.clone());
                                        match ctor {
                                            Ok(ctor) => Ok(LRValue::R(
                                                intrp.invoke(&ctor, args.clone(), span)?,
                                            )),
                                            Err(err) => Err(InterpError::NameError(err)),
                                        }
                                    } else {
                                        Err(InterpError::NameError(err))
                                    }
                                }
                            }
                        }
                        Err(err) => Err(err),
                    }
                }
                ExprKind::Lambda(lambda) => {
                    validate_params(lambda.params.as_slice())?;
                    let params = lambda
                        .params
                        .iter()
                        .map(|param| param.eval(intrp))
                        .collect::<InterpResult<Vec<_>>>()?;

                    let mut collector =
                        CaptureCollector::new(lambda.params.as_slice(), intrp.ctx.local_scopes());
                    match &lambda.body {
                        LambdaBody::Expr(expr) => collector.visit_expr(expr),
                        LambdaBody::Block(body) => collector.visit_block(body),
                    }
                    let captures = collector.into_captures();

                    let body = match &lambda.body {
                        LambdaBody::Expr(expr) => {
                            ListNode::new(vec![Stmt::expr((**expr).clone())])
                        }
                        LambdaBody::Block(body) => body.clone(),
                    };

                    let name = Spanned::new(Ustr::from("<lambda>"), self.span());
                    let mut func = Function::lambda(name, params, body, captures);
                    if let Some(module) = intrp.ctx.active_module() {
                        func.module_id = Some(module.id);
                    }

                    Ok(LRValue::R(Value::Function(func)))
                }
                ExprKind::List(node) => {
                    let mut values = vec![];
                    for item in node.iter() {
                        let value = Interp::<Value>::eval(item, intrp)?;
                        values.push(value.into());
                    }
                    Ok(LRValue::R(Value::List(List::new(values))))
                }
                ExprKind::Object(node) => {
                    let mut fields: Vec<(Ustr, Value)> = vec![];
                    for field in node.iter() {
                        let key = Ustr::from(field.key.raw.as_str());
                        let value = Interp::<Value>::eval(&field.value, intrp)?;
                        if let Some((_, existing)) = fields.iter_mut().find(|(k, _)| *k == key) {
                            *existing = value.into();
                        } else {
                            fields.push((key, value.into()));
                        }
                    }
                    Ok(LRValue::R(Value::object(fields)))
                }
                ExprKind::Tuple(node) => {
                    let mut values = vec![];
                    for item in node.iter() {
                        let value = Interp::<Value>::eval(item, intrp)?;
                        values.push(value.into());
                    }
                    Ok(LRValue::R(Value::Tuple(Tuple::new(SmallVec::from_vec(values)))))
                }
                ExprKind::Path(path) => {
                    let res = Interp::<ValueRef>::eval(path, intrp)?;
                    Ok(LRValue::L(res))
                }
                ExprKind::Ident(ident) => {
                    let res = Interp::<ValueRef>::eval(ident, intrp)?;
                    Ok(LRValue::L(res))
                }
                ExprKind::Number(num) => Ok(LRValue::R(
                    Interp::<rt::Number>::eval(num, intrp).map(Value::from)?,
                )),
                ExprKind::String(s) => Ok(LRValue::R(Value::String(s.clone()))),
                ExprKind::Boolean(b) => Ok(LRValue::R(Value::Boolean(*b))),
                ExprKind::Unit(unit) => {
                    let module_id = intrp.ctx.active_module().unwrap().id;
                    let unit = intrp
                        .ctx
                        .modules
                        .resolve_unit_suffix_in(module_id, unit.clone().into_raw_spanned())?;
                    Ok(LRValue::R(Value::Unit(unit.name.into())))
                }
                ExprKind::Slice(container, start, stop) => {
                    let value = Interp::<Value>::eval(container, intrp)?;
                    let start_val = match start {
                        Some(expr) => Some(Interp::<Value>::eval(expr, intrp)?),
                        None => None,
                    };
                    let stop_val = match stop {
                        Some(expr) => Some(Interp::<Value>::eval(expr, intrp)?),
                        None => None,
                    };

                    let slice_result = apply_slice(intrp.ctx, value, start_val, stop_val)?;
                    Ok(LRValue::R(slice_result))
                }
                ExprKind::Type(ty) => {
                    let ty = ty.eval(intrp)?;
                    Ok(LRValue::R(Value::Ty(ty)))
                }
                ExprKind::Splat(_) => {
                    Err(TypeError::mismatch(
                        "splat expressions can only be used as arguments to variadic functions".to_string(),
                        self.span().into_spanned("invalid splat usage".to_string()),
                    ).into())
                }
            }
        }}
    }
}

impl<'ctx> Interp<'ctx, Value> for Expr {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Value> {
        match Interp::<LRValue>::eval(self, intrp)? {
            LRValue::L(vref) => Ok(vref.get()),
            LRValue::R(value) => Ok(value),
        }
    }
}

fn value_to_isize(ctx: &mut Context, value: Value) -> Result<isize, Exception> {
    match value {
        Value::Ref(r) => value_to_isize(ctx, r.borrow().clone()),
        Value::Quantity(q) if q.is_dimless() => {
            let int = q.number.into_int(ctx)?;
            int.to_isize().ok_or_else(|| {
                Exception::new(
                    "IndexError",
                    "index must be non-negative and in range".to_string(),
                )
                .with_backtrace(ctx.backtrace())
            })
        }
        other => Err(Exception::new(
            "TypeError",
            format!(
                "slice indices must be integers, found {}",
                other.ty().pretty_string(ctx)
            ),
        )
        .with_backtrace(ctx.backtrace())),
    }
}

fn apply_slice(
    ctx: &mut Context,
    container: Value,
    start: Option<Value>,
    stop: Option<Value>,
) -> Result<Value, InterpError> {
    let (len, slicer): (usize, Box<dyn Fn(usize, usize) -> Value>) = match container {
        Value::List(list) => {
            let len = list.len();
            let slicer = move |start, stop| Value::List(list.slice(start, stop));
            (len, Box::new(slicer))
        }
        Value::Tuple(tuple) => {
            let items: Vec<Value> = tuple.iter().map(|v| (**v).clone()).collect();
            let len = items.len();
            let slicer = move |start, stop| {
                let slice = items[start..stop].to_vec();
                Value::Tuple(Tuple::new(SmallVec::from_vec(
                    slice.into_iter().map(Box::new).collect(),
                )))
            };
            (len, Box::new(slicer))
        }
        Value::String(s) => {
            let chars: Vec<char> = s.chars().collect();
            let len = chars.len();
            let slicer = move |start, stop| {
                let slice: String = chars[start..stop].iter().collect();
                Value::String(slice)
            };
            (len, Box::new(slicer))
        }
        other => {
            return Err(InterpError::from(
                Exception::new(
                    "TypeError",
                    format!("cannot slice type: {}", other.ty().pretty_string(ctx)),
                )
                .with_backtrace(ctx.backtrace()),
            ))
        }
    };

    let len_isize = len as isize;

    let mut start_idx = match start {
        Some(v) => value_to_isize(ctx, v)?,
        None => 0,
    };
    let mut stop_idx = match stop {
        Some(v) => value_to_isize(ctx, v)?,
        None => len_isize,
    };

    if start_idx < 0 {
        start_idx += len_isize;
    }
    if stop_idx < 0 {
        stop_idx += len_isize;
    }

    start_idx = start_idx.clamp(0, len_isize);
    stop_idx = stop_idx.clamp(0, len_isize);

    let (start_u, stop_u) = if start_idx > stop_idx {
        let v = stop_idx as usize;
        (v, v)
    } else {
        (start_idx as usize, stop_idx as usize)
    };

    Ok(slicer(start_u, stop_u))
}

impl<'ctx> Interp<'ctx, ValueRef> for Expr {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<ValueRef> {
        match Interp::<LRValue>::eval(self, intrp)? {
            LRValue::L(vref) => Ok(vref),
            LRValue::R(value) => Ok(ValueRef::new(value)),
        }
    }
}

impl<'ctx> Interp<'ctx, rt::Pattern> for BindPat {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<rt::Pattern> {
        trace! {self, intrp, "Interp::<Pattern>::BindPat", {
            use rt::Pattern;
            Ok(match &self.kind {
                BindPatKind::Ignored => Pattern::Ignore,
                BindPatKind::Var(ident) => Pattern::Var(ident.as_spanned_ustr()),
                BindPatKind::Tuple(pats) => {
                    let pats = pats.eval(intrp)?;
                    Pattern::Tuple(pats)
                }
            })
        }}
    }
}

impl<'ctx> Interp<'ctx, rt::Ty> for Ty {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<rt::Ty> {
        use rt::Ty;
        no_trace! {self, intrp, "Interp::<Ty>::Ty", {
            Ok(match &self.kind {
                TyKind::Any => Ty::Any,
                TyKind::Bool => Ty::Bool,
                TyKind::Int => Ty::Int,
                TyKind::Float => Ty::Float,
                TyKind::Str => Ty::Str,
                TyKind::Function => Ty::Function,
                TyKind::Iter => Ty::Iter,
                TyKind::Io => Ty::UserType("io".into()),
                TyKind::UserType(name) => Ty::UserType(*name),
                TyKind::Num => Ty::Num,
                TyKind::Unit => Ty::Unit,
                TyKind::Type => Ty::Type,
                TyKind::List => Ty::List,
                TyKind::Object => Ty::Object,
                TyKind::Tuple(tys) => {
                    let tys = tys.eval(intrp)?.into_iter().map(|ty| Box::new(ty)).collect();
                    Ty::Tuple(SmallVec::from_vec(tys))
                },
                TyKind::Ref(r) => Ty::Ref(Box::new(r.eval(intrp)?)),
                TyKind::Optional(ty) => ty.eval(intrp)?,
            })
        }}
    }
}

impl<'ctx> Interp<'ctx, Function> for Operator {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Function> {
        trace! {self, intrp, "Interp::<Function>::Operator", {
            let module_id = intrp.ctx.active_module().unwrap().id;
            let op = intrp
                .ctx
                .modules
                .resolve_operator_in(module_id, self.kind, self.as_spanned_ustr())
                .map_err(InterpError::from)?;
            match op.func.clone() {
                Left(path) => path.eval(intrp),
                Right(body) => todo!(),
            }
        }}
    }
}

impl<'ctx> Interp<'ctx, Value> for Path {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Value> {
        intrp
            .ctx
            .resolve_variable(self.path_parts())
            .map(|vref| vref.get())
            .or_else(|_| {
                intrp
                    .ctx
                    .resolve_function(self.path_parts())
                    .map(|f| Value::Function(f.clone()))
            })
            .map_err(InterpError::from)
    }
}

impl<'ctx> Interp<'ctx, ValueRef> for Path {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<ValueRef> {
        intrp
            .ctx
            .resolve_variable(self.path_parts())
            .map_err(InterpError::from)
    }
}

impl<'ctx> Interp<'ctx, Function> for Path {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Function> {
        trace! {self, intrp, "Interp::<Function>::Path", {
            if let Ok(vref) = intrp.ctx.resolve_variable(self.path_parts()) {
                let value = vref.get();
                let func = match &value {
                    Value::Function(func) => Some(func.clone()),
                    Value::Ref(inner) => match inner.get() {
                        Value::Function(func) => Some(func),
                        _ => None,
                    },
                    _ => None,
                };

                if let Some(func) = func {
                    return Ok(func);
                }

                return Err(TypeError::mismatch(
                    "function".to_string(),
                    self.span()
                        .into_spanned(value.ty().pretty_string(intrp.ctx)),
                )
                .into());
            }

            intrp
                .ctx
                .resolve_function(self.path_parts())
                .map_err(InterpError::from)
                .cloned()
        }}
    }
}

impl<'ctx> Interp<'ctx, Value> for Ident {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Value> {
        trace! {self, intrp, "Interp::<Value>::Ident", {
            let name = self.as_spanned_ustr();
            intrp
                .ctx
                .resolve_variable(name)
                .map(|vref| vref.get())
                .or_else(|_| intrp.ctx.resolve_function(name).map(|f| Value::Function(f.clone())))
                .map_err(InterpError::from)
        }}
    }
}

impl<'ctx> Interp<'ctx, ValueRef> for Ident {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<ValueRef> {
        intrp
            .ctx
            .resolve_variable(self.as_spanned_ustr())
            .map_err(InterpError::from)
    }
}

impl<'ctx> Interp<'ctx, Function> for Ident {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<Function> {
        trace! {self, intrp, "Interp::<Function>::Ident", {
            let name = self.as_spanned_ustr();
            intrp.ctx.resolve_function(name).map_err(InterpError::from).cloned()
        }}
    }
}

impl<'ctx> Interp<'ctx, rt::Number> for Number {
    fn eval(&self, intrp: &mut Interpreter<'ctx>) -> InterpResult<rt::Number> {
        trace! {self, intrp, "Interp::<Number>::Number", {
            Ok(match &self.kind {
                NumberKind::Integer(v) => rt::Number::Int(v.clone()),
                NumberKind::Float(v) => rt::Number::Float(v.clone()),
            })
        }}
    }
}
