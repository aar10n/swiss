use super::builtin::NativeFn;
use super::context::Context;
use super::dimension::DimExpr;
use super::exception::Exception;
use super::value::{Ty, Value};
use super::{DeclError, TypeError};

use crate::ast::{Expr, Ident, ListNode};
use crate::print::ansi::{
    chars::{ARROW, COMMA, EQUALS, LBRAC, LPARN, RBRAC, RPARN},
    ATTR, BOLD, DIMENSION, DIRECTIVE, IDENT, KEYWORD, KIND, NUMBER, OPERATOR, PUNCT, RESET, STRING,
    UNIT,
};
use crate::print::{PrettyPrint, PrettyString};
use crate::source::{SourceSpan, Spanned};

use either::{Either, Left, Right};
use std::collections::HashMap;
use std::fmt::Debug;
use ustr::{Ustr, UstrMap};

// MARK: Constant

/// A registered constant.
#[derive(Clone, Debug)]
pub struct Constant {
    pub name: Spanned<Ustr>,
    pub value: Value,
}

impl Constant {
    pub fn new(name: Spanned<Ustr>, value: Value) -> Self {
        Self { name, value }
    }
}

impl<Ctx> PrettyPrint<Ctx> for Constant {
    fn pretty_print<Output: std::io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        level: usize,
    ) -> std::io::Result<()> {
        write!(out, "{BOLD}{}{RESET} {EQUALS} ", self.name.raw)?;
        self.value.pretty_print(out, ctx, 0)
    }
}

// MARK: Function

/// A registered function.
#[derive(Clone, Debug)]
pub struct Function {
    pub name: Spanned<Ustr>,
    pub params: Vec<Param>,
    pub kind: FunctionKind,
}

impl Function {
    pub fn new(name: Spanned<Ustr>, params: Vec<Param>, kind: FunctionKind) -> Self {
        Self { name, params, kind }
    }

    pub fn builtin(name: &str, params: Vec<Param>, func: NativeFn) -> Self {
        Self {
            name: Spanned::new(Ustr::from(name), SourceSpan::default()),
            params,
            kind: FunctionKind::Native(func),
        }
    }

    pub fn source(name: Spanned<Ustr>, params: Vec<Param>, body: ListNode<Expr>) -> Self {
        Self {
            name,
            params,
            kind: FunctionKind::Source(body),
        }
    }

    pub fn unique_name(&self) -> String {
        let arg_types = self
            .params
            .iter()
            .map(|p| p.type_string())
            .collect::<Vec<_>>()
            .join(",");

        if arg_types.is_empty() {
            format!("{}@{}", self.name.raw, self.params.len())
        } else {
            format!("{}@{}[{}]", self.name.raw, self.params.len(), arg_types)
        }
    }

    /// Checks whether another function is compatible as an overload of this function.
    /// A function is compatible if it is uniquely distinguishable by its parameters,
    /// both in count and/or type.
    pub fn check_compatible_overload(&self, other: &Function) -> Result<(), DeclError> {
        if self.params.len() != other.params.len() {
            return Ok(()); // unambiguous by parameter count
        }

        // if the parameter counts are the same, we need to check that at least one
        // parameter type is different between the two functions. different here can
        // also mean it is a stricter subtype (e.g. `int` is a stricter subtype of `num`).
        let is_unique = self
            .params
            .iter()
            .zip(other.params.iter())
            .map(|(a, b)| {
                let a = a.ty.as_ref().map(|ty| &ty.raw).unwrap_or(&Ty::Any);
                let b = b.ty.as_ref().map(|ty| &ty.raw).unwrap_or(&Ty::Any);
                Ty::is_stricter_or_not(a, b)
            })
            .any(|x| x);
        if is_unique {
            Ok(())
        } else {
            Err(DeclError::new(
                "function",
                other.name.to_string_inner(),
                self.name.span(),
            ))
        }
    }
}

impl<Ctx> PrettyPrint<Ctx> for Function {
    fn pretty_print<Output: std::io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        level: usize,
    ) -> std::io::Result<()> {
        write!(out, "{BOLD}{}{RESET} {LPARN}", self.name.raw)?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(out, "{COMMA} ")?;
            }
            param.pretty_print(out, ctx, level)?;
        }
        write!(out, "{RPARN} {ARROW} ")?;
        match &self.kind {
            FunctionKind::Native(_) => write!(out, "{KIND}native{RESET}"),
            FunctionKind::Source(_) => write!(out, "{KIND}source{RESET}"),
        }
    }
}

#[derive(Clone)]
pub enum FunctionKind {
    Native(NativeFn),
    Source(ListNode<Expr>),
}

impl Debug for FunctionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Native(arg0) => f.write_str("Native"),
            Self::Source(arg0) => f.write_str("Source"),
        }
    }
}

// MARK: Param

#[derive(Clone, Debug)]
pub struct Param {
    pub name: Spanned<Ustr>,
    pub ty: Option<Spanned<Ty>>,
}

impl Param {
    pub fn new(name: Spanned<Ustr>, ty: Option<Spanned<Ty>>) -> Self {
        Self { name, ty }
    }

    pub fn type_string(&self) -> String {
        match &self.ty {
            Some(ty) => ty.raw.to_string(),
            None => Ty::Any.to_string(),
        }
    }
}

impl From<(Ustr, Option<Ty>)> for Param {
    fn from((name, ty): (Ustr, Option<Ty>)) -> Self {
        Self {
            name: Spanned::new(name, SourceSpan::default()),
            ty: ty.map(|ty| Spanned::new(ty, SourceSpan::default())),
        }
    }
}

impl<Ctx> PrettyPrint<Ctx> for Param {
    fn pretty_print<Output: std::io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        level: usize,
    ) -> std::io::Result<()> {
        write!(out, "{IDENT}{}{RESET}", self.name.raw)?;
        if let Some(ty) = &self.ty {
            write!(out, " {DIMENSION}{}{RESET}", ty.pretty_string(&()))?;
        }
        Ok(())
    }
}

// MARK: Name

/// A registered name (constant or function).
#[derive(Clone, Debug)]
pub enum Name {
    Constant(Constant),
    Function(Function),
}

impl Name {
    pub fn constant(name: Spanned<Ustr>, value: Value) -> Self {
        Name::Constant(Constant::new(name, value))
    }

    pub fn function(name: Spanned<Ustr>, params: Vec<Param>, kind: FunctionKind) -> Self {
        Name::Function(Function::new(name, params, kind))
    }

    pub fn kind(&self) -> &str {
        match self {
            Name::Constant(_) => "constant",
            Name::Function(_) => "function",
        }
    }

    pub fn name(&self) -> &Spanned<Ustr> {
        match self {
            Name::Constant(c) => &c.name,
            Name::Function(f) => &f.name,
        }
    }

    pub fn unique_name(&self) -> String {
        match self {
            Name::Constant(c) => c.name.raw.to_string(),
            Name::Function(f) => f.unique_name(),
        }
    }

    pub fn span(&self) -> SourceSpan {
        match self {
            Name::Constant(c) => c.name.span(),
            Name::Function(f) => f.name.span(),
        }
    }

    pub fn is_constant(&self) -> bool {
        matches!(self, Name::Constant(_))
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Name::Function(_))
    }
}

// MARK: NameTable

/// A table that tracks names.
#[derive(Clone, Debug)]
pub struct NameTable {
    names: UstrMap<Either<Constant, Vec<Function>>>,
    unique_names: HashMap<String, Ustr>,
}

impl NameTable {
    pub fn new() -> Self {
        Self {
            names: UstrMap::default(),
            unique_names: HashMap::default(),
        }
    }

    pub fn contains(&self, name: &Ustr) -> bool {
        self.names.contains_key(name)
    }

    pub fn contains_unique(&self, name: &str) -> bool {
        self.unique_names.contains_key(name)
    }

    pub fn insert_constant(&mut self, constant: Constant) -> Result<(), DeclError> {
        if let Some(existing) = self.names.get_mut(&constant.name.raw) {
            return match existing {
                Left(existing) => Err(DeclError::new(
                    "constant",
                    constant.name.to_string_inner(),
                    existing.name.span(),
                )),
                Right(_) => Err(DeclError::new(
                    "constant",
                    constant.name.to_string_inner(),
                    constant.name.span(),
                )),
            };
        }

        self.names.insert(constant.name.raw, Left(constant));
        Ok(())
    }

    pub fn insert_function(&mut self, func: Function) -> Result<(), DeclError> {
        // println!("registering function: {}", func.pretty_string(&()));
        if let Some(existing) = self.names.get_mut(&func.name.raw) {
            return match existing {
                Left(constant) => Err(DeclError::new(
                    "function",
                    func.name.to_string_inner(),
                    constant.name.span(),
                )),
                Right(functions) => {
                    for f in functions.iter() {
                        f.check_compatible_overload(&func)?;
                    }
                    functions.push(func);
                    Ok(())
                }
            };
        }

        let unique_name = func.unique_name();
        let unique_name_ustr = Ustr::from(&unique_name);
        self.names.insert(func.name.raw, Right(vec![func]));
        self.unique_names.insert(unique_name, unique_name_ustr);
        Ok(())
    }

    pub fn resolve(&self, name: &Ustr) -> NameResult {
        match self.names.get(name) {
            Some(Left(constant)) => NameResult::Constant(constant),
            Some(Right(functions)) => match functions.len() {
                1 => NameResult::Function(&functions[0]),
                _ => NameResult::Ambiguous(functions),
            },
            None => NameResult::None,
        }
    }
}

#[derive(Clone, Debug)]
pub enum NameResult<'a> {
    Constant(&'a Constant),
    Function(&'a Function),
    Ambiguous(&'a Vec<Function>),
    None,
}
