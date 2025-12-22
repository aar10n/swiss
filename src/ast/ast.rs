pub use super::{node_id, KindNode, ListNode, NodeId, Spannable};

use crate::id::{ModuleId, SourceId};
use crate::source::{SourceSpan, Spanned};
use crate::{impl_identifiable, impl_spannable};

use either::{Either, Left, Right};
use smallvec::SmallVec;
use std::hash::Hash;
use ustr::{ustr, Ustr};

pub type P<T> = Box<T>;

pub struct Module {
    pub source_id: SourceId,
    pub module_id: ModuleId,
    pub items: Vec<Item>,
}

impl Module {
    pub fn new(source_id: SourceId, module_id: ModuleId, items: Vec<Item>) -> Self {
        Self {
            source_id,
            module_id,
            items,
        }
    }
}

/// An item in a module.
pub type Item = KindNode<ItemKind>;

impl Item {
    pub fn import(path: Path) -> Self {
        Self::new(ItemKind::Import(path))
    }

    pub fn directive(directive: Directive) -> Self {
        Self::new(ItemKind::Directive(directive.into()))
    }

    pub fn dim_decl(decl: DimDecl) -> Self {
        Self::new(ItemKind::DimDecl(decl.into()))
    }

    pub fn unit_decl(decl: UnitDecl) -> Self {
        Self::new(ItemKind::UnitDecl(decl.into()))
    }

    pub fn op_decl(decl: OpDecl) -> Self {
        Self::new(ItemKind::OpDecl(decl.into()))
    }

    pub fn const_decl(decl: ConstDecl) -> Self {
        Self::new(ItemKind::ConstDecl(decl.into()))
    }

    pub fn fn_decl(decl: FnDecl) -> Self {
        Self::new(ItemKind::FnDecl(decl.into()))
    }

    pub fn module_decl(decl: ModuleDecl) -> Self {
        Self::new(ItemKind::ModuleDecl(decl.into()))
    }

    pub fn expr(expr: Expr) -> Self {
        Self::new(ItemKind::Expr(expr.into()))
    }
}

#[derive(Clone, Debug)]
pub enum ItemKind {
    Import(Path),
    Directive(P<Directive>),
    DimDecl(P<DimDecl>),
    UnitDecl(P<UnitDecl>),
    OpDecl(P<OpDecl>),
    ConstDecl(P<ConstDecl>),
    FnDecl(P<FnDecl>),
    ModuleDecl(P<ModuleDecl>),
    Expr(P<Expr>),
}

/// An interpreter directive.
pub type Directive = KindNode<DirectiveKind>;

impl Directive {
    pub fn associativity(assoc: OpAssoc) -> Self {
        Self::new(DirectiveKind::Associativity(assoc))
    }

    pub fn binary_coercion(behavior: BinaryCoercion) -> Self {
        Self::new(DirectiveKind::BinaryCoercion(behavior))
    }

    pub fn coerce(behavior: Coercion) -> Self {
        Self::new(DirectiveKind::Coercion(behavior))
    }

    pub fn float_conversion(behavior: FloatConversion) -> Self {
        Self::new(DirectiveKind::FloatConversion(behavior))
    }

    pub fn float_precision(prec: u32) -> Self {
        Self::new(DirectiveKind::FloatPrecision(prec))
    }

    pub fn significant_figures(places: Option<u32>) -> Self {
        Self::new(DirectiveKind::SignificantFigures(places))
    }

    pub fn precedence(prec: isize) -> Self {
        Self::new(DirectiveKind::Precedence(prec))
    }

    pub fn unit_preference(preference: UnitPreference) -> Self {
        Self::new(DirectiveKind::UnitPreference(preference))
    }

    pub fn default_formatter(name: Spanned<Ustr>) -> Self {
        Self::new(DirectiveKind::DefaultFormatter(name))
    }

    pub fn builtin() -> Self {
        Self::new(DirectiveKind::Builtin)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum DirectiveKind {
    Associativity(OpAssoc),
    BinaryCoercion(BinaryCoercion),
    Coercion(Coercion),
    FloatConversion(FloatConversion),
    FloatPrecision(u32),
    SignificantFigures(Option<u32>),
    Precedence(isize),
    UnitPreference(UnitPreference),
    DefaultFormatter(Spanned<Ustr>),
    Builtin,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Coercion {
    /// Automatically coerce.
    Auto,
    /// Do not coerce.
    Never,
}

impl Coercion {
    pub fn is_never(&self) -> bool {
        matches!(self, Coercion::Never)
    }
}

impl Default for Coercion {
    fn default() -> Self {
        Coercion::Auto
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryCoercion {
    /// Coerce the rhs to the type of the lhs.
    Left,
    /// Coerce the lhs to the type of the rhs.
    Right,
    /// If any side is a float, coerce the other side to it. (or left)
    FloatOrLeft,
    /// If any side is an int, coerce the other side to it. (or left)
    IntOrLeft,
}

impl Default for BinaryCoercion {
    fn default() -> Self {
        BinaryCoercion::FloatOrLeft
    }
}

/// Describes how to handle conversion from floats to integers.
#[derive(Clone, Debug, PartialEq)]
pub enum FloatConversion {
    /// Truncate the fractional part.
    Trunc,
    /// Round to the nearest integer.
    Round,
}

impl Default for FloatConversion {
    fn default() -> Self {
        FloatConversion::Trunc
    }
}

/// Controls from where the unit for results of quantity operations should be taken.
#[derive(Clone, Debug, PartialEq)]
pub enum UnitPreference {
    /// Prefer the left side.
    Left,
    /// Prefer the right side.
    Right,
}

/// A dimension declaration.
#[derive(Clone, Debug)]
pub struct DimDecl {
    id: NodeId,
    span: SourceSpan,
    pub name: Ident,
    pub label: Option<Ident>,
    pub dimension: Option<DimExpr>,
}

impl DimDecl {
    pub fn new(name: Ident, dimension: Option<DimExpr>) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            name,
            label: None,
            dimension,
        }
    }

    pub fn with_label(name: Ident, label: Ident, dimension: Option<DimExpr>) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            name,
            label: Some(label),
            dimension,
        }
    }
}

/// Unit declaration.
#[derive(Clone, Debug)]
pub struct UnitDecl {
    id: NodeId,
    span: SourceSpan,
    pub name: Ident,
    pub kind: UnitKind,
    pub suffixes: Vec<Ident>,
    pub dimension: Option<DimExpr>,
    pub value: Option<Either<Expr, UnitImpl>>,
}

impl UnitDecl {
    pub fn base_unit(name: Ident, suffixes: Vec<Ident>, dimension: DimExpr) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            name,
            kind: UnitKind::BaseUnit,
            suffixes,
            dimension: Some(dimension),
            value: None,
        }
    }

    pub fn sub_unit(
        name: Ident,
        suffixes: Vec<Ident>,
        dimension: DimExpr,
        value: Either<Expr, UnitImpl>,
    ) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            name,
            kind: UnitKind::SubUnit,
            suffixes,
            dimension: Some(dimension),
            value: Some(value),
        }
    }

    pub fn expr_unit(name: Ident, suffixes: Vec<Ident>, expr: Either<Expr, UnitImpl>) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            name,
            kind: UnitKind::SubUnit,
            suffixes,
            dimension: None, // Will be computed from expr
            value: Some(expr),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnitKind {
    BaseUnit,
    SubUnit,
}

/// A unit implementation.
#[derive(Clone, Debug)]
pub struct UnitImpl {
    id: NodeId,
    span: SourceSpan,
    pub functions: Vec<FnDecl>,
}

impl UnitImpl {
    pub fn new(functions: Vec<FnDecl>) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            functions,
        }
    }
}

/// An operator declaration.
#[derive(Clone, Debug)]
pub struct OpDecl {
    id: NodeId,
    span: SourceSpan,
    pub name: Operator,
    pub kind: OpKind,
    pub assoc: OpAssoc,
    pub prec: isize,

    pub params: ListNode<Param>,
    pub body: Either<Path, ListNode<Stmt>>,
}

impl OpDecl {
    pub fn new(
        name: Operator,
        kind: OpKind,
        assoc: OpAssoc,
        prec: isize,
        params: ListNode<Param>,
        body: Either<Path, ListNode<Stmt>>,
    ) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            name,
            kind,
            assoc,
            prec,

            params,
            body,
        }
    }
}

/// An operator kind.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum OpKind {
    Prefix,
    Postfix,
    Infix,
}

/// An operator associativity.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum OpAssoc {
    Left,
    Right,
}

/// A constant declaration.
#[derive(Clone, Debug)]
pub struct ConstDecl {
    id: NodeId,
    span: SourceSpan,
    pub name: Ident,
    pub value: Expr,
}

impl ConstDecl {
    pub fn new(name: Ident, value: Expr) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            name,
            value,
        }
    }
}

/// A function declaration.
#[derive(Clone, Debug)]
pub struct FnDecl {
    id: NodeId,
    span: SourceSpan,
    pub name: Ident,
    pub params: ListNode<Param>,
    pub body: ListNode<Stmt>,
    pub ret: Option<Either<DimExpr, Ty>>,
    pub is_builtin_wrapper: bool,
}

impl FnDecl {
    pub fn new(
        name: Ident,
        params: ListNode<Param>,
        body: ListNode<Stmt>,
        ret: Option<Either<DimExpr, Ty>>,
    ) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            name,
            params,
            body,
            ret,
            is_builtin_wrapper: false,
        }
    }
}

/// A module declaration.
#[derive(Clone, Debug)]
pub struct ModuleDecl {
    id: NodeId,
    span: SourceSpan,
    pub name: Ident,
    pub items: Vec<Item>,
}

impl ModuleDecl {
    pub fn new(name: Ident, items: Vec<Item>) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            name,
            items,
        }
    }
}

/// A function argument.
#[derive(Clone, Debug)]
pub struct Param {
    id: NodeId,
    span: SourceSpan,
    pub name: Ident,
    pub anno: Option<Either<DimExpr, Ty>>,
    pub is_variadic: bool,
    pub is_optional: bool,
}

impl Param {
    pub fn new(name: Ident, anno: Option<Either<DimExpr, Ty>>) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            name,
            anno,
            is_variadic: false,
            is_optional: false,
        }
    }

    pub fn new_variadic(name: Ident) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            name,
            anno: None,
            is_variadic: true,
            is_optional: false,
        }
    }

    pub fn optional(name: Ident, anno: Option<Either<DimExpr, Ty>>) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            name,
            anno,
            is_variadic: false,
            is_optional: true,
        }
    }

    pub fn plain(name: Ident) -> Self {
        Self::new(name, None)
    }

    pub fn dim(name: Ident, dim: DimExpr) -> Self {
        Self::new(name, Some(Left(dim)))
    }

    pub fn ty(name: Ident, ty: Ty) -> Self {
        Self::new(name, Some(Right(ty)))
    }
}

/// A dimensional expression.
pub type DimExpr = KindNode<DimExprKind>;

#[derive(Clone, Debug)]
pub enum DimExprKind {
    /// A multiplication operation.
    Mul(P<DimExpr>, P<DimExpr>),
    /// A division operation.
    Div(P<DimExpr>, P<DimExpr>),
    /// An exponentiation operation.
    Pow(P<DimExpr>, P<DimExpr>),
    /// A negation operation.
    Neg(P<DimExpr>),
    /// An identifier.
    Ident(Ident),
    /// A number.
    Number(Number),
    /// A unit constraint (e.g., [rad] means "convert to radians").
    Unit(Ident),
}

impl DimExpr {
    pub fn mul(lhs: DimExpr, rhs: DimExpr) -> Self {
        Self::new(DimExprKind::Mul(lhs.into(), rhs.into()))
    }

    pub fn div(lhs: DimExpr, rhs: DimExpr) -> Self {
        Self::new(DimExprKind::Div(lhs.into(), rhs.into()))
    }

    pub fn pow(lhs: DimExpr, rhs: DimExpr) -> Self {
        Self::new(DimExprKind::Pow(lhs.into(), rhs.into()))
    }

    pub fn neg(expr: DimExpr) -> Self {
        Self::new(DimExprKind::Neg(expr.into()))
    }

    pub fn ident(ident: Ident) -> Self {
        Self::new(DimExprKind::Ident(ident))
    }

    pub fn number(number: Number) -> Self {
        Self::new(DimExprKind::Number(number))
    }

    pub fn unit(ident: Ident) -> Self {
        Self::new(DimExprKind::Unit(ident))
    }
}

impl ToString for DimExprKind {
    fn to_string(&self) -> String {
        match self {
            DimExprKind::Mul(lhs, rhs) => format!("{} * {}", lhs.to_string(), rhs.to_string()),
            DimExprKind::Div(lhs, rhs) => format!("{} / {}", lhs.to_string(), rhs.to_string()),
            DimExprKind::Pow(lhs, rhs) => format!("{} ^ {}", lhs.to_string(), rhs.to_string()),
            DimExprKind::Neg(expr) => format!("-{}", expr.to_string()),
            DimExprKind::Ident(ident) => ident.to_string(),
            DimExprKind::Number(number) => number.to_string(),
            DimExprKind::Unit(ident) => format!("[{}]", ident.to_string()),
        }
    }
}

/// A statement.
pub type Stmt = KindNode<StmtKind>;

#[derive(Clone, Debug)]
pub enum StmtKind {
    Break,
    Continue,
    Expr(P<Expr>),
    Return(P<Expr>),
}

impl Stmt {
    pub fn break_() -> Self {
        Self::new(StmtKind::Break)
    }

    pub fn continue_() -> Self {
        Self::new(StmtKind::Continue)
    }

    pub fn expr(expr: Expr) -> Self {
        Self::new(StmtKind::Expr(expr.into()))
    }

    pub fn return_(expr: Expr) -> Self {
        Self::new(StmtKind::Return(expr.into()))
    }
}

/// An expression.
pub type Expr = KindNode<ExprKind>;

/// A key-value pair within an object literal.
#[derive(Clone, Debug)]
pub struct ObjectField {
    id: NodeId,
    span: SourceSpan,
    pub key: Spanned<String>,
    pub value: Expr,
}

impl ObjectField {
    pub fn new(key: Spanned<String>, value: Expr) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            key,
            value,
        }
    }
}

#[derive(Clone, Debug)]
pub struct IfBranch {
    pub cond: P<Expr>,
    pub body: ListNode<Stmt>,
}

impl IfBranch {
    pub fn new(cond: Expr, body: ListNode<Stmt>) -> Self {
        Self {
            cond: cond.into(),
            body,
        }
    }
}

#[derive(Clone, Debug)]
pub struct If {
    pub branches: Vec<IfBranch>,
    pub else_branch: Option<ListNode<Stmt>>,
}

impl If {
    pub fn new(branches: Vec<IfBranch>, else_branch: Option<ListNode<Stmt>>) -> Self {
        Self {
            branches,
            else_branch,
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    /// An assignment expresion.
    Assign(P<BindPat>, P<Expr>),
    /// An assignment to an index (e.g., list/object indexing).
    IndexAssign(P<Expr>, P<Expr>, P<Expr>),
    /// A slice expression (e.g., list[start:stop]).
    Slice(P<Expr>, Option<P<Expr>>, Option<P<Expr>>),
    /// An infix operation.
    InfixOp(Operator, P<Expr>, P<Expr>),
    /// A prefix operation.
    PrefixOp(Operator, P<Expr>),
    /// A postfix operation.
    PostfixOp(P<Expr>, Operator),
    /// A unit cast expression.
    UnitCast(P<Expr>, Unit),
    /// An if expression.
    If(If),
    // A for-range expression.
    ForRange(P<BindPat>, P<Expr>, ListNode<Stmt>),
    /// A function call expression.
    FnCall(Path, ListNode<Expr>),
    /// A splat expression (...)
    Splat(P<Expr>),

    /// A list.
    List(ListNode<Expr>),
    /// A tuple.
    Tuple(ListNode<Expr>),
    /// An object literal.
    Object(ListNode<ObjectField>),
    /// Empty unit expression ().
    Empty,
    /// An identifier path.
    Path(Path),
    /// An identifier.
    Ident(Ident),
    /// A number.
    Number(Number),
    /// A string
    String(String),
    /// A boolean.
    Boolean(bool),
    /// A unit.
    Unit(Unit),
    /// A type.
    Type(Ty),
}

impl Expr {
    pub fn assign(bind: BindPat, expr: Expr) -> Self {
        Self::new(ExprKind::Assign(bind.into(), expr.into()))
    }

    pub fn infix_op(op: Operator, lhs: Expr, rhs: Expr) -> Self {
        Self::new(ExprKind::InfixOp(op, lhs.into(), rhs.into()))
    }

    pub fn index_assign(container: Expr, index: Expr, value: Expr) -> Self {
        Self::new(ExprKind::IndexAssign(
            container.into(),
            index.into(),
            value.into(),
        ))
    }

    pub fn slice(container: Expr, start: Option<Expr>, stop: Option<Expr>) -> Self {
        Self::new(ExprKind::Slice(
            container.into(),
            start.map(Box::new),
            stop.map(Box::new),
        ))
    }

    pub fn prefix_op(op: Operator, expr: Expr) -> Self {
        Self::new(ExprKind::PrefixOp(op, expr.into()))
    }

    pub fn postfix_op(expr: Expr, op: Operator) -> Self {
        Self::new(ExprKind::PostfixOp(expr.into(), op))
    }

    pub fn unit_cast(expr: Expr, unit: Unit) -> Self {
        Self::new(ExprKind::UnitCast(expr.into(), unit))
    }

    pub fn if_expr(if_expr: If) -> Self {
        Self::new(ExprKind::If(if_expr))
    }

    pub fn for_range(bind: BindPat, expr: Expr, body: ListNode<Stmt>) -> Self {
        Self::new(ExprKind::ForRange(bind.into(), expr.into(), body))
    }

    pub fn fn_call(path: Path, args: ListNode<Expr>) -> Self {
        Self::new(ExprKind::FnCall(path, args))
    }

    pub fn splat(expr: Expr) -> Self {
        Self::new(ExprKind::Splat(expr.into()))
    }

    pub fn list(items: ListNode<Expr>) -> Self {
        Self::new(ExprKind::List(items))
    }

    pub fn tuple(items: ListNode<Expr>) -> Self {
        Self::new(ExprKind::Tuple(items))
    }

    pub fn object(items: ListNode<ObjectField>) -> Self {
        Self::new(ExprKind::Object(items))
    }

    pub fn empty() -> Self {
        Self::new(ExprKind::Empty)
    }

    pub fn path(path: Path) -> Self {
        Self::new(ExprKind::Path(path))
    }

    pub fn ident(ident: Ident) -> Self {
        Self::new(ExprKind::Ident(ident))
    }

    pub fn number(number: Number) -> Self {
        Self::new(ExprKind::Number(number))
    }

    pub fn string(string: String) -> Self {
        Self::new(ExprKind::String(string))
    }

    pub fn boolean(value: bool) -> Self {
        Self::new(ExprKind::Boolean(value))
    }

    pub fn unit(unit: Unit) -> Self {
        Self::new(ExprKind::Unit(unit))
    }

    pub fn ty(ty: Ty) -> Self {
        Self::new(ExprKind::Type(ty))
    }

    pub fn into_bind_pat(self) -> Result<BindPat, Spanned<String>> {
        let span = self.span();
        match self.kind {
            ExprKind::Tuple(items) => {
                let items = items.to_vec();
                let items = items
                    .into_iter()
                    .map(|expr| expr.into_bind_pat())
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(BindPat::tuple(ListNode::new(items)).with_span(span))
            }
            ExprKind::Ident(ident) => {
                if ident.raw == "_" {
                    Ok(BindPat::ignored().with_span(span))
                } else {
                    Ok(BindPat::var(ident).with_span(span))
                }
            }
            _ => Err(span.into_spanned("invalid binding pattern".to_string())),
        }
    }
}

/// A binding pattern.
pub type BindPat = KindNode<BindPatKind>;

#[derive(Clone, Debug)]
pub enum BindPatKind {
    /// An ignored binding.
    Ignored,
    /// A variable pattern.
    Var(Ident),
    /// A tuple pattern.
    Tuple(ListNode<BindPat>),
}

impl BindPat {
    pub fn ignored() -> Self {
        Self::new(BindPatKind::Ignored)
    }

    pub fn var(ident: Ident) -> Self {
        Self::new(BindPatKind::Var(ident))
    }

    pub fn tuple(items: ListNode<BindPat>) -> Self {
        Self::new(BindPatKind::Tuple(items))
    }
}

/// A type.
pub type Ty = KindNode<TyKind>;

#[derive(Clone, Debug)]
pub enum TyKind {
    Any,
    Bool,
    Int,
    Float,
    Str,
    Num,
    Function,
    Io,
    Handle(Ustr),
    Unit,
    Type,
    Object,
    List,
    Tuple(ListNode<Ty>),
    Ref(Box<Ty>),
}

impl Ty {
    pub fn any() -> Self {
        Self::new(TyKind::Any)
    }

    pub fn bool() -> Self {
        Self::new(TyKind::Bool)
    }

    pub fn int() -> Self {
        Self::new(TyKind::Int)
    }

    pub fn float() -> Self {
        Self::new(TyKind::Float)
    }

    pub fn num() -> Self {
        Self::new(TyKind::Num)
    }

    pub fn str() -> Self {
        Self::new(TyKind::Str)
    }

    pub fn function() -> Self {
        Self::new(TyKind::Function)
    }

    pub fn io() -> Self {
        Self::new(TyKind::Io)
    }

    pub fn handle(name: Ustr) -> Self {
        Self::new(TyKind::Handle(name))
    }

    pub fn list() -> Self {
        Self::new(TyKind::List)
    }

    pub fn unit() -> Self {
        Self::new(TyKind::Unit)
    }

    pub fn ty() -> Self {
        Self::new(TyKind::Type)
    }

    pub fn object() -> Self {
        Self::new(TyKind::Object)
    }

    pub fn tuple(items: ListNode<Ty>) -> Self {
        Self::new(TyKind::Tuple(items))
    }

    pub fn ref_(ty: Ty) -> Self {
        Self::new(TyKind::Ref(Box::new(ty)))
    }
}

/// A path is a multi-part identifier specifiying an item in a module.
#[derive(Debug, Clone)]
pub struct Path {
    id: NodeId,
    span: SourceSpan,
    pub parts: Vec<Ident>,
}

impl Path {
    pub fn new(parts: Vec<Ident>) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            parts,
        }
    }

    pub fn name_part(&self) -> Spanned<Ustr> {
        self.parts.last().unwrap().as_spanned_ustr()
    }

    pub fn module_parts(&self) -> SmallVec<[Spanned<Ustr>; 4]> {
        if self.parts.len() == 1 {
            return SmallVec::new();
        }

        self.parts[..self.parts.len() - 1]
            .iter()
            .map(|ident| ident.as_spanned_ustr())
            .collect()
    }

    pub fn path_parts(&self) -> SmallVec<[Spanned<Ustr>; 4]> {
        self.parts
            .iter()
            .map(|ident| ident.as_spanned_ustr())
            .collect()
    }
}

/// An identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Ident {
    id: NodeId,
    span: SourceSpan,
    pub raw: Ustr,
}

impl Ident {
    pub fn new(raw: Ustr) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            raw,
        }
    }

    pub fn as_spanned_ustr(&self) -> Spanned<Ustr> {
        Spanned::new(self.raw, self.span)
    }

    pub fn as_spanned_string(&self) -> Spanned<String> {
        Spanned::new(self.raw.to_owned(), self.span)
    }
}

impl ToString for Ident {
    fn to_string(&self) -> String {
        self.raw.to_string()
    }
}

/// An operator.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Operator {
    id: NodeId,
    span: SourceSpan,
    pub raw: Ustr,
    pub kind: OpKind,
}

impl Operator {
    pub fn new(raw: Ustr, kind: OpKind) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            raw,
            kind,
        }
    }

    pub fn as_spanned_ustr(&self) -> Spanned<Ustr> {
        Spanned::new(self.raw, self.span)
    }

    pub fn as_spanned_string(&self) -> Spanned<String> {
        Spanned::new(self.raw.to_owned(), self.span)
    }
}

/// A unit.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unit {
    id: NodeId,
    span: SourceSpan,
    pub name: Ustr,
}

impl Unit {
    pub fn new(name: Ustr) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            name,
        }
    }

    pub fn into_raw_spanned(self) -> Spanned<Ustr> {
        Spanned::new(self.name, self.span)
    }
}

impl From<Ident> for Unit {
    fn from(ident: Ident) -> Self {
        Self::new(ident.raw).with_span(ident.span)
    }
}

/// A number.
pub type Number = KindNode<NumberKind>;

impl Number {
    pub fn integer(value: rug::Integer) -> Self {
        Self::new(NumberKind::Integer(value))
    }

    pub fn float(value: rug::Float) -> Self {
        Self::new(NumberKind::Float(value))
    }

    pub fn from_bool(value: bool) -> Self {
        Self::integer(rug::Integer::from(value as i32))
    }
}

/// A number kind.
#[derive(Clone, Debug, PartialEq)]
pub enum NumberKind {
    Integer(rug::Integer),
    Float(rug::Float),
}

impl ToString for NumberKind {
    fn to_string(&self) -> String {
        match self {
            NumberKind::Integer(value) => value.to_string(),
            NumberKind::Float(value) => value.to_string(),
        }
    }
}

/// A string literal.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringLit {
    id: NodeId,
    span: SourceSpan,
    pub value: String,
}

impl StringLit {
    pub fn new(value: String) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            value,
        }
    }

    pub fn as_spanned_string(&self) -> Spanned<String> {
        Spanned::new(self.value.clone(), self.span)
    }
}

//
// MARK: Automatic Impls
//

impl_identifiable!(DimDecl);
impl_spannable!(DimDecl);
impl_identifiable!(UnitDecl);
impl_spannable!(UnitDecl);
impl_identifiable!(UnitImpl);
impl_spannable!(UnitImpl);
impl_identifiable!(OpDecl);
impl_spannable!(OpDecl);
impl_identifiable!(ConstDecl);
impl_spannable!(ConstDecl);
impl_identifiable!(FnDecl);
impl_spannable!(FnDecl);
impl_identifiable!(ModuleDecl);
impl_spannable!(ModuleDecl);
impl_identifiable!(Param);
impl_spannable!(Param);
impl_identifiable!(Operator);
impl_spannable!(Operator);
impl_identifiable!(Unit);
impl_spannable!(Unit);
impl_identifiable!(Path);
impl_spannable!(Path);
impl_identifiable!(Ident);
impl_spannable!(Ident);
impl_identifiable!(StringLit);
impl_spannable!(StringLit);
impl_identifiable!(ObjectField);
impl_spannable!(ObjectField);
