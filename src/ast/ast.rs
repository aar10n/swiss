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

    pub fn fn_decl(decl: FnDecl) -> Self {
        Self::new(ItemKind::FnDecl(decl.into()))
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
    FnDecl(P<FnDecl>),
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

    pub fn precedence(prec: isize) -> Self {
        Self::new(DirectiveKind::Precedence(prec))
    }

    pub fn precision(prec: u32) -> Self {
        Self::new(DirectiveKind::Precision(prec))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum DirectiveKind {
    Associativity(OpAssoc),
    BinaryCoercion(BinaryCoercion),
    Coercion(Coercion),
    FloatConversion(FloatConversion),
    Precedence(isize),
    Precision(u32),
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

/// A dimension declaration.
#[derive(Clone, Debug)]
pub struct DimDecl {
    id: NodeId,
    span: SourceSpan,
    pub name: Ident,
    pub dimension: Option<DimExpr>,
}

impl DimDecl {
    pub fn new(name: Ident, dimension: Option<DimExpr>) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            name,
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
    pub dimension: DimExpr,
    pub scalar: Option<Expr>,
}

impl UnitDecl {
    pub fn base_unit(name: Ident, suffixes: Vec<Ident>, dimension: DimExpr) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            name,
            kind: UnitKind::BaseUnit,
            suffixes,
            dimension,
            scalar: None,
        }
    }

    pub fn sub_unit(name: Ident, suffixes: Vec<Ident>, dimension: DimExpr, scalar: Expr) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            name,
            kind: UnitKind::SubUnit,
            suffixes,
            dimension,
            scalar: Some(scalar),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnitKind {
    BaseUnit,
    SubUnit,
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
    pub body: Either<Path, ListNode<Expr>>,
}

impl OpDecl {
    pub fn new(
        name: Operator,
        kind: OpKind,
        assoc: OpAssoc,
        prec: isize,
        params: ListNode<Param>,
        body: Either<Path, ListNode<Expr>>,
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

/// A function declaration.
#[derive(Clone, Debug)]
pub struct FnDecl {
    id: NodeId,
    span: SourceSpan,
    pub name: Ident,
    pub params: ListNode<Param>,
    pub body: ListNode<Expr>,
    pub ret: Option<Either<DimExpr, Ty>>,
}

impl FnDecl {
    pub fn new(
        name: Ident,
        params: ListNode<Param>,
        body: ListNode<Expr>,
        ret: Option<Either<DimExpr, Ty>>,
    ) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            name,
            params,
            body,
            ret,
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
}

impl Param {
    pub fn new(name: Ident, anno: Option<Either<DimExpr, Ty>>) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            name,
            anno,
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
}

/// A dimensional expression kind.
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
        }
    }
}

/// An expression.
pub type Expr = KindNode<ExprKind>;

impl Expr {
    pub fn infix_op(op: Operator, lhs: Expr, rhs: Expr) -> Self {
        Self::new(ExprKind::InfixOp(op, lhs.into(), rhs.into()))
    }

    pub fn prefix_op(op: Operator, expr: Expr) -> Self {
        Self::new(ExprKind::PrefixOp(op, expr.into()))
    }

    pub fn postfix_op(expr: Expr, op: Operator) -> Self {
        Self::new(ExprKind::PostfixOp(expr.into(), op))
    }

    pub fn unit(expr: Expr, unit: Unit) -> Self {
        Self::new(ExprKind::Unit(expr.into(), unit))
    }

    pub fn if_else(cond: Expr, then: ListNode<Expr>, else_: ListNode<Expr>) -> Self {
        Self::new(ExprKind::IfElse(cond.into(), then, else_))
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
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    /// An infix operation.
    InfixOp(Operator, P<Expr>, P<Expr>),
    /// A prefix operation.
    PrefixOp(Operator, P<Expr>),
    /// A postfix operation.
    PostfixOp(P<Expr>, Operator),
    /// A unit expression.
    Unit(P<Expr>, Unit),
    // An if-else expression.
    IfElse(P<Expr>, ListNode<Expr>, ListNode<Expr>),
    /// A function call expression.
    FnCall(Path, ListNode<Expr>),
    /// An identifier path.
    Path(Path),
    /// An identifier.
    Ident(Ident),
    /// A number.
    Number(Number),
}

/// A type.
pub type Ty = KindNode<TyKind>;

impl Ty {
    pub fn any() -> Self {
        Self::new(TyKind::Any)
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
}

#[derive(Clone, Debug)]
pub enum TyKind {
    Any,
    Int,
    Float,
    Num,
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
impl_identifiable!(OpDecl);
impl_spannable!(OpDecl);
impl_identifiable!(FnDecl);
impl_spannable!(FnDecl);
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
