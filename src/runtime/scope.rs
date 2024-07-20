use super::dimension::{Dimension, DimensionTable};

use super::name::{Constant, Function, Name, NameResult, NameTable, Param};
use super::operator::{OpAssoc, OpKind, Operator, OperatorTable};
use super::unit::{Unit, UnitKind, UnitTable};
use super::{DeclError, NameError, Value};

use crate::id::ModuleId;
use crate::source::Spanned;

use smallvec::{smallvec, SmallVec};
use std::collections::HashMap;
use ustr::Ustr;

// MARK: Scope trait
pub trait Scope {
    // fn resolve(path: impl PathRef) ->
    // fn resolve_function(&self, ) -> Result<&Function, NameError>;
}

// MARK: ModuleScope struct

pub struct ModuleScope {
    pub id: ModuleId,
    pub names: NameTable,
    pub dimensions: DimensionTable,
    pub operators: OperatorTable,
    pub units: UnitTable,
}

// MARK: LocalScope struct

pub struct LocalScope<'module> {
    pub vars: HashMap<Spanned<Ustr>, Value>,
    pub parent: &'module ModuleScope,
}
