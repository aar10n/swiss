use super::value::Number;

use crate::source::Spanned;

use std::cmp::Ordering;
use std::collections::HashMap;
use std::ops::Index;
use ustr::Ustr;

pub use super::value::DimExpr;

/// A registered dimension.
#[derive(Clone, Debug)]
pub struct Dimension {
    pub name: Spanned<Ustr>,
    pub expr: DimExpr,
}

impl Dimension {
    pub fn new(name: Spanned<Ustr>, expr: DimExpr) -> Self {
        Self { name, expr }
    }
}

/// A table that tracks dimensions.
#[derive(Clone, Debug)]
pub struct DimensionTable {
    dimensions: HashMap<Ustr, Dimension>,
    dim_exprs: HashMap<String, Ustr>,
}

impl DimensionTable {
    pub fn new() -> Self {
        Self {
            dimensions: HashMap::new(),
            dim_exprs: HashMap::new(),
        }
    }

    pub fn insert(&mut self, dim: Dimension) {
        self.dim_exprs.insert(dim.expr.to_string(), dim.name.raw);
        self.dimensions.insert(dim.name.raw, dim);
    }

    pub fn get(&self, name: Ustr) -> Option<&Dimension> {
        self.dimensions.get(&name)
    }

    pub fn resolve_expr(&self, expr: &DimExpr) -> Option<&Dimension> {
        self.dim_exprs
            .get(&expr.to_string())
            .and_then(|name| self.get(*name))
    }
}

impl Index<Ustr> for DimensionTable {
    type Output = Dimension;

    fn index(&self, name: Ustr) -> &Self::Output {
        self.get(name).expect("dimension not found")
    }
}
