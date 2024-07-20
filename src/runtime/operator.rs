use crate::ast::{Expr, ListNode, Path};
pub use crate::ast::{OpAssoc, OpKind};
use crate::source::{SourceSpan, Spanned};

use either::{Either, Left, Right};
use std::collections::{HashMap, HashSet};
use std::ops::Index;
use ustr::Ustr;

/// A registered operator.
#[derive(Clone, Debug)]
pub struct Operator {
    pub name: Spanned<Ustr>,
    pub kind: OpKind,
    pub assoc: OpAssoc,
    pub prec: isize,
    pub func: Either<Path, ListNode<Expr>>,
}

impl Operator {
    pub fn new(
        name: Spanned<Ustr>,
        kind: OpKind,
        assoc: OpAssoc,
        prec: isize,
        func: Either<Path, ListNode<Expr>>,
    ) -> Self {
        Self {
            name,
            kind,
            assoc,
            prec,
            func,
        }
    }

    pub fn is_right(&self) -> bool {
        self.assoc == OpAssoc::Right
    }
}

/// A table that tracks operators.
#[derive(Clone, Debug)]
pub struct OperatorTable {
    op_set: HashSet<Ustr>,
    op_map: HashMap<(OpKind, Ustr), Operator>,
    op_starters: HashMap<OpKind, HashSet<char>>,
}

impl OperatorTable {
    pub fn new() -> Self {
        Self {
            op_set: HashSet::new(),
            op_map: HashMap::new(),
            op_starters: HashMap::new(),
        }
    }

    pub fn insert(&mut self, op: Operator) {
        let kind = op.kind;
        let name = op.name.clone();

        self.op_set.insert(name.raw);
        self.op_map.insert((op.kind, name.raw), op);
        self.op_starters
            .entry(kind)
            .or_insert_with(HashSet::new)
            .insert(name.as_str().chars().next().unwrap());
    }

    pub fn get(&self, kind: OpKind, name: Ustr) -> Option<&Operator> {
        self.op_map.get(&(kind, name))
    }

    pub fn get_by_name(&self, name: Ustr) -> Vec<&Operator> {
        self.op_map
            .iter()
            .filter(|(_, op)| op.name.raw == name)
            .map(|(_, op)| op)
            .collect()
    }

    pub fn contains(&self, name: Ustr) -> bool {
        self.op_set.contains(&name)
    }

    pub fn does_start(&self, kind: OpKind, s: Ustr) -> bool {
        self.op_starters.get(&kind).map_or(false, |set| {
            set.contains(&s.as_str().chars().next().unwrap())
        })
    }
}

impl Index<(OpKind, Ustr)> for OperatorTable {
    type Output = Operator;

    fn index(&self, index: (OpKind, Ustr)) -> &Self::Output {
        &self.op_map[&index]
    }
}
