use super::{node_id, NodeId};
pub use crate::impl_spannable;
pub use crate::source::{SourcePos, SourceSpan};

use either::{Either, Left, Right};
use std::hash::Hash;
use std::ops::{Deref, DerefMut};
use ustr::Ustr;

/// An AST node that wraps an inner 'kind' value.
#[derive(Clone, Debug)]
pub struct KindNode<T> {
    id: NodeId,
    span: SourceSpan,
    pub kind: T,
}

impl<T> KindNode<T> {
    pub fn new(kind: T) -> Self {
        Self {
            id: node_id::next(),
            kind,
            span: SourceSpan::default(),
        }
    }
}

impl<T: ToString> ToString for KindNode<T> {
    fn to_string(&self) -> String {
        self.kind.to_string()
    }
}

impl<T: Hash> Hash for KindNode<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
    }
}

impl<T> Spannable for KindNode<T> {
    fn with_span(self, span: SourceSpan) -> Self {
        Self { span, ..self }
    }

    fn span(&self) -> SourceSpan {
        self.span
    }
}

impl<T> Identifiable for KindNode<T> {
    fn id(&self) -> NodeId {
        self.id
    }
}

impl<T> Deref for KindNode<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

impl<T> DerefMut for KindNode<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.kind
    }
}

/// A list of AST Nodes.
#[derive(Clone, Debug, PartialEq)]
pub struct ListNode<T> {
    id: NodeId,
    span: SourceSpan,
    pub items: Vec<T>,
    pub ldelim: Option<(&'static str, SourcePos)>,
    pub rdelim: Option<(&'static str, SourcePos)>,
}

impl<T> ListNode<T> {
    pub fn new(items: Vec<T>) -> Self {
        Self {
            id: node_id::next(),
            span: SourceSpan::default(),
            items,
            ldelim: None,
            rdelim: None,
        }
    }

    pub fn with_delims(
        self,
        ldelim: (&'static str, SourcePos),
        rdelim: (&'static str, SourcePos),
    ) -> Self {
        Self {
            ldelim: Some(ldelim),
            rdelim: Some(rdelim),
            ..self
        }
    }

    pub fn start_pos(&self) -> SourcePos {
        self.ldelim
            .map(|(_, pos)| pos)
            .unwrap_or_else(|| self.span.start_pos())
    }

    pub fn end_pos(&self) -> SourcePos {
        self.rdelim
            .map(|(_, pos)| pos)
            .unwrap_or_else(|| self.span.end_pos())
    }
}

impl<T: Spannable> From<Vec<T>> for ListNode<T> {
    fn from(items: Vec<T>) -> Self {
        let mut span = items
            .first()
            .map(|first| first.span())
            .unwrap_or(SourceSpan::default());
        if let Some(last) = items.last() {
            span.end = last.span().end;
        }

        Self {
            id: node_id::next(),
            span,
            items,
            ldelim: Some(("", span.start_pos())),
            rdelim: Some(("", span.end_pos())),
        }
    }
}

impl<T> Spannable for ListNode<T> {
    fn with_span(self, span: SourceSpan) -> Self {
        Self { span, ..self }
    }

    fn span(&self) -> SourceSpan {
        self.span
    }
}

impl<T> Identifiable for ListNode<T> {
    fn id(&self) -> NodeId {
        self.id
    }
}

impl<T> Deref for ListNode<T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.items
    }
}

impl<T> DerefMut for ListNode<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.items
    }
}

// MARK: Spannable

/// A trait for spannable nodes.   
pub trait Spannable: Sized {
    fn with_span(self, span: SourceSpan) -> Self;
    fn span(&self) -> SourceSpan;
}

#[macro_export]
macro_rules! impl_spannable {
    ($ty:ty) => {
        impl $crate::ast::Spannable for $ty {
            fn with_span(self, span: $crate::source::SourceSpan) -> Self {
                Self { span, ..self }
            }

            fn span(&self) -> $crate::source::SourceSpan {
                self.span
            }
        }
    };
}

impl<L, R> Spannable for Either<L, R>
where
    L: Spannable,
    R: Spannable,
{
    fn with_span(self, span: SourceSpan) -> Self {
        match self {
            Left(left) => Left(left.with_span(span)),
            Right(right) => Right(right.with_span(span)),
        }
    }

    fn span(&self) -> SourceSpan {
        match self {
            Left(left) => left.span(),
            Right(right) => right.span(),
        }
    }
}

// MARK: Identifiable

/// A trait for identifiable AST nodes.   
pub trait Identifiable {
    fn id(&self) -> NodeId;
}

#[macro_export]
macro_rules! impl_identifiable {
    ($ty:ty) => {
        impl $crate::ast::Identifiable for $ty {
            fn id(&self) -> $crate::ast::NodeId {
                self.id
            }
        }
    };
}

impl<L, R> Identifiable for Either<L, R>
where
    L: Identifiable,
    R: Identifiable,
{
    fn id(&self) -> NodeId {
        match self {
            Left(left) => left.id(),
            Right(right) => right.id(),
        }
    }
}
