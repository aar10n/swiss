mod file;
mod map;
#[macro_use]
mod span;

pub use file::*;
pub use map::*;
pub use span::*;

pub use crate::id::{source_id, SourceId};

use std::fmt::Display;
use std::ops::{Add, Sub};

/// A position in a source file.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SourcePos {
    pub source_id: SourceId,
    pub offset: usize,
}

impl SourcePos {
    pub fn new(source_id: SourceId, offset: usize) -> Self {
        Self { source_id, offset }
    }

    pub fn as_span(self) -> SourceSpan {
        SourceSpan::new(self.source_id, self.offset, self.offset + 1)
    }
}

impl Add<usize> for SourcePos {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        Self {
            source_id: self.source_id,
            offset: self.offset + rhs,
        }
    }
}

impl Sub<usize> for SourcePos {
    type Output = Self;

    fn sub(self, rhs: usize) -> Self::Output {
        Self {
            source_id: self.source_id,
            offset: self.offset - rhs,
        }
    }
}

/// A line/column location in a source file.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SourceLoc {
    pub source_id: SourceId,
    pub line: u32,
    pub column: u32,
}

impl SourceLoc {
    pub fn new(source_id: SourceId, line: u32, column: u32) -> Self {
        Self {
            source_id,
            line,
            column,
        }
    }
}

impl Display for SourceLoc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// A trait for objects that own a source map.
pub trait SourceProvider {
    fn sources(&self) -> &SourceMap;
}
