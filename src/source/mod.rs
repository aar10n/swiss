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
use std::path::{Path, PathBuf};
use std::{env, fs};

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

pub fn swisspath_dirs() -> Vec<PathBuf> {
    let cwd = env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    if let Some(raw) = env::var_os("SWISSPATH") {
        if !raw.is_empty() {
            return parse_swisspath(&raw.to_string_lossy(), &cwd);
        }
    }

    // Development builds default to the repo root; release builds can embed a
    // default SWISSPATH at compile time via SWISS_INSTALL_SWISSPATH.
    let default_raw = if cfg!(debug_assertions) {
        env!("CARGO_MANIFEST_DIR").to_owned() + ":" + env!("CARGO_MANIFEST_DIR") + "/std"
    } else {
        option_env!("SWISS_INSTALL_SWISSPATH")
            .unwrap_or("")
            .to_owned()
    };

    if default_raw.is_empty() {
        vec![normalize_path(&cwd)]
    } else {
        parse_swisspath(&default_raw, &cwd)
    }
}

fn parse_swisspath(raw: &str, cwd: &Path) -> Vec<PathBuf> {
    raw.split(':')
        .filter(|part| !part.is_empty())
        .map(|part| {
            let mut path = PathBuf::from(part);
            if !path.is_absolute() {
                path = cwd.join(path);
            }
            normalize_path(&path)
        })
        .collect()
}

pub fn swisspath_best_base(path: &Path) -> Option<PathBuf> {
    let file_path = normalize_path(path);
    let mut best: Option<(usize, PathBuf)> = None;
    for base in swisspath_dirs() {
        if file_path.starts_with(&base) {
            let len = base.to_string_lossy().len();
            match &best {
                Some((best_len, _)) if *best_len >= len => {}
                _ => best = Some((len, base)),
            }
        }
    }
    best.map(|(_, base)| base)
}

pub fn normalize_path(path: &Path) -> PathBuf {
    fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf())
}
