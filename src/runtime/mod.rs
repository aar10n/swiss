pub mod builtin;
mod context;
mod dimension;
mod exception;
mod module;
mod name;
mod operator;
mod path;
mod scope;
mod unit;
mod value;

use crate::diag::{Error, IntoError};
use crate::source::{SourceId, SourcePos, SourceSpan, Spanned};
pub use context::*;
pub use dimension::*;
pub use exception::*;
pub use module::*;
pub use name::*;
pub use operator::*;
pub use unit::*;
pub use value::*;

/// A declaration name error.
#[derive(Debug)]
pub struct DeclError {
    pub kind: &'static str,
    pub name: Spanned<String>,
    pub prev: SourceSpan,
}

impl DeclError {
    pub fn new(kind: &'static str, name: Spanned<String>, prev: SourceSpan) -> Self {
        Self { kind, name, prev }
    }
}

impl IntoError for DeclError {
    fn into_error(self) -> Error {
        let msg = format!(
            "conflicting {} declaration for '{}'",
            self.kind,
            self.name.as_str()
        );

        Error::new(msg, self.name.span).with_extra(format!("previous declaration here"), self.prev)
    }
}

/// A directive error.
#[derive(Debug)]
pub struct DirectiveError {
    pub problem: &'static str,
    pub directive: String,
    pub span: SourceSpan,
}

impl DirectiveError {
    pub fn new(problem: &'static str, directive: String, span: SourceSpan) -> Self {
        Self {
            problem,
            directive,
            span,
        }
    }
}

impl IntoError for DirectiveError {
    fn into_error(self) -> Error {
        let msg = format!("{} '{}'", self.problem, self.directive);
        Error::new(msg, self.span)
    }
}

/// A name error.
#[derive(Debug)]
pub struct NameError {
    pub problem: &'static str,
    pub name: Spanned<String>,
    pub extra: Option<String>,
}

impl NameError {
    pub fn new(problem: &'static str, name: Spanned<String>) -> Self {
        Self {
            problem,
            name,
            extra: None,
        }
    }

    pub fn with_extra(mut self, extra: String) -> Self {
        self.extra = Some(extra);
        self
    }
}

impl IntoError for NameError {
    fn into_error(self) -> Error {
        let msg = format!("{} '{}'", self.problem, self.name.as_str());
        Error::new(msg, self.name.span)
    }
}

/// A type error.
#[derive(Debug)]
pub struct TypeError {
    pub expected: String,
    pub found: Spanned<String>,
    pub context: Option<Spanned<String>>,
}

impl TypeError {
    pub fn new(expected: String, found: Spanned<String>) -> Self {
        Self {
            expected,
            found,
            context: None,
        }
    }

    pub fn with_context(mut self, context: Spanned<String>) -> Self {
        self.context = Some(context);
        self
    }
}

impl IntoError for TypeError {
    fn into_error(self) -> Error {
        let msg = format!(
            "expected {}, found '{}'",
            self.expected,
            self.found.as_str()
        );
        let mut err = Error::new(msg, self.found.span);
        if let Some(context) = self.context {
            err = err.with_extra(format!("in context '{}'", context.as_str()), context.span);
        }
        err
    }
}

/// A value error.
#[derive(Debug)]
pub struct ValueError {
    pub problem: &'static str,
    pub value: Spanned<String>,
    pub extra: Option<String>,
}

impl ValueError {
    pub fn new(problem: &'static str, value: Spanned<String>) -> Self {
        Self {
            problem,
            value,
            extra: None,
        }
    }

    pub fn with_extra(mut self, extra: String) -> Self {
        self.extra = Some(extra);
        self
    }
}

impl IntoError for ValueError {
    fn into_error(self) -> Error {
        let msg = format!("{}", self.problem);
        let mut err = Error::new(msg, self.value.span);
        if let Some(extra) = self.extra {
            err = err.with_extra(extra, SourceSpan::INVALID);
        }
        err
    }
}
