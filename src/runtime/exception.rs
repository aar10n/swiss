use super::Context;
use crate::diag::{Error, IntoError, IntoErrorCtx};
use crate::source::{SourceId, SourcePos, SourceSpan, Spanned};

use ustr::Ustr;

/// A runtime exception.
#[derive(Clone, Debug)]
pub struct Exception {
    pub kind: &'static str,
    pub message: String,
    pub extras: Vec<Spanned<String>>,
    pub backtrace: Vec<StackFrame>,
}

impl Exception {
    pub fn new(kind: &'static str, message: String) -> Self {
        Self {
            kind,
            message,
            extras: vec![],
            backtrace: vec![],
        }
    }

    pub fn with_extra(mut self, message: Spanned<String>) -> Self {
        self.extras.push(message);
        self
    }

    pub fn with_backtrace(mut self, backtrace: Vec<StackFrame>) -> Self {
        self.backtrace = backtrace;
        self
    }
}

impl IntoErrorCtx<Context> for Exception {
    fn into_error_ctx(self, ctx: &Context) -> Error {
        let mut err = Error::new(self.message, SourceSpan::default());
        for frame in self.backtrace {
            err = err.with_extra(
                format!("in call to '{}'", frame.function.raw),
                frame.call_site,
            );
        }
        err
    }
}

#[derive(Clone, Debug)]
pub struct StackFrame {
    pub function: Spanned<Ustr>,
    pub call_site: SourceSpan,
}
