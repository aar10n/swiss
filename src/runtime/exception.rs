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
    pub primary_span: Option<SourceSpan>,
}

impl Exception {
    pub fn new(kind: &'static str, message: String) -> Self {
        Self {
            kind,
            message,
            extras: vec![],
            backtrace: vec![],
            primary_span: None,
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

    pub fn with_primary_span(mut self, span: SourceSpan) -> Self {
        self.primary_span = Some(span);
        self
    }
}

impl std::fmt::Display for Exception {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.kind, self.message)
    }
}

impl IntoErrorCtx<Context> for Exception {
    fn into_error_ctx(self, ctx: &Context) -> Error {
        // Prefer an explicit primary span, otherwise fall back to the first extra, otherwise invalid.
        let primary_span = self.primary_span.unwrap_or_else(|| {
            self.extras
                .first()
                .map(|e| e.span)
                .unwrap_or_else(SourceSpan::default)
        });

        let mut err = Error::new(format!("Exception: {}", self.message), primary_span);

        // Attach any additional context spans.
        for extra in self.extras {
            err = err.with_extra(extra.raw, extra.span);
        }

        // Append backtrace frames.
        for frame in self.backtrace {
            err = err.with_extra(
                format!("  in call to '{}'", frame.function.raw),
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

impl StackFrame {
    pub fn new(function: Spanned<Ustr>, call_site: SourceSpan) -> Self {
        Self {
            function,
            call_site,
        }
    }
}
