use super::printer::{PrettyPrinter, Printer};
use crate::source::{SourceProvider, SourceSpan};

use std::error;
use std::io;

/// An error diagnostic.
#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
    pub span: SourceSpan,
    pub extras: Vec<(String, SourceSpan)>,
}

impl Error {
    pub fn new(message: String, span: SourceSpan) -> Self {
        Self {
            message,
            span,
            extras: vec![],
        }
    }

    pub fn with_span(mut self, span: SourceSpan) -> Self {
        self.span = span;
        self
    }

    pub fn with_extra(mut self, message: String, span: SourceSpan) -> Self {
        self.extras.push((message, span));
        self
    }

    pub fn write<Out, Print, Ctx>(
        &self,
        out: &mut Out,
        printer: &Print,
        ctx: &Ctx,
    ) -> io::Result<()>
    where
        Out: io::Write,
        Print: Printer,
        Ctx: SourceProvider,
    {
        printer.write(out, ctx, self)
    }

    pub fn print_stderr<Ctx: SourceProvider>(&self, ctx: &Ctx) -> io::Result<()> {
        self.write(&mut io::stderr(), &PrettyPrinter::new(), ctx)
    }
}

impl<T: error::Error> From<T> for Error {
    fn from(err: T) -> Self {
        Self::new(err.to_string(), SourceSpan::default())
    }
}

/// A trait for types that can be converted into an error diagnostic.
pub trait IntoError: Sized {
    fn into_error(self) -> Error;
    fn into_error_result<T>(self) -> Result<T, Error> {
        Err(self.into_error())
    }
}

impl<T: error::Error> IntoError for T {
    fn into_error(self) -> Error {
        Error::new(self.to_string(), SourceSpan::default())
    }
}

/// A trait for types that can be converted into an error diagnostic with added context.
pub trait IntoErrorCtx<Ctx>: Sized {
    fn into_error_ctx(self, ctx: &Ctx) -> Error;
    fn into_error_ctx_result<T>(self, ctx: &Ctx) -> Result<T, Error> {
        Err(self.into_error_ctx(ctx))
    }
}

impl<T: IntoError, Ctx> IntoErrorCtx<Ctx> for T {
    fn into_error_ctx(self, ctx: &Ctx) -> Error {
        self.into_error()
    }
}
