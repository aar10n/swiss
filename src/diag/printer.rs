use super::Error;
use crate::print::ansi::{RED, RESET};
use crate::source::{SourceProvider, SourceRef, SourceSpan};

use std::io;

/// A trait for diagnostic printers.
pub trait Printer {
    fn write<Out, Ctx>(&self, out: &mut Out, ctx: &Ctx, diag: &Error) -> io::Result<()>
    where
        Out: io::Write,
        Ctx: SourceProvider;
}

/// A diagnostic printer that pretty-prints diagnostics with colors.
#[derive(Clone)]
pub struct PrettyPrinter {}

impl PrettyPrinter {
    pub fn new() -> Self {
        Self {}
    }
}

impl PrettyPrinter {
    fn write_error<Out: io::Write, Ctx: SourceProvider>(
        &self,
        out: &mut Out,
        ctx: &Ctx,
        message: &str,
        span: SourceSpan,
    ) -> io::Result<()> {
        writeln!(out, "{RED}{}{RESET}", message)?;
        if span == SourceSpan::default() {
            return Ok(());
        }

        if let Some(source_ref) = ctx.sources().lookup_span(span) {
            self.write_source_lines_with_caret(out, source_ref, Some(""))
        } else {
            writeln!(out, "  <invalid source span>")
        }
    }

    fn write_source_lines_with_caret<Out: io::Write>(
        &self,
        out: &mut Out,
        source_ref: SourceRef,
        caret_note: Option<&str>,
    ) -> io::Result<()> {
        let start_loc = source_ref.start_loc();
        let end_loc = source_ref.end_loc();

        // print the source file+location
        write!(out, "  {}:{}", source_ref.file_name(), start_loc,)?;
        let max_cols = if start_loc.line != end_loc.line {
            writeln!(out, "-{}", end_loc)?; // multiline
        } else if (end_loc.column - start_loc.column) > 1 {
            writeln!(out, "-{}", end_loc.column)?; // multicolumn
        } else {
            writeln!(out)?; // single location
        };

        // print the source lines
        let ln_no_width = end_loc.line.to_string().len();
        for (line_no, line_str) in source_ref.lines() {
            writeln!(
                out,
                "{:>ln_no_width$} | {}",
                line_no,
                line_str,
                ln_no_width = ln_no_width
            )?;
        }

        // print the caret line
        let num_cols = source_ref.num_cols();
        let padding = " ".repeat(start_loc.column as usize + ln_no_width + 2);
        if num_cols == 1 {
            writeln!(out, "{}{RED}{}{RESET}", padding, '^')
        } else {
            let underline = "^".repeat(num_cols as usize);
            writeln!(out, "{}{RED}{}{RESET}", padding, underline)
        }
    }
}

impl Printer for PrettyPrinter {
    fn write<Out: io::Write, Ctx: SourceProvider>(
        &self,
        out: &mut Out,
        ctx: &Ctx,
        diag: &Error,
    ) -> io::Result<()> {
        self.write_error(out, ctx, &diag.message, diag.span)?;
        for (message, span) in &diag.extras {
            self.write_error(out, ctx, message, *span)?;
        }
        Ok(())
    }
}
