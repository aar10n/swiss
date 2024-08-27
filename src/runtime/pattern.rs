use super::context::Context;
use super::value::Value;
use super::Exception;

use crate::print::ansi::{
    chars::{LPARN, RPARN},
    BOLD, PUNCT, RESET,
};
use crate::print::{PrettyPrint, PrettyString};
use crate::source::{SourceSpan, Spanned};

use smallvec::{smallvec, SmallVec};
use ustr::Ustr;

#[derive(Clone, Debug)]
pub enum Pattern {
    Ignore,
    Var(Spanned<Ustr>),
    Tuple(Vec<Pattern>),
}

impl Pattern {
    pub fn bind_with(
        &self,
        ctx: &Context,
        value: Value,
    ) -> Result<SmallVec<[(Ustr, Value); 3]>, Exception> {
        match self {
            Pattern::Ignore => Ok(SmallVec::new()),
            Pattern::Var(ident) => Ok(smallvec![(ident.raw, value)]),
            Pattern::Tuple(pats) => {
                if let Value::Tuple(values) = value {
                    if values.len() < pats.len() {
                        Err(Exception::new(
                            "ValueError",
                            format!("not enough values to bind (expected {})", pats.len()),
                        ))
                    } else if values.len() > pats.len() {
                        Err(Exception::new(
                            "ValueError",
                            format!("too many values to bind (expected {})", pats.len()),
                        ))
                    } else {
                        let mut bindings = SmallVec::new();
                        for (pat, value) in pats.iter().zip(values) {
                            bindings.extend(pat.bind_with(ctx, *value)?);
                        }
                        Ok(bindings)
                    }
                } else {
                    Err(Exception::new(
                        "ValueError",
                        format!("expected tuple, got {}", value.ty().pretty_string(ctx)),
                    ))
                }
            }
        }
    }
}

impl PrettyPrint<Context> for Pattern {
    fn pretty_print<Output: std::io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> std::io::Result<()> {
        match self {
            Pattern::Ignore => write!(out, "{BOLD}_{RESET}"),
            Pattern::Var(name) => write!(out, "{BOLD}{}{RESET}", name.raw),
            Pattern::Tuple(patterns) => {
                write!(out, "{LPARN}")?;
                for (i, pattern) in patterns.iter().enumerate() {
                    if i > 0 {
                        write!(out, "{PUNCT}, {RESET}")?;
                    }
                    pattern.pretty_print(out, ctx, level)?;
                }
                write!(out, "{RPARN}")
            }
        }
    }
}
