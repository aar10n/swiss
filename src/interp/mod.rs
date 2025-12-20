mod interp;

use crate::ast::{Expr, Module};
use crate::diag::{Error, IntoError, IntoErrorCtx};
use crate::lexer::LexError;
use crate::parser::ParseError;
use crate::print::PrettyString;
use crate::runtime::{Context, DeclError, NameError, TypeError};
use crate::source::{SourcePos, Spanned};

pub use crate::runtime::{Exception, ModuleId, VRef, Value};
pub use interp::*;

pub type InterpResult<T> = Result<T, InterpError>;

#[derive(Debug)]
pub enum InterpError {
    DeclError(DeclError),
    LexError(LexError),
    NameError(NameError),
    ParseError(ParseError),
    TypeError(TypeError),
    Exception(Exception),
    Break,
    Continue,
    Return(Value),
}

macro_rules! impl_from_error {
    ($error:ident) => {
        impl From<$error> for InterpError {
            fn from(err: $error) -> Self {
                InterpError::$error(err)
            }
        }
    };
}

impl_from_error!(DeclError);
impl_from_error!(LexError);
impl_from_error!(NameError);
impl_from_error!(ParseError);
impl_from_error!(TypeError);
impl_from_error!(Exception);

impl std::fmt::Display for InterpError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpError::DeclError(_) => write!(f, "declaration error"),
            InterpError::LexError(_) => write!(f, "lex error"),
            InterpError::NameError(_) => write!(f, "name error"),
            InterpError::ParseError(_) => write!(f, "parse error"),
            InterpError::TypeError(_) => write!(f, "type error"),
            InterpError::Exception(e) => write!(f, "{}: {}", e.kind, e.message),
            InterpError::Return(_) => write!(f, "'return' used outside of function"),
            InterpError::Break => write!(f, "'break' used outside of loop"),
            InterpError::Continue => write!(f, "'continue' used outside of loop"),
        }
    }
}

impl IntoErrorCtx<Context> for InterpError {
    fn into_error_ctx(self, ctx: &Context) -> Error {
        match self {
            InterpError::DeclError(err) => err.into_error_ctx(ctx),
            InterpError::LexError(err) => err.into_error(),
            InterpError::NameError(err) => err.into_error_ctx(ctx),
            InterpError::ParseError(err) => err.into_error(),
            InterpError::TypeError(err) => err.into_error_ctx(ctx),
            InterpError::Exception(err) => err.into_error_ctx(ctx),
            InterpError::Return(value) => Error::new(
                format!("'return' used outside of function"),
                crate::source::SourceSpan::default(),
            ),
            InterpError::Break => Error::new(
                "'break' used outside of loop".to_string(),
                crate::source::SourceSpan::default(),
            ),
            InterpError::Continue => Error::new(
                "'continue' used outside of loop".to_string(),
                crate::source::SourceSpan::default(),
            ),
        }
    }
}

pub fn evaluate(ctx: &mut Context, expr: &Expr) -> Result<Value, Error> {
    let mut interp = interp::Interpreter::new(ctx);
    match expr.eval(&mut interp) {
        Ok(value) => Ok(value),
        Err(err) => Err(err.into_error_ctx(&interp.ctx)),
    }
}

pub fn interpret(ctx: &mut Context, ast_module: &Module) -> InterpResult<Option<Value>> {
    Context::with_active_module(ctx, ast_module.module_id, |ctx| {
        let mut interp = interp::Interpreter::new(ctx).with_source(ast_module.source_id);
        let mut value = None;
        for item in &ast_module.items {
            if let Some(v) = item.eval(&mut interp)? {
                value = Some(v);
            }
        }
        Ok(value)
    })
}
