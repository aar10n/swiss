mod interp;
// mod unit;
// mod value;

use crate::ast::{Expr, Module};
use crate::diag::{Error, IntoErrorCtx};
use crate::print::PrettyString;
use crate::runtime::{Context, DeclError, Exception, NameError, TypeError};
use crate::source::{SourcePos, Spanned};

pub use crate::runtime::{ModuleId, Value};
use interp::Interp;
pub use interp::*;

pub type InterpResult<T> = Result<T, InterpError>;

#[derive(Debug)]
pub enum InterpError {
    DeclError(DeclError),
    NameError(NameError),
    TypeError(TypeError),
    Exception(Exception),
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
impl_from_error!(NameError);
impl_from_error!(TypeError);
impl_from_error!(Exception);

impl IntoErrorCtx<Context> for InterpError {
    fn into_error_ctx(self, ctx: &Context) -> Error {
        match self {
            InterpError::DeclError(err) => err.into_error_ctx(ctx),
            InterpError::NameError(err) => err.into_error_ctx(ctx),
            InterpError::TypeError(err) => err.into_error_ctx(ctx),
            InterpError::Exception(err) => err.into_error_ctx(ctx),
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
    ctx.with_active_module(ast_module.module_id, |ctx| {
        let mut interp = interp::Interpreter::new(ctx);
        let mut value = None;
        for item in &ast_module.items {
            value = item.eval(&mut interp)?;
        }
        Ok(value)
    })
}
