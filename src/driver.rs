use crate::ast::SourceSpan;
use crate::diag::{Error, IntoError, IntoErrorCtx};
use crate::id::{ModuleId, SourceId};
use crate::print::ansi::{GREEN, RESET, YELLOW};
use crate::print::{PrettyPrint, PrettyString};
use crate::runtime::{builtin, Context};

use crate::interp;
use crate::lexer;
use crate::parser;

use std::io;

pub fn new_context() -> Context {
    let mut ctx = Context::new();
    builtin::register_builtin_module(&mut ctx);
    ctx
}

pub fn eval_source(
    ctx: &mut Context,
    source_id: SourceId,
    module_id: ModuleId,
) -> Result<(), Error> {
    let tokens = match lexer::lex(source_id, ctx.sources[source_id].raw()) {
        Ok(tokens) => tokens,
        Err(err) => return err.into_error_result(),
    };

    if std::env::var("TRACE_TOKENS").is_ok() {
        for (token, span) in &tokens {
            let source_ref = ctx.sources.lookup_span(*span).unwrap();
            let start_loc = source_ref.start_loc();
            let end_loc = source_ref.end_loc();
            println!(
                "token: {} from={{ln={}, col={}}} to={{ln={}, col={}}} ({}:{}) len={}",
                token.pretty_string(&()),
                start_loc.line,
                start_loc.column,
                end_loc.line,
                end_loc.column,
                span.start,
                span.end,
                source_ref.len()
            );
        }
    }

    let mut module = match parser::parse(&mut ctx.modules[module_id], &tokens) {
        Ok(module) => module,
        Err(err) => return err.into_error_result(),
    };

    if std::env::var("TRACE_AST").is_ok() {
        module.print_stdout(&());
    }

    match interp::interpret(ctx, &mut module) {
        Ok(value) => {
            if let Some(value) = value {
                println!("{GREEN}RESULT:{RESET} {}", value.pretty_string(&ctx));
            } else {
                println!("{GREEN}RESULT:{RESET} {YELLOW}None{RESET}");
            }
            Ok(())
        }
        Err(err) => Err(err.into_error_ctx(&ctx)),
    }
}
