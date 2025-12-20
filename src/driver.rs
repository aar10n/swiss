use crate::ast::SourceSpan;
use crate::diag::{Error, IntoError, IntoErrorCtx};
use crate::id::{ModuleId, SourceId};
use crate::print::{PrettyPrint, PrettyString};
use crate::runtime::{builtin, Context, Value};

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
) -> Result<Option<Value>, Error> {
    // Ensure the module is seeded with prelude declarations before parsing so
    // operator/unit lookups behave consistently.
    ctx.apply_preludes_to_module(module_id)
        .map_err(|e| e.into_error())?;

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
        Ok(value) => Ok(value),
        Err(err) => Err(err.into_error_ctx(&ctx)),
    }
}

/// Evaluate all prelude files (if any) and cache their modules in the context.
pub fn eval_preludes(ctx: &mut Context) -> Result<(), ()> {
    let prelude_paths = ctx.config.prelude_files.clone();
    for path in prelude_paths {
        let source = match std::fs::read_to_string(&path) {
            Ok(code) => code,
            Err(err) => {
                eprintln!("error reading prelude '{}': {:?}", path, err);
                return Err(());
            }
        };

        let source_id = ctx.sources.add_source(path.clone(), source);
        let module_path = ctx.sources[source_id].module_path();

        let module = match ctx.modules.get_or_add_module(module_path.clone()) {
            Ok(module) => module,
            Err(err) => {
                let _ = err.into_error().print_stderr(ctx);
                return Err(());
            }
        };
        let module_id = module.id;

        match eval_source(ctx, source_id, module_id) {
            Ok(_) => ctx.prelude_modules.push(module_id),
            Err(err) => {
                let _ = err.print_stderr(ctx);
                return Err(());
            }
        }
    }

    Ok(())
}
