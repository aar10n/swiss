#![allow(warnings)]
mod ast;
mod diag;
mod driver;
mod id;
mod interp;
mod lexer;
mod parser;
mod print;
mod repl;
mod runtime;
mod source;

use id::{ModuleId, SourceId};
use print::ansi::{GREEN, RESET, YELLOW};
use print::{PrettyPrint, PrettyString};
use runtime::{Context, IoHandle, Value};
use source::{SourceFile, SourceSpan, Spanned};

use std::env;
use std::io;
use std::{io::Read, process};

use atty::Stream;
use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short, long, help = "Evaluate the given file(s)")]
    file: Vec<String>,

    #[arg(short, long, help = "Evaluate the given expression")]
    expr: Vec<String>,

    #[arg(short, long, help = "Run an interactive REPL")]
    interactive: bool,

    #[clap(value_parser)]
    input: Option<String>,
}

fn main() -> io::Result<()> {
    let args = Args::parse();
    let mut sources = Vec::new();
    for path in args.file {
        sources.push((path.clone(), read_from_file(&path)));
    }

    let has_expressions = !args.expr.is_empty();
    for expr in args.expr {
        sources.push(("<expr>".to_owned(), expr));
    }

    match args.input.as_deref() {
        Some("-") => sources.push(("<stdin>".to_owned(), read_from_stdin())),
        Some(file) => sources.push((file.to_owned(), read_from_file(file))),
        None => {
            // If no expressions were provided via -e, read from stdin
            if !has_expressions && !args.interactive && !atty::is(Stream::Stdin) {
                sources.push(("<stdin>".to_owned(), read_from_stdin()));
            }
        }
    };
    let num_sources = sources.len();

    // create the runtime context
    let mut ctx = driver::new_context();
    let module_id = ctx.modules.new_module("global").unwrap().id;
    for (i, (path, source)) in sources.into_iter().enumerate() {
        let source_id = ctx.sources.add_source(path, source);
        let print_result = i == num_sources - 1 && !args.interactive;
        if let Err(()) = evaluate(&mut ctx, source_id, module_id, print_result) {
            std::process::exit(1);
        }
    }

    if !args.interactive {
        return Ok(());
    }

    repl::main(&mut ctx, |ctx, (_, line)| {
        let source_id = ctx.sources.add_source(format!("<module>"), line);
        evaluate(ctx, source_id, module_id, /*print_result=*/ true);
        Ok(()) // continue
    });
}

fn evaluate(
    ctx: &mut Context,
    source_id: SourceId,
    module_id: ModuleId,
    print_result: bool,
) -> Result<(), ()> {
    match driver::eval_source(ctx, source_id, module_id) {
        Ok(Some(value)) if print_result => {
            use crate::print::DisplayString;
            if let Some(buf) = ctx.take_pending_output() {
                Ok(println!("{GREEN}RESULT:{RESET} {}", buf))
            } else {
                // If a default formatter is configured, try to use it before falling back to
                // the regular display_string.
                if let Some(fmt_name) = ctx.default_formatter.clone() {
                    let fmt_span = SourceSpan::default();
                    if let Ok(func) = ctx.modules[module_id]
                        .resolve_function(Spanned::new(fmt_name, fmt_span))
                        .cloned()
                    {
                        let io = IoHandle::buffer();
                        let res = Context::with_active_module(ctx, module_id, |ctx| {
                            crate::interp::call_function(
                                ctx,
                                func,
                                vec![value.clone(), Value::Io(io.clone())],
                            )
                        });
                        if res.is_ok() {
                            if let Some(buf) = io.take_buffer() {
                                return Ok(println!("{GREEN}RESULT:{RESET} {}", buf));
                            }
                        }
                    }
                }

                Ok(println!(
                    "{GREEN}RESULT:{RESET} {}",
                    value.display_string(ctx)
                ))
            }
        }
        Ok(None) if print_result => {
            // Fallback: if interpretation returned None, try to use the last evaluated value
            // (e.g., when a trailing directive wipes the result).
            if let Some(value) = ctx.take_last_value() {
                use crate::print::DisplayString;
                if let Some(buf) = ctx.take_pending_output() {
                    return Ok(println!("{GREEN}RESULT:{RESET} {}", buf));
                } else if let Some(fmt_name) = ctx.default_formatter.clone() {
                    let fmt_span = SourceSpan::default();
                    if let Ok(func) = ctx.modules[module_id]
                        .resolve_function(Spanned::new(fmt_name, fmt_span))
                        .cloned()
                    {
                        let io = IoHandle::buffer();
                        let res = Context::with_active_module(ctx, module_id, |ctx| {
                            crate::interp::call_function(
                                ctx,
                                func,
                                vec![value.clone(), Value::Io(io.clone())],
                            )
                        });
                        if res.is_ok() {
                            if let Some(buf) = io.take_buffer() {
                                return Ok(println!("{GREEN}RESULT:{RESET} {}", buf));
                            }
                        }
                    }
                }

                Ok(println!(
                    "{GREEN}RESULT:{RESET} {}",
                    value.display_string(ctx)
                ))
            } else if let Some(buf) = ctx.take_pending_output() {
                Ok(println!("{GREEN}RESULT:{RESET} {}", buf))
            } else {
                Ok(println!("{GREEN}RESULT:{RESET} {YELLOW}None{RESET}"))
            }
        }
        Ok(_) => Ok(()),
        Err(err) => {
            err.print_stderr(ctx);
            Err(())
        }
    }
}

fn read_from_file(path: &str) -> String {
    match std::fs::read_to_string(path) {
        Ok(code) => code,
        Err(err) => {
            eprintln!("error: {:?}", err);
            process::exit(1);
        }
    }
}

fn read_from_stdin() -> String {
    let mut code = String::new();
    match std::io::stdin().read_to_string(&mut code) {
        Ok(_) => code,
        Err(err) => {
            eprintln!("error: {:?}", err);
            process::exit(1);
        }
    }
}
