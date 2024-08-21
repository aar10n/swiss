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
use runtime::Context;
use source::SourceFile;

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

    match args.input.as_deref() {
        Some("-") => sources.push(("<stdin>".to_owned(), read_from_stdin())),
        Some(file) => sources.push((file.to_owned(), read_from_file(file))),
        None => (),
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
        Ok(Some(value)) if print_result => Ok(println!(
            "{GREEN}RESULT:{RESET} {}",
            value.pretty_string(ctx)
        )),
        Ok(None) if print_result => Ok(println!("{GREEN}RESULT:{RESET} {YELLOW}None{RESET}")),
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
