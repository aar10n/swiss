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

use print::PrettyPrint;
use print::PrettyString;
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
    files: Vec<String>,

    #[arg(short, help = "Run an interactive REPL")]
    interactive: bool,

    #[clap(value_parser)]
    input: Option<String>,
}

fn main() -> io::Result<()> {
    let args = Args::parse();
    let mut sources = Vec::new();
    for path in args.files {
        sources.push((path.clone(), read_from_file(&path)));
    }

    match args.input.as_deref() {
        Some("-") => sources.push(("<stdin>".to_owned(), read_from_stdin())),
        Some(file) => sources.push((file.to_owned(), read_from_file(file))),
        None => (),
    };

    // create the runtime context
    let mut ctx = driver::new_context();
    let module_id = ctx.modules.new_module("global").unwrap().id;
    for (path, source) in sources.into_iter() {
        let source_id = ctx.sources.add_source(path, source);
        if let Err(err) = driver::eval_source(&mut ctx, source_id, module_id) {
            err.print_stderr(&ctx)?;
            std::process::exit(1);
        }
    }

    if !args.interactive {
        return Ok(());
    }

    repl::main(&mut ctx, |ctx, (_, line)| {
        let source_id = ctx.sources.add_source(format!("<module>"), line);
        if let Err(err) = driver::eval_source(ctx, source_id, module_id) {
            err.print_stderr(ctx);
        }
        Ok(()) // continue
    });
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
