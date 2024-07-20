use std::fmt::format;

use crate::driver;
use crate::runtime::Context;
use crate::source::SourceId;

use rustyline::error::ReadlineError;
use rustyline::{Cmd, DefaultEditor, EventHandler, KeyCode, KeyEvent, Modifiers};

pub fn main(
    ctx: &mut Context,
    mut on_input: impl FnMut(&mut Context, (i32, String)) -> Result<(), i32>,
) -> ! {
    let mut rl = DefaultEditor::new().unwrap();
    _ = rl.load_history("target/history.txt");
    rl.bind_sequence(
        KeyEvent(KeyCode::Tab, Modifiers::NONE),
        EventHandler::Simple(Cmd::Insert(1, "  ".into())),
    );
    rl.bind_sequence(
        KeyEvent(KeyCode::Char('N'), Modifiers::CTRL),
        EventHandler::Simple(Cmd::Newline),
    );

    let mut exit_code = 0;
    let mut line_count = 0;
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.clone()).unwrap();
                if let Err(code) = on_input(ctx, (line_count, line)) {
                    exit_code = code;
                    break;
                }
                line_count += 1;
            }
            Err(ReadlineError::Eof) => {
                println!("ctrl-d");
                break;
            }
            Err(ReadlineError::Interrupted) => {
                println!("ctrl-c");
                exit_code = 1;
                break;
            }
            Err(err) => {
                eprintln!("error: {:?}", err);
                exit_code = 1;
                break;
            }
        }
    }

    _ = rl.save_history("target/history.txt");
    std::process::exit(exit_code);
}
