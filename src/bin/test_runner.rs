// Swiss test runner CLI entrypoint.
// Reuse the integration test runner module.
#[path = "../../tests/test_runner.rs"]
mod test_runner;

fn main() {
    if let Err(err) = test_runner::run_cli() {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}
