use std::env;
use std::path::PathBuf;
use std::process;

// We'll need to include the test runner code here since it's in tests/
// For now, let's create a simple CLI interface

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 {
        eprintln!("Usage: {} <test_directory> [test_file]", args[0]);
        eprintln!("Examples:");
        eprintln!("  {} tests/cases                 # Run all tests", args[0]);
        eprintln!("  {} tests/cases arithmetic.test # Run specific test file", args[0]);
        process::exit(1);
    }

    let test_dir = PathBuf::from(&args[1]);
    
    if !test_dir.exists() {
        eprintln!("Test directory '{}' does not exist", test_dir.display());
        process::exit(1);
    }

    // For now, just run cargo test which will execute our integration tests
    println!("Running Swiss language tests from '{}'", test_dir.display());
    
    let status = std::process::Command::new("cargo")
        .args(&["test", "--test", "integration_tests"])
        .status()
        .expect("Failed to run cargo test");

    if !status.success() {
        process::exit(1);
    }
}