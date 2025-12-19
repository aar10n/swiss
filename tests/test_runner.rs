use clap::Parser;
use std::env;
use std::error::Error;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::{
    atomic::{AtomicBool, Ordering},
    mpsc, Arc, Mutex,
};
use std::thread;

#[derive(Debug, Clone)]
pub struct TestCase {
    pub name: String,
    pub input: String,
    pub expect_type: ExpectType,
    pub expected: String,
}

#[derive(Debug, Clone)]
pub enum ExpectType {
    Output,
    Error,
}

#[derive(Debug)]
pub struct TestFile {
    pub include_file: Option<String>,
    pub setup_code: Option<String>,
    pub test_cases: Vec<TestCase>,
}

#[derive(Clone, Debug)]
pub struct TestRunner {
    pub test_dir: PathBuf,
}

/// CLI options for the test runner.
#[derive(Parser, Debug, Clone)]
#[command(author, version, about = "Swiss language test runner", long_about = None)]
pub struct CliArgs {
    /// Filters to select which test files to run (matches file name)
    pub filters: Vec<String>,

    /// Verbose output (print per-test-case results and detailed summary)
    #[arg(short = 'v', long = "verbose")]
    pub verbose: bool,

    /// Test directory containing .test files
    #[arg(short = 'd', long = "test-dir", default_value = "./tests/cases/")]
    pub test_dir: String,

    /// Stop on first failure
    #[arg(short = 'F', long = "fail-fast")]
    pub fail_fast: bool,

    /// Number of worker threads (1 disables parallelism)
    #[arg(short = 'j', long = "jobs", default_value = "4")]
    pub jobs: usize,

    /// Color output: auto, always, never
    #[arg(long = "color", default_value = "auto", value_parser = ["auto", "always", "never"])]
    pub color: String,
}

impl TestRunner {
    pub fn new<P: AsRef<Path>>(test_dir: P) -> Self {
        Self {
            test_dir: test_dir.as_ref().to_path_buf(),
        }
    }

    pub fn run_all_tests(
        &self,
        verbose: bool,
        filters: &[String],
        fail_fast: bool,
        jobs: usize,
        use_color: bool,
    ) -> Result<(), Box<dyn Error>> {
        let swiss_bin = resolve_swiss_bin()?;

        // Collect test files in deterministic order
        let mut entries: Vec<PathBuf> = fs::read_dir(&self.test_dir)?
            .filter_map(|e| e.ok().map(|e| e.path()))
            .filter(|p| p.extension() == Some(std::ffi::OsStr::new("test")))
            .collect();
        entries.sort();

        // Parse files and enqueue tasks
        struct Task {
            file_idx: usize,
            case_idx: usize,
            test_case: TestCase,
            include: Option<String>,
            setup: Option<String>,
            swiss_bin: PathBuf,
        }

        let mut tasks: Vec<Task> = Vec::new();
        let mut per_file_counts: Vec<usize> = Vec::new();
        let mut per_file_names: Vec<PathBuf> = Vec::new();

        for (file_idx, path) in entries.into_iter().enumerate() {
            let file_name = path
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or_default()
                .to_string();

            if !filters.is_empty() && !filters.iter().any(|f| file_name.contains(f)) {
                continue;
            }

            let content = fs::read_to_string(&path)?;
            let parsed = self.parse_test_file(&content)?;
            per_file_counts.push(parsed.test_cases.len());
            per_file_names.push(path.clone());

            for (case_idx, test_case) in parsed.test_cases.into_iter().enumerate() {
                tasks.push(Task {
                    file_idx,
                    case_idx,
                    test_case,
                    include: parsed.include_file.clone(),
                    setup: parsed.setup_code.clone(),
                    swiss_bin: swiss_bin.clone(),
                });
            }
        }

        let total_tests: usize = per_file_counts.iter().sum();
        let mut passed_tests = 0usize;
        let mut failed_tests: Vec<(usize, usize, String)> = Vec::new();

        // Shared state
        let stop_flag = Arc::new(AtomicBool::new(false));

        // Task queue and result channel
        let (task_tx, task_rx) = mpsc::channel::<Task>();
        let task_rx = Arc::new(Mutex::new(task_rx));
        let (res_tx, res_rx) = mpsc::channel::<(usize, usize, String, bool)>();

        // Spawn workers
        let worker_count = if jobs == 0 { 1 } else { jobs };
        for _ in 0..worker_count {
            let rx = Arc::clone(&task_rx);
            let runner = self.clone();
            let stop = Arc::clone(&stop_flag);
            let res_tx = res_tx.clone();
            let verbose_worker = verbose;
            thread::spawn(move || {
                loop {
                    if stop.load(Ordering::SeqCst) {
                        break;
                    }
                    let task = {
                        let lock = rx.lock().unwrap();
                        match lock.recv() {
                            Ok(t) => t,
                            Err(_) => break, // channel closed
                        }
                    };
                    if stop.load(Ordering::SeqCst) {
                        break;
                    }
                    if verbose_worker {
                        print_with_color(
                            use_color,
                            Color::Cyan,
                            &format!("  \u{2192} {}", task.test_case.name),
                        );
                    }
                    let passed = runner
                        .run_test_case(&task.test_case, &task.include, &task.setup, &task.swiss_bin)
                        .unwrap_or(false);
                    if verbose_worker {
                        let status = if passed { "✓" } else { "✗" };
                        let color = if passed { Color::Green } else { Color::Red };
                        print_with_color(
                            use_color,
                            color,
                            &format!("  {} {}", status, task.test_case.name),
                        );
                    }
                    let _ =
                        res_tx.send((task.file_idx, task.case_idx, task.test_case.name, passed));
                }
            });
        }
        drop(res_tx); // keep one sender alive per worker

        // Enqueue tasks
        for task in tasks {
            if stop_flag.load(Ordering::SeqCst) {
                break;
            }
            if task_tx.send(task).is_err() {
                break;
            }
        }
        drop(task_tx);

        // Collect results
        let mut results: Vec<Vec<Option<(String, bool)>>> = per_file_counts
            .iter()
            .map(|&count| vec![None; count])
            .collect();

        while let Ok((file_idx, case_idx, name, passed)) = res_rx.recv() {
            if let Some(slot) = results
                .get_mut(file_idx)
                .and_then(|cases| cases.get_mut(case_idx))
            {
                *slot = Some((name.clone(), passed));
            }

            if passed {
                passed_tests += 1;
            } else {
                failed_tests.push((file_idx, case_idx, name.clone()));
                if fail_fast {
                    stop_flag.store(true, Ordering::SeqCst);
                    break;
                }
            }
        }

        // Reporting
        for (file_idx, path) in per_file_names.iter().enumerate() {
            let cases = results.get(file_idx);
            if verbose {
                print_with_color(
                    use_color,
                    Color::Yellow,
                    &format!("Running test file: {}", path.display()),
                );
            }
            if let Some(cases) = cases {
                for entry in cases {
                    if let Some((name, passed)) = entry {
                        if *passed {
                            if verbose {
                                print_with_color(use_color, Color::Green, &format!("  ✓ {}", name));
                            }
                        } else {
                            print_with_color(use_color, Color::Red, &format!("  ✗ {}", name));
                        }
                    }
                }
            }
        }

        let failed_count = failed_tests.len();
        if verbose {
            print_plain("\nTest Results:");
            print_plain(&format!("  Total: {}", total_tests));
            print_with_color(
                use_color,
                Color::Green,
                &format!("  Passed: {}", passed_tests),
            );
            print_with_color(
                use_color,
                if failed_count == 0 {
                    Color::Green
                } else {
                    Color::Red
                },
                &format!("  Failed: {}", failed_count),
            );
        } else {
            print_plain(&format!(
                "Results: passed {} / {}, failed {}",
                passed_tests, total_tests, failed_count
            ));
        }

        if failed_count > 0 {
            return Err("Some tests failed".into());
        }

        Ok(())
    }

    #[allow(dead_code)]
    #[allow(dead_code)]
    pub fn run_test_file(
        &self,
        path: &Path,
    ) -> Result<Vec<(String, bool)>, Box<dyn std::error::Error>> {
        let content = fs::read_to_string(path)?;
        let test_file = self.parse_test_file(&content)?;
        let mut results = Vec::new();
        let swiss_bin = resolve_swiss_bin()?;

        for test_case in test_file.test_cases {
            let passed = self.run_test_case(
                &test_case,
                &test_file.include_file,
                &test_file.setup_code,
                &swiss_bin,
            )?;
            results.push((test_case.name, passed));
        }

        Ok(results)
    }

    fn parse_test_file(&self, content: &str) -> Result<TestFile, Box<dyn std::error::Error>> {
        let mut include_file = None;
        let mut setup_code = None;
        let mut test_cases = Vec::new();
        let mut lines = content.lines().peekable();

        while let Some(line) = lines.next() {
            let line = line.trim();

            // Skip empty lines and comments
            if line.is_empty() || line.starts_with("//") {
                continue;
            }

            if line.starts_with("INCLUDE:") {
                include_file = Some(line[8..].trim().to_string());
            } else if line.starts_with("SETUP:") {
                // Parse multi-line setup code
                let first_line = line[6..].trim();
                let mut setup_lines = Vec::new();

                if !first_line.is_empty() {
                    // Single-line setup
                    setup_lines.push(first_line.to_string());
                } else {
                    // Multi-line setup - collect until we hit TEST:, INCLUDE:, empty line after content, or comment
                    let mut has_content = false;
                    while let Some(&next_line) = lines.peek() {
                        let next_line_trimmed = next_line.trim();

                        // Stop at TEST:, INCLUDE:, or comment lines
                        if next_line_trimmed.starts_with("TEST:")
                            || next_line_trimmed.starts_with("INCLUDE:")
                            || next_line_trimmed.starts_with("//")
                        {
                            break;
                        }

                        // Stop at empty line if we've already collected some content
                        if next_line_trimmed.is_empty() && has_content {
                            break;
                        }

                        let line = lines.next().unwrap();
                        if !line.trim().is_empty() {
                            has_content = true;
                            setup_lines.push(line.to_string());
                        }
                    }
                }

                if !setup_lines.is_empty() {
                    setup_code = Some(setup_lines.join("\n"));
                }
            } else if line.starts_with("TEST:") {
                let test_name = line[5..].trim().to_string();
                let mut input = String::new();
                let mut expect_type = ExpectType::Output;
                let mut expected = String::new();

                // Parse the test case body
                while let Some(&next_line) = lines.peek() {
                    let next_line = next_line.trim();
                    if next_line.starts_with("TEST:") || next_line.is_empty() {
                        break;
                    }

                    let line = lines.next().unwrap().trim();

                    if line.starts_with("INPUT:") {
                        let first_line = line[6..].trim();
                        if first_line.is_empty() {
                            // Multi-line input - collect until we hit EXPECT: or EXPECT_ERROR:
                            let mut input_lines = Vec::new();
                            while let Some(&next_line) = lines.peek() {
                                let next_line = next_line.trim();
                                if next_line.starts_with("EXPECT:")
                                    || next_line.starts_with("EXPECT_ERROR:")
                                    || next_line.starts_with("TEST:")
                                {
                                    break;
                                }
                                input_lines.push(lines.next().unwrap().trim().to_string());
                            }
                            input = input_lines.join("\n");
                        } else if first_line == "{" {
                            // Brace-delimited block - collect until matching closing brace
                            // The braces are just delimiters and not included in the actual input
                            let mut input_lines = Vec::new();
                            let mut brace_depth = 1; // We've seen the opening brace
                            let mut found_closing_brace = false;

                            while let Some(next_line) = lines.next() {
                                let trimmed = next_line.trim();

                                // Count braces in this line
                                let opens = trimmed.matches('{').count();
                                let closes = trimmed.matches('}').count();
                                let delta = opens as i32 - closes as i32;

                                // Check if adding this line would close the block
                                if brace_depth + delta == 0 && trimmed == "}" {
                                    found_closing_brace = true;
                                    break;
                                }

                                // Update depth and add the line
                                brace_depth += delta;
                                input_lines.push(trimmed.to_string());
                            }

                            if !found_closing_brace {
                                eprintln!("Warning: Unclosed brace in INPUT block");
                            }

                            input = input_lines.join("\n");
                        } else {
                            // Single-line input
                            input = first_line.to_string();
                        }
                    } else if line.starts_with("EXPECT:") {
                        expect_type = ExpectType::Output;
                        expected = line[7..].trim().to_string();
                    } else if line.starts_with("EXPECT_ERROR:") {
                        expect_type = ExpectType::Error;
                        expected = line[13..].trim().to_string();
                    }
                }

                if !input.is_empty() && !expected.is_empty() {
                    test_cases.push(TestCase {
                        name: test_name,
                        input,
                        expect_type,
                        expected,
                    });
                }
            }
        }

        Ok(TestFile {
            include_file,
            setup_code,
            test_cases,
        })
    }

    fn run_test_case(
        &self,
        test_case: &TestCase,
        include_file: &Option<String>,
        setup_code: &Option<String>,
        swiss_bin: &Path,
    ) -> Result<bool, Box<dyn std::error::Error>> {
        let mut args: Vec<String> = vec![];
        if let Some(include) = include_file {
            args.push("-f".to_string());
            args.push(include.clone());
        }

        // Build the full input: setup code + test input
        let mut full_input = String::new();
        if let Some(setup) = setup_code {
            // Add semicolons to setup code lines to ensure they're treated as separate statements
            for line in setup.lines() {
                let trimmed = line.trim();
                if !trimmed.is_empty() && !trimmed.starts_with(';') {
                    full_input.push_str(line);
                    // Add semicolon if the line doesn't already end with one
                    if !trimmed.ends_with(';') {
                        full_input.push(';');
                    }
                    full_input.push('\n');
                } else {
                    full_input.push_str(line);
                    full_input.push('\n');
                }
            }
            // Ensure there's a newline after setup code
            if !full_input.ends_with('\n') {
                full_input.push('\n');
            }
        }
        full_input.push_str(&test_case.input);

        // Use stdin for all inputs - Swiss now properly handles this
        let mut cmd = Command::new(swiss_bin);
        cmd.args(&args);
        cmd.stdin(std::process::Stdio::piped());
        cmd.stdout(std::process::Stdio::piped());
        cmd.stderr(std::process::Stdio::piped());

        let mut child = cmd.spawn().expect("Failed to spawn cargo process");

        // Write input to stdin
        if let Some(stdin) = child.stdin.take() {
            use std::io::Write;
            let mut stdin = stdin;
            stdin
                .write_all(full_input.as_bytes())
                .expect("Failed to write to stdin");
        }

        let output = child.wait_with_output().expect("Failed to read output");

        self.check_test_output(test_case, output)
    }

    fn check_test_output(
        &self,
        test_case: &TestCase,
        output: std::process::Output,
    ) -> Result<bool, Box<dyn std::error::Error>> {
        let success = match test_case.expect_type {
            ExpectType::Output => {
                let stdout = String::from_utf8_lossy(&output.stdout);
                let result_line = stdout
                    .lines()
                    .find(|line| line.contains("RESULT:"))
                    .unwrap_or("");

                if let Some(result_part) = result_line.split("RESULT:").nth(1) {
                    let actual = result_part.trim();
                    // Remove ANSI color codes for comparison
                    let actual_clean = strip_ansi_codes(actual).trim().to_string();
                    let expected_clean = strip_ansi_codes(&test_case.expected).trim().to_string();

                    let matches = actual_clean == expected_clean;
                    if !matches {
                        println!("    Expected: '{}'", expected_clean);
                        println!("    Actual:   '{}'", actual_clean);
                    }
                    matches
                } else {
                    println!("    No RESULT found in output");
                    false
                }
            }
            ExpectType::Error => {
                let stderr = String::from_utf8_lossy(&output.stderr);
                let actual_clean = strip_ansi_codes(&stderr);
                let expected_clean = strip_ansi_codes(&test_case.expected);

                let matches = actual_clean.contains(&expected_clean);
                if !matches {
                    println!("    Expected error containing: '{}'", expected_clean);
                    println!("    Actual error: '{}'", actual_clean);
                }
                matches
            }
        };

        Ok(success)
    }
}

/// Entry point for CLI usage.
#[allow(dead_code)]
pub fn run_cli() -> Result<(), Box<dyn Error>> {
    let args = CliArgs::parse();
    let runner = TestRunner::new(&args.test_dir);

    // Determine color usage
    let use_color = match args.color.as_str() {
        "always" => true,
        "never" => false,
        "auto" | _ => {
            let no_color = env::var_os("NO_COLOR").is_some();
            let is_tty = atty::is(atty::Stream::Stdout);
            is_tty && !no_color
        }
    };

    runner.run_all_tests(
        args.verbose,
        &args.filters,
        args.fail_fast,
        args.jobs,
        use_color,
    )
}

#[derive(Copy, Clone)]
enum Color {
    Green,
    Red,
    Yellow,
    Cyan,
}

fn print_with_color(enabled: bool, color: Color, msg: &str) {
    if enabled {
        let code = match color {
            Color::Green => "\x1b[32m",
            Color::Red => "\x1b[31m",
            Color::Yellow => "\x1b[33m",
            Color::Cyan => "\x1b[36m",
        };
        let mut out = std::io::stdout();
        let _ = writeln!(out, "{}{}{}", code, msg, "\x1b[0m");
    } else {
        let mut out = std::io::stdout();
        let _ = writeln!(out, "{}", msg);
    }
}

fn print_plain(msg: &str) {
    let mut out = std::io::stdout();
    let _ = writeln!(out, "{}", msg);
}

fn resolve_swiss_bin() -> Result<PathBuf, Box<dyn Error>> {
    // Prefer cargo-provided env var if available
    if let Ok(bin) = env::var("CARGO_BIN_EXE_swiss") {
        return Ok(PathBuf::from(bin));
    }

    // Fallback to target/debug/swiss
    let path = PathBuf::from("target/debug/swiss");
    if !path.exists() {
        let status = Command::new("cargo")
            .args(["build", "--bin", "swiss"])
            .status()?;
        if !status.success() {
            return Err("failed to build swiss binary".into());
        }
    }
    Ok(path)
}

fn strip_ansi_codes(input: &str) -> String {
    // Simple regex-free ANSI code stripper
    let mut result = String::new();
    let mut chars = input.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\x1b' {
            // Skip escape sequence
            if chars.peek() == Some(&'[') {
                chars.next(); // consume '['
                while let Some(ch) = chars.next() {
                    if ch.is_ascii_alphabetic() {
                        break;
                    }
                }
            }
        } else {
            result.push(ch);
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_runner() {
        let test_runner = TestRunner::new("tests/cases");

        // Create test directory if it doesn't exist
        fs::create_dir_all("tests/cases").unwrap();

        match test_runner.run_all_tests(true, &[], false, 1, false) {
            Ok(()) => println!("All tests passed!"),
            Err(e) => panic!("Tests failed: {}", e),
        }
    }

    #[test]
    fn test_file_parsing() {
        let content = r#"
INCLUDE: prelude.ch

SETUP:
    x = 10
    y = 20

TEST: Basic addition
INPUT: 2 + 3
EXPECT: 5

TEST: Unit conversion
INPUT: 60s -> min  
EXPECT: 1 min

TEST: Error case
INPUT: 60s -> 5
EXPECT_ERROR: expected 'unit', found 'number'
"#;

        let runner = TestRunner::new(".");
        let test_file = runner.parse_test_file(content).unwrap();

        assert_eq!(test_file.include_file, Some("prelude.ch".to_string()));
        assert!(test_file.setup_code.is_some());
        assert_eq!(test_file.test_cases.len(), 3);

        assert_eq!(test_file.test_cases[0].name, "Basic addition");
        assert_eq!(test_file.test_cases[0].input, "2 + 3");
        assert_eq!(test_file.test_cases[0].expected, "5");

        assert_eq!(test_file.test_cases[2].name, "Error case");
        assert!(matches!(
            test_file.test_cases[2].expect_type,
            ExpectType::Error
        ));
    }
}
