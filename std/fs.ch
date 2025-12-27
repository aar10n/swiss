;
; File
;

; Represents a file object.
#[type=builtin::file]
type file
#[builtin]
; Returns true if the file is still open.
fn (file) is_open(f: file): bool { builtin::file_impl::is_open(f) }
#[builtin]
fn (file) close(f: file) { builtin::file_impl::close(f) }
#[builtin]
fn (file) read_all(f: file) { builtin::file_impl::read_all(f) }
#[builtin]
; Writes a value to the file.
fn (file) write(f: file, v: any) { builtin::file_impl::write(f, v) }
#[builtin]
; Writes a value plus a newline to the file.
fn (file) writeln(f: file, v: any) { builtin::file_impl::writeln(f, v) }
#[builtin]
; Flushes buffered writes to disk.
fn (file) flush(f: file) { builtin::file_impl::flush(f) }

;
; File system
;

; Opens a file at the given path and returns a file object.
; Supported modes: "r" (default), "w", "a", "r+".
fn open(path: str, mode: str?): file {
  builtin::fs::open(path, mode)
}

#[builtin]
; Reads the contents of a directory returning a list of all entries as strings.
; If no path is provided, uses the current working directory.
fn listdir(path: str?): list { builtin::fs::listdir(path) }

#[builtin]
; Returns the current working directory as a string.
fn cwd(): str { builtin::fs::cwd() }

#[builtin]
; Checks if a path exists.
fn exists(path: str): bool { builtin::fs::exists(path) }

#[builtin]
; Creates a directory at the specified path. If recursive is true, creates parent directories as needed.
fn mkdir(path: str, recursive: bool?): bool { builtin::fs::mkdir(path, recursive) }

#[builtin]
; Removes the file or directory at the specified path.
fn remove(path: str) { builtin::fs::remove(path) }

;
; Path
;

module path {
  #[builtin]
  ; Joins two path segments.
  fn join(a: str, b: str): str { builtin::path::join(a, b) }

  #[builtin]
  ; Returns the directory component of a path.
  fn dirname(path: str): str { builtin::path::dirname(path) }

  #[builtin]
  ; Returns the final path component.
  fn basename(path: str): str { builtin::path::basename(path) }

  #[builtin]
  ; Returns the extension of a path (without leading '.').
  fn extname(path: str): str { builtin::path::extname(path) }

  #[builtin]
  ; Normalizes a path, collapsing '.' and '..' when possible.
  fn normalize(path: str): str { builtin::path::normalize(path) }

  #[builtin]
  ; Returns true if the path is absolute.
  fn is_abs(path: str): bool { builtin::path::is_abs(path) }
}
