;
; Environment variables
;

#[builtin]
; Gets the value of an environment variable if it exists.
fn getenv(var: str): str? { builtin::env::getenv(var) }

#[builtin]
; Sets the value of an environment variable.
fn setenv(var: str, value: str) { builtin::env::setenv(var, value) }

#[builtin]
; Unsets an environment variable.
fn unsetenv(var: str) { builtin::env::unsetenv(var) }

; Returns true if an environment variable is set.
fn has(var: str): bool { getenv(var) != () }
