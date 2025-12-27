;
; Text utilities
;

#[builtin]
; Trims leading and trailing whitespace.
fn trim(s: str): str { builtin::text::trim(s) }

#[builtin]
; Trims leading whitespace.
fn trim_start(s: str): str { builtin::text::trim_start(s) }

#[builtin]
; Trims trailing whitespace.
fn trim_end(s: str): str { builtin::text::trim_end(s) }

#[builtin]
; Converts a string to lowercase.
fn lower(s: str): str { builtin::text::lower(s) }

#[builtin]
; Converts a string to uppercase.
fn upper(s: str): str { builtin::text::upper(s) }

#[builtin]
; Returns true if the string starts with the prefix.
fn starts_with(s: str, prefix: str): bool { builtin::text::starts_with(s, prefix) }

#[builtin]
; Returns true if the string ends with the suffix.
fn ends_with(s: str, suffix: str): bool { builtin::text::ends_with(s, suffix) }

#[builtin]
; Returns true if the string contains the needle.
fn contains(s: str, needle: str): bool { builtin::text::contains(s, needle) }

#[builtin]
; Replaces all occurrences of a substring.
fn replace(s: str, from: str, to: str): str { builtin::text::replace(s, from, to) }

#[builtin]
; Splits a string by the given separator or by whitespace when omitted.
fn split(s: str, sep: str?): list { builtin::text::split(s, sep) }

#[builtin]
; Joins a list of strings with a separator.
fn join(items: list, sep: str): str { builtin::text::join(items, sep) }

#[builtin]
; Splits a string on line boundaries.
fn lines(s: str): list { builtin::text::lines(s) }

#[builtin]
; Strips a prefix if present.
fn strip_prefix(s: str, pfx: str): str { builtin::text::strip_prefix(s, pfx) }

#[builtin]
; Strips a suffix if present.
fn strip_suffix(s: str, suffix: str): str { builtin::text::strip_suffix(s, suffix) }
