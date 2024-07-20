use super::{source_id, SourceId, SourceLoc, SourceSpan, Spanned};

use smallvec::{smallvec, SmallVec};
use std::env;
use std::fs::File;
use std::io::{self, Read};
use ustr::Ustr;

/// A source file.
#[derive(Debug, Clone)]
pub struct SourceFile {
    /// The source id.
    id: SourceId,
    /// The source file name.
    name: String,
    /// The raw source code.
    source: String,
    /// The start and end offsets of all lines of source code.
    lines: Vec<(usize, usize)>,
}

impl SourceFile {
    pub fn new(id: SourceId, name: String, source: String) -> Self {
        let lines = source_line_spans(&source);
        Self {
            id,
            name,
            source,
            lines,
        }
    }

    pub fn from_file(name: &str) -> io::Result<SourceFile> {
        let mut file = File::open(name)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        Ok(SourceFile::new(
            source_id::next(),
            name.to_owned(),
            contents,
        ))
    }

    /// Returns the source id.
    pub fn id(&self) -> SourceId {
        self.id
    }

    /// Returns the source file name.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Returns the raw source code.
    pub fn raw(&self) -> &str {
        &self.source
    }

    /// Returns the module name for the source file.
    pub fn module_name(&self) -> Ustr {
        // ex. "src/source/my-module.ch" -> "my_module"
        let file_name = self.name();
        Ustr::from(&sanitize_module_name(file_name))
    }

    /// Returns the full module path of the source file.
    pub fn module_path(&self) -> SmallVec<[Spanned<Ustr>; 4]> {
        // ex. "src/source/my-module.ch" -> src::source::my_module
        separate_module_path(self.name())
    }

    /// Returns the (line, column) for the given offset.
    pub fn decode_offset(&self, offset: usize) -> (u32, u32) {
        decode_offset_loc(&self.source, &self.lines, offset)
    }

    /// Returns the string for the given line number (1-based).
    pub fn line_str(&self, line: u32) -> Option<&str> {
        let line = line as usize;
        if line == 0 || line > self.lines.len() {
            return None;
        }

        self.lines.get(line - 1).map(|&(start, end)| {
            let line = &self.source[start..end];
            if line.ends_with('\n') {
                &line[..line.len() - 1]
            } else {
                line
            }
        })
    }

    /// Returns whether the span crosses more than one line.
    pub fn is_span_multiline(&self, span: &SourceSpan) -> bool {
        let (start_line, _) = self.decode_offset(span.start);
        let (end_line, _) = self.decode_offset(span.end);
        start_line != end_line
    }

    /// Returns the line number/string pairs for all lines in the given span.
    pub fn lines_for_span(&self, span: &SourceSpan) -> Vec<(u32, &str)> {
        let (start_line, _) = self.decode_offset(span.start);
        let (end_line, _) = self.decode_offset(span.end);
        (start_line..=end_line)
            .map(|line| (line, self.line_str(line).unwrap()))
            .collect()
    }
}

/// A reference to a span of source code.
#[derive(Debug, Clone)]
pub struct SourceRef<'a> {
    pub file: &'a SourceFile,
    pub start: usize,
    pub end: usize,
}

impl<'a> SourceRef<'a> {
    pub fn new(file: &'a SourceFile, start: usize, end: usize) -> Self {
        Self { file, start, end }
    }

    pub fn span(&self) -> SourceSpan {
        SourceSpan::new(self.file.id(), self.start, self.end)
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn num_cols(&self) -> u32 {
        let (start_ln, start_col) = self.file.decode_offset(self.start);
        let (end_ln, end_col) = self.file.decode_offset(self.end);
        if start_ln == end_ln {
            end_col - start_col
        } else {
            end_col
        }
    }

    pub fn str(&self) -> &str {
        &self.file.raw()[self.start..self.end]
    }

    pub fn lines(&self) -> Vec<(u32, &str)> {
        self.file.lines_for_span(&self.span())
    }

    pub fn file_name(&self) -> &str {
        &self.file.name
    }

    pub fn start_loc(&self) -> SourceLoc {
        let (line, column) = self.file.decode_offset(self.start);
        SourceLoc::new(self.file.id, line as u32, column as u32)
    }

    pub fn end_loc(&self) -> SourceLoc {
        let (line, column) = self.file.decode_offset(self.end);
        SourceLoc::new(self.file.id, line as u32, column as u32)
    }

    pub fn is_multiline(&self) -> bool {
        self.file.is_span_multiline(&self.span())
    }
}

// Returns a vector containting the start and end offsets of all lines.
fn source_line_spans(src: &str) -> Vec<(usize, usize)> {
    let mut lines = Vec::new();
    let mut start = 0;
    let mut end = 0;

    for (i, ch) in src.char_indices() {
        if ch == '\n' {
            end = i + 1;
            lines.push((start, end));
            start = end;
        }
    }

    if end < src.len() {
        lines.push((end, src.len()));
    }
    lines
}

// Converts an offset to a line/column pair.
fn decode_offset_loc(source: &str, lines: &[(usize, usize)], offset: usize) -> (u32, u32) {
    for (i, &(start, end)) in lines.iter().enumerate() {
        if offset >= start && offset < end {
            let line = (i as u32) + 1;
            let col = decode_offset_column(&source[start..end], start, end, offset);
            return (line, col as u32);
        }
    }
    if offset == source.len() {
        let line = lines.len() as u32;
        return (line, 1);
    }
    (0, 0)
}

fn decode_offset_column(line: &str, start: usize, end: usize, offset: usize) -> u32 {
    let mut cols = 1;
    for (col, (off, ch)) in line.char_indices().enumerate() {
        cols += 1;
        if (start + off) == offset {
            return (col as u32) + 1;
        }
    }
    cols
}

fn sanitize_module_name<S: AsRef<str>>(name: S) -> String {
    name.as_ref()
        .split('/')
        .last()
        .unwrap_or(name.as_ref())
        .split('.')
        .next()
        .unwrap_or(name.as_ref())
        .to_owned()
        .replace("-", "_")
}

fn separate_module_path(path: &str) -> SmallVec<[Spanned<Ustr>; 4]> {
    path.split('/')
        .map(|part| Spanned::from(Ustr::from(&sanitize_module_name(part))))
        .collect()
}
