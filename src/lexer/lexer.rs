use super::charset::CharSet;
use super::iter::PeekableN;
use super::token::{StringPart, Token, KEYWORDS};
use super::{LexError, LexResult};
use crate::runtime::Context;
use crate::source::{SourceFile, SourceId, SourcePos, SourceSpan};

use std::ops::RangeInclusive;
use std::str::{CharIndices, Chars};

use rug::ops::CompleteRound;
use static_init::dynamic;
use unicode_xid::UnicodeXID;
use ustr::Ustr;

#[dynamic]
// Extend the characters allowed by UnicodeXID::is_xid_continue
static IDENTIFIER_CHARS_EXTRA: CharSet = CharSet::new()
    .add_chars("'") // Single quote
    .add_chars("′″‴") // Primes
    .add_chars("°μΩ") // Degree, micro, ohm symbols commonly used in units
    ;

#[dynamic]
static OPERATOR_CHARS: CharSet = CharSet::new()
    .add_chars("+-*/!$%^&|<>~:=@$.?")
    .add_chars("¬±×÷")
    .add_range('\u{2200}'..='\u{22FF}') // Mathematical Operators block
    .add_range('\u{2A00}'..='\u{2AFF}') // Supplemental Mathematical Operators block
    .add_range('\u{2190}'..='\u{21FF}') // Arrows block
    ;

#[derive(PartialEq, Eq)]
enum LexerState {
    StartOfLine,
    MiddleOfLine,
    StartOfDirective,
    MiddleOfDirective,
}

impl LexerState {
    fn is_directive(&self) -> bool {
        matches!(
            self,
            LexerState::StartOfDirective | LexerState::MiddleOfDirective
        )
    }
}

pub struct Lexer<'a> {
    source_id: SourceId,
    source_raw: &'a str,
    chars: PeekableN<Chars<'a>>,
    state: LexerState,
    offset: usize,
    trace_on: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(source_id: SourceId, source: &'a str) -> Lexer<'a> {
        Self::new_with_offset(source_id, source, 0, true)
    }

    pub fn new_with_offset(
        source_id: SourceId,
        source: &'a str,
        offset: usize,
        start_of_line: bool,
    ) -> Lexer<'a> {
        Lexer {
            source_id,
            source_raw: source,
            chars: PeekableN::new(source.chars(), 2),
            state: if start_of_line {
                LexerState::StartOfLine
            } else {
                LexerState::MiddleOfLine
            },
            offset,
            trace_on: std::env::var("TRACE_LEXER").is_ok(),
        }
    }

    pub fn lex(&mut self) -> LexResult<Vec<(Token, SourceSpan)>> {
        let mut tokens = Vec::new();
        while let Some(token) = self.next_token() {
            match token {
                Ok(token) => tokens.push(token),
                Err(err) => return Err(err),
            }
        }

        Ok(tokens)
    }

    fn next_token(&mut self) -> Option<LexResult<(Token, SourceSpan)>> {
        let ch = self.chars.peek().copied()?;
        let start_off = self.offset;

        if self.trace_on {
            eprintln!(
                "[TRACE_LEXER] next_token at offset {}: ch={:?}",
                start_off, ch
            );
        }

        let result = if is_decimal_digit(ch) {
            self.lex_number()
        } else if is_identifier_char_start(ch) {
            if self.state == LexerState::StartOfDirective {
                self.lex_directive()
            } else {
                self.lex_identifier_or_similar()
            }
        } else if ch == ';' {
            self.lex_line_comment()
        } else if ch == '"' {
            self.lex_string()
        } else if ch == '.' {
            if self.peek(0) == '.' && self.peek(1) == '.' {
                self.take_one().unwrap(); // .
                self.take_one().unwrap(); // .
                self.take_one().unwrap(); // .
                Ok(Token::TripleDot)
            } else if is_decimal_digit(self.peek(0)) {
                self.lex_number()
            } else {
                self.lex_operator()
            }
        } else if is_operator_char(ch) {
            self.lex_operator()
        } else if is_whitespace_char(ch) {
            self.lex_whitespace()
        } else if ch == '#' && self.state == LexerState::StartOfLine {
            self.take_one().unwrap(); // #
            self.expect_take_one(|ch| ch == '[', "expected '['");
            self.state = LexerState::StartOfDirective;
            Ok(Token::DirectiveStart)
        } else if ch == ']' && self.state == LexerState::MiddleOfDirective {
            self.take_one().unwrap(); // ]
            let next = self.peek(0);
            if next == '\n' || next == '\0' {
                self.state = LexerState::MiddleOfLine;
                Ok(Token::DirectiveEnd)
            } else {
                Err(LexError::new("expected newline", self.position()))
            }
        } else {
            let result = match ch {
                '\n' => Ok(Token::NewLine),
                ',' => Ok(Token::Comma),
                '(' => Ok(Token::LDelim("(")),
                ')' => Ok(Token::RDelim(")")),
                '[' => Ok(Token::LDelim("[")),
                ']' => Ok(Token::RDelim("]")),
                '{' => Ok(Token::LDelim("{")),
                '}' => Ok(Token::RDelim("}")),
                _ => Err(LexError::new("unexpected character", self.position())),
            };

            if result.is_ok() {
                self.take_one();
            }
            result
        };

        let end_off = self.offset;
        let span = SourceSpan::new(self.source_id, start_off, end_off);

        if self.trace_on {
            match &result {
                Ok(token) => eprintln!(
                    "[TRACE_LEXER] token: {:?} @ {}..{}",
                    token, start_off, end_off
                ),
                Err(_) => eprintln!("[TRACE_LEXER] error at {}..{}", start_off, end_off),
            }
        }

        Some(result.map(|token| (token, span)))
    }

    fn lex_number(&mut self) -> LexResult<Token> {
        // 1234_1234
        // 1234.5678
        // .1234
        // 12.25e-4
        // 10e+5
        // 0b1010_1111
        // 0x1234_1234
        // 0o7777_7777
        let mut is_int = true;
        let mut is_exp = false;
        let mut is_exp_start = false;
        let mut can_underscore = false;

        let mut value = String::new();
        let mut radix = 10i32;

        if self.peek(0) == '0' {
            match self.peek(1) {
                'b' => radix = 2,
                'o' => radix = 8,
                'x' => radix = 16,
                _ => (),
            }
            if radix != 10 {
                self.take_one().unwrap(); // 0
                self.take_one().unwrap(); // b, o, x
            }
        }

        while let Some(ch) = self.chars.peek().copied() {
            if ch == '_' && can_underscore {
                can_underscore = false;
                self.take_one();
                continue;
            }

            if ch == '.' && is_int && !is_exp {
                is_int = false;
                if radix != 10 {
                    return Err(LexError::new(
                        "unexpected character in integer",
                        self.position(),
                    ));
                }
            } else if (ch == 'e' || ch == 'E') && !is_exp {
                is_int = false;
                is_exp = true;
                is_exp_start = true;
            } else if is_exp_start && (ch == '-' || ch == '+') {
                is_exp_start = false;
            } else if !ch.is_digit(radix as u32) {
                // if ch.is_ascii_alphanumeric() {
                //     return Err(LexError::new(
                //         &format!(
                //             "unexpected character in {}",
                //             if is_int { "integer" } else { "float" }
                //         ),
                //         self.position(),
                //     ));
                // }
                break;
            } else if is_exp_start {
                is_exp_start = false;
            }

            can_underscore = true;
            value.push(self.take_one().unwrap());
        }

        if is_int {
            Ok(Token::Integer(value, radix))
        } else {
            Ok(Token::Float(value))
        }
    }

    fn lex_string(&mut self) -> LexResult<Token> {
        // "string" or """multiline"""
        self.take_one().unwrap(); // "
        let multiline = if self.peek(0) == '"' && self.peek(1) == '"' {
            self.take_one();
            self.take_one();
            true
        } else {
            false
        };

        let mut value = String::new();
        let mut parts: Vec<StringPart> = Vec::new();
        let mut saw_interpolation = false;
        let mut terminated = false;
        while let Some(ch) = self.take_one() {
            if ch == '\\' {
                let err_pos = self.position();
                let ch = self
                    .take_one()
                    .ok_or_else(|| LexError::new("unexpected end of input", self.position()))?;
                let ch = match ch {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    'a' => '\u{07}',
                    'b' => '\u{08}',
                    'f' => '\u{0C}',
                    'v' => '\u{0B}',
                    '0' => '\0',
                    '\\' => '\\',
                    '"' => '"',
                    '$' => '$',
                    'x' => {
                        let h1 = self.take_one().ok_or_else(|| {
                            LexError::new("unexpected end of input", self.position())
                        })?;
                        let h2 = self.take_one().ok_or_else(|| {
                            LexError::new("unexpected end of input", self.position())
                        })?;
                        let code = match (hex_value(h1), hex_value(h2)) {
                            (Some(a), Some(b)) => (a << 4) | b,
                            _ => return Err(LexError::new("invalid escape sequence", err_pos)),
                        };
                        char::from_u32(code as u32).unwrap()
                    }
                    'u' => {
                        let mut code: u32 = 0;
                        for shift in [12, 8, 4, 0] {
                            let h = self.take_one().ok_or_else(|| {
                                LexError::new("unexpected end of input", self.position())
                            })?;
                            let Some(val) = hex_value(h) else {
                                return Err(LexError::new("invalid escape sequence", err_pos));
                            };
                            code |= (val as u32) << shift;
                        }
                        char::from_u32(code)
                            .ok_or_else(|| LexError::new("invalid escape sequence", err_pos))?
                    }
                    _ => return Err(LexError::new("invalid escape sequence", err_pos)),
                };
                value.push(ch);
            } else if ch == '$' && self.peek(0) == '{' {
                self.take_one().unwrap(); // {
                if !value.is_empty() {
                    parts.push(StringPart::Text(std::mem::take(&mut value)));
                }
                saw_interpolation = true;

                let expr_start = self.offset;
                let mut expr = String::new();
                let mut depth = 0usize;
                let mut in_string = false;
                let mut in_triple_string = false;
                let mut in_comment = false;
                let mut string_escape = false;

                loop {
                    let mut ch = self.take_one().ok_or_else(|| {
                        LexError::new("unexpected end of input", self.position())
                    })?;
                    if ch == '\\' {
                        let err_pos = self.position();
                        let next = self
                            .take_one()
                            .ok_or_else(|| LexError::new("unexpected end of input", self.position()))?;
                        ch = match next {
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            'a' => '\u{07}',
                            'b' => '\u{08}',
                            'f' => '\u{0C}',
                            'v' => '\u{0B}',
                            '0' => '\0',
                            '\\' => '\\',
                            '"' => '"',
                            '$' => '$',
                            'x' => {
                                let h1 = self.take_one().ok_or_else(|| {
                                    LexError::new("unexpected end of input", self.position())
                                })?;
                                let h2 = self.take_one().ok_or_else(|| {
                                    LexError::new("unexpected end of input", self.position())
                                })?;
                                let code = match (hex_value(h1), hex_value(h2)) {
                                    (Some(a), Some(b)) => (a << 4) | b,
                                    _ => return Err(LexError::new("invalid escape sequence", err_pos)),
                                };
                                char::from_u32(code as u32).unwrap()
                            }
                            'u' => {
                                let mut code: u32 = 0;
                                for shift in [12, 8, 4, 0] {
                                    let h = self.take_one().ok_or_else(|| {
                                        LexError::new("unexpected end of input", self.position())
                                    })?;
                                    let Some(val) = hex_value(h) else {
                                        return Err(LexError::new("invalid escape sequence", err_pos));
                                    };
                                    code |= (val as u32) << shift;
                                }
                                char::from_u32(code)
                                    .ok_or_else(|| LexError::new("invalid escape sequence", err_pos))?
                            }
                            _ => return Err(LexError::new("invalid escape sequence", err_pos)),
                        };
                    }

                    if in_comment {
                        expr.push(ch);
                        if ch == '\n' {
                            in_comment = false;
                        }
                        continue;
                    }

                    if in_triple_string {
                        if ch == '"' && self.peek(0) == '"' && self.peek(1) == '"' {
                            in_triple_string = false;
                            expr.push(ch);
                            expr.push(self.take_one().unwrap());
                            expr.push(self.take_one().unwrap());
                            continue;
                        }
                        expr.push(ch);
                        continue;
                    }

                    if in_string {
                        if string_escape {
                            string_escape = false;
                        } else if ch == '\\' {
                            string_escape = true;
                        } else if ch == '"' {
                            in_string = false;
                        }
                        expr.push(ch);
                        continue;
                    }

                    if ch == '"' {
                        if self.peek(0) == '"' && self.peek(1) == '"' {
                            in_triple_string = true;
                            expr.push(ch);
                            expr.push(self.take_one().unwrap());
                            expr.push(self.take_one().unwrap());
                        } else {
                            in_string = true;
                            expr.push(ch);
                        }
                        continue;
                    }

                    if ch == ';' {
                        in_comment = true;
                        expr.push(ch);
                        continue;
                    }

                    if ch == '{' {
                        depth += 1;
                        expr.push(ch);
                        continue;
                    }

                    if ch == '}' {
                        if depth == 0 {
                            let expr_end = self.offset - ch.len_utf8();
                            let span = SourceSpan::new(self.source_id, expr_start, expr_end);
                            parts.push(StringPart::Expr { source: expr, span });
                            break;
                        }
                        depth -= 1;
                        expr.push(ch);
                        continue;
                    }

                    expr.push(ch);
                }
            } else if ch == '"' {
                if multiline && self.peek(0) == '"' && self.peek(1) == '"' {
                    self.take_one().unwrap();
                    self.take_one().unwrap();
                    terminated = true;
                    break;
                }
                if !multiline {
                    terminated = true;
                    break;
                }
                value.push(ch);
            } else {
                value.push(ch);
            }
        }

        if !terminated {
            return Err(LexError::new(
                if multiline {
                    "unterminated multiline string"
                } else {
                    "unterminated string"
                },
                self.position(),
            ));
        }

        if saw_interpolation {
            if !value.is_empty() {
                parts.push(StringPart::Text(value));
            }
            if multiline {
                normalize_multiline_string_parts(&mut parts);
            }
            Ok(Token::InterpolatedString(parts))
        } else if multiline {
            let mut parts = vec![StringPart::Text(value)];
            normalize_multiline_string_parts(&mut parts);
            match parts.pop() {
                Some(StringPart::Text(value)) => Ok(Token::String(value)),
                _ => Ok(Token::String(String::new())),
            }
        } else {
            Ok(Token::String(value))
        }
    }

    fn lex_directive(&mut self) -> LexResult<Token> {
        let ident = self.take_while(is_identifier_char_continue);
        self.drop_while(is_whitespace_char);

        if matches!(self.peek(0), '=' | ']') {
            Ok(Token::Directive(ident))
        } else {
            Err(LexError::new("unexpected character", self.position()))
        }
    }

    fn lex_identifier_or_similar(&mut self) -> LexResult<Token> {
        let ident = self.take_while(is_identifier_char_continue);
        let token = if let Some(keyword) = KEYWORDS.get(&ident) {
            Token::Keyword(keyword.clone())
        } else {
            match ident.as_str() {
                "nan" | "NaN" => Token::Float("NaN".to_string()),
                "inf" | "Inf" => Token::Float("Inf".to_string()),
                "true" => Token::Bool(true),
                "false" => Token::Bool(false),
                _ => Token::Identifier(Ustr::from(&ident)),
            }
        };

        Ok(token)
    }

    fn lex_operator(&mut self) -> LexResult<Token> {
        let Some(op) = self.take_if(is_operator_char) else {
            return Err(LexError::new("expected operator", self.position()));
        };

        let token = if op == ':' {
            if self.peek(0) == ':' {
                // ::
                self.take_one();
                Token::PathSep
            } else if self.peek(0) == '=' {
                // :=
                self.take_one();
                Token::RangeAssign
            } else {
                Token::Colon
            }
        } else if op == '=' {
            if self.peek(0) == '>' {
                self.take_one();
                Token::FatArrow
            } else if !is_operator_char(self.peek(0)) {
                Token::Assign
            } else {
                Token::Operator(Ustr::from(&op.to_string()))
            }
        } else if op == '&' {
            Token::Ampersand
        } else {
            Token::Operator(Ustr::from(&op.to_string()))
        };
        Ok(token)
    }

    fn lex_whitespace(&mut self) -> LexResult<Token> {
        // one or more spaces
        self.drop_while(is_whitespace_char);
        Ok(Token::Space)
    }

    fn lex_line_comment(&mut self) -> LexResult<Token> {
        // ; line comment
        self.expect_take_one(|ch| ch == ';', "expected ';'")?;

        let comment = self.take_while(|ch| ch != '\n');
        Ok(Token::Comment(comment))
    }
}

impl<'a> Lexer<'a> {
    fn position(&self) -> SourcePos {
        SourcePos::new(self.source_id, self.offset)
    }

    fn peek(&self, n: usize) -> char {
        self.chars.peek_n(n).unwrap_or('\0')
    }

    fn take_one(&mut self) -> Option<char> {
        let Some(ch) = self.chars.next() else {
            return None;
        };

        self.offset += ch.len_utf8();
        if ch == '\n' {
            self.state = LexerState::StartOfLine;
        } else if self.state == LexerState::StartOfLine && !is_whitespace_char(ch) {
            self.state = LexerState::MiddleOfLine;
        } else if self.state == LexerState::StartOfDirective {
            self.state = LexerState::MiddleOfDirective;
        }
        Some(ch)
    }

    fn take_if<F>(&mut self, f: F) -> Option<char>
    where
        F: Fn(char) -> bool,
    {
        let ch = self.chars.peek().copied()?;
        if f(ch) {
            self.take_one()
        } else {
            None
        }
    }

    fn take_while<F>(&mut self, mut f: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut result = String::new();
        while let Some(ch) = self.chars.peek().copied() {
            if f(ch) {
                result.push(self.take_one().unwrap());
            } else {
                break;
            }
        }

        result
    }

    fn drop_while<F>(&mut self, mut f: F)
    where
        F: Fn(char) -> bool,
    {
        while let Some(ch) = self.chars.peek().copied() {
            if f(ch) {
                self.take_one().unwrap();
            } else {
                break;
            }
        }
    }

    fn expect_take_one<F>(&mut self, f: F, expected: &str) -> LexResult<char>
    where
        F: Fn(char) -> bool,
    {
        let ch = self.take_one().ok_or_else(|| {
            LexError::new(
                &format!("unexpected end of input: {}", expected),
                self.position(),
            )
        })?;

        if !f(ch) {
            return Err(LexError::new(expected, self.position()));
        }

        Ok(ch)
    }
}

fn is_decimal_digit(ch: char) -> bool {
    ch.is_digit(10)
}

fn normalize_multiline_string_parts(parts: &mut Vec<StringPart>) {
    strip_first_newline(parts);
    let min_indent = min_indentation(parts);
    if min_indent > 0 {
        strip_indentation(parts, min_indent);
    }
    parts.retain(|part| match part {
        StringPart::Text(text) => !text.is_empty(),
        StringPart::Expr { .. } => true,
    });
}

fn strip_first_newline(parts: &mut Vec<StringPart>) {
    let mut idx = 0;
    while idx < parts.len() {
        match &mut parts[idx] {
            StringPart::Text(text) => {
                if text.is_empty() {
                    idx += 1;
                    continue;
                }
                if text.starts_with('\n') {
                    text.remove(0);
                }
                if text.is_empty() {
                    parts.remove(idx);
                }
                break;
            }
            StringPart::Expr { .. } => break,
        }
    }
}

fn min_indentation(parts: &[StringPart]) -> usize {
    let mut min_indent: Option<usize> = None;
    let mut line_indent = 0usize;
    let mut at_line_start = true;
    let mut line_has_content = false;

    for part in parts {
        match part {
            StringPart::Text(text) => {
                for ch in text.chars() {
                    if at_line_start {
                        if ch == '\n' {
                            finish_line(
                                &mut min_indent,
                                &mut line_indent,
                                &mut line_has_content,
                                &mut at_line_start,
                            );
                            continue;
                        }
                        if is_indent_char(ch) {
                            line_indent += 1;
                            continue;
                        }
                        line_has_content = true;
                        at_line_start = false;
                    }

                    if ch == '\n' {
                        finish_line(
                            &mut min_indent,
                            &mut line_indent,
                            &mut line_has_content,
                            &mut at_line_start,
                        );
                    } else {
                        line_has_content = true;
                        at_line_start = false;
                    }
                }
            }
            StringPart::Expr { .. } => {
                if at_line_start {
                    line_has_content = true;
                    at_line_start = false;
                } else {
                    line_has_content = true;
                }
            }
        }
    }

    if line_has_content {
        let value = line_indent;
        min_indent = Some(min_indent.map_or(value, |min| min.min(value)));
    }

    min_indent.unwrap_or(0)
}

fn finish_line(
    min_indent: &mut Option<usize>,
    line_indent: &mut usize,
    line_has_content: &mut bool,
    at_line_start: &mut bool,
) {
    if *line_has_content {
        let value = *line_indent;
        *min_indent = Some(min_indent.map_or(value, |min| min.min(value)));
    }
    *line_indent = 0;
    *line_has_content = false;
    *at_line_start = true;
}

fn strip_indentation(parts: &mut Vec<StringPart>, indent: usize) {
    let mut at_line_start = true;
    let mut remaining = indent;

    for part in parts.iter_mut() {
        match part {
            StringPart::Text(text) => {
                let mut out = String::new();
                for ch in text.chars() {
                    if at_line_start {
                        if ch == '\n' {
                            out.push(ch);
                            remaining = indent;
                            continue;
                        }
                        if remaining > 0 && is_indent_char(ch) {
                            remaining -= 1;
                            continue;
                        }
                        at_line_start = false;
                        out.push(ch);
                    } else {
                        out.push(ch);
                        if ch == '\n' {
                            at_line_start = true;
                            remaining = indent;
                        }
                    }
                }
                *text = out;
            }
            StringPart::Expr { .. } => {
                if at_line_start {
                    at_line_start = false;
                    remaining = indent;
                }
            }
        }
    }
}

fn is_indent_char(ch: char) -> bool {
    ch == ' ' || ch == '\t'
}

fn hex_value(ch: char) -> Option<u8> {
    ch.to_digit(16).map(|d| d as u8)
}

fn is_identifier_char_start(ch: char) -> bool {
    UnicodeXID::is_xid_start(ch) || ch == '_' || IDENTIFIER_CHARS_EXTRA.contains(ch)
}

fn is_identifier_char_continue(ch: char) -> bool {
    UnicodeXID::is_xid_continue(ch) || IDENTIFIER_CHARS_EXTRA.contains(ch)
}

fn is_operator_char(ch: char) -> bool {
    OPERATOR_CHARS.contains(ch)
}

fn is_whitespace_char(ch: char) -> bool {
    // treat newlines as distinct from whitespace
    ch.is_whitespace() && ch != '\n'
}
