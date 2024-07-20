use super::charset::CharSet;
use super::iter::PeekableN;
use super::token::{Token, KEYWORDS};
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
    ;

#[dynamic]
static OPERATOR_CHARS: CharSet = CharSet::new()
    .add_chars("+-*/!$%^&|<>~:=@$.")
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
}

impl<'a> Lexer<'a> {
    pub fn new(source_id: SourceId, source: &'a str) -> Lexer<'a> {
        Lexer {
            source_id,
            source_raw: source,
            chars: PeekableN::new(source.chars(), 2),
            state: LexerState::StartOfLine,
            offset: 0,
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

        let result = if ch.is_digit(10) {
            self.lex_number()
        } else if is_identifier_char_start(ch) {
            if self.state == LexerState::StartOfDirective {
                self.lex_directive()
            } else {
                self.lex_identifier()
            }
        } else if ch == '/' {
            match self.peek(1) {
                '/' => self.lex_line_comment(),
                '*' => self.lex_block_comment(),
                _ => self.lex_operator(),
            }
        } else if ch == '"' {
            self.lex_string()
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
            if self.peek(0) == '\n' {
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
        Some(result.map(|token| (token, span)))
    }

    fn lex_number(&mut self) -> LexResult<Token> {
        // 1234_1234
        // 1234.5678
        // 12.25e-4
        // 10e+5
        // 0b1010_1111
        // 0x1234_1234
        // 0o7777_7777
        let mut value = String::new();
        let mut radix = 10i32;
        if self.peek(0) == '0' {
            match self.peek(1) {
                'b' => radix = 2,
                'o' => radix = 8,
                'x' => radix = 16,
                _ => value.push(self.take_one().unwrap()),
            }
        }

        if radix != 10 {
            self.take_one().unwrap(); // 0
            self.take_one().unwrap(); // b, o, x
        }

        let mut is_int = true;
        let mut is_exp = false;
        let mut is_exp_start = false;
        while let Some(ch) = self.chars.peek().copied() {
            if ch == '_' {
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
                if ch.is_ascii_alphanumeric() {
                    return Err(LexError::new(
                        &format!(
                            "unexpected character in {}",
                            if is_int { "integer" } else { "float" }
                        ),
                        self.position(),
                    ));
                }
                break;
            } else if is_exp_start {
                is_exp_start = false;
            }

            value.push(self.take_one().unwrap());
        }

        if is_int {
            Ok(Token::Integer(value, radix))
        } else {
            Ok(Token::Float(value))
        }
    }

    fn lex_string(&mut self) -> LexResult<Token> {
        // "string"
        self.take_one().unwrap(); // "

        let mut value = String::new();
        while let Some(ch) = self.take_one() {
            if ch == '\\' {
                let ch = self
                    .take_one()
                    .ok_or_else(|| LexError::new("unexpected end of input", self.position()))?;
                let ch = match ch {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\\' => '\\',
                    '"' => '"',
                    _ => return Err(LexError::new("invalid escape sequence", self.position())),
                };
                value.push(ch);
            } else if ch == '"' {
                break;
            } else {
                value.push(ch);
            }
        }

        Ok(Token::String(value))
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

    fn lex_identifier(&mut self) -> LexResult<Token> {
        let ident = self.take_while(is_identifier_char_continue);
        let token = if let Some(keyword) = KEYWORDS.get(&ident) {
            Token::Keyword(keyword.clone())
        } else {
            Token::Identifier(Ustr::from(&ident))
        };

        Ok(token)
    }

    fn lex_operator(&mut self) -> LexResult<Token> {
        let Some(op) = self.take_if(is_operator_char) else {
            return Err(LexError::new("expected operator", self.position()));
        };

        let token = if op == ':' {
            if self.peek(0) == ':' {
                self.take_one();
                Token::PathOp
            } else {
                Token::Colon
            }
        } else if op == '=' && !is_operator_char(self.peek(0)) {
            Token::AssignOp
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
        // // line comment
        self.expect_take_one(|ch| ch == '/', "expected '/'")?;
        self.expect_take_one(|ch| ch == '/', "expected '/'")?;

        let comment = self.take_while(|ch| ch != '\n');
        Ok(Token::Comment(comment))
    }

    fn lex_block_comment(&mut self) -> LexResult<Token> {
        // /* block
        //    comment */
        self.expect_take_one(|ch| ch == '/', "expected '/'")?;
        self.expect_take_one(|ch| ch == '*', "expected '*'")?;

        let mut comment = String::new();
        while let Some(ch) = self.take_one() {
            if ch == '*' && self.peek(0) == '/' {
                self.take_one();
                break;
            }

            comment.push(ch);
        }

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
        } else if self.state == LexerState::StartOfLine {
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

fn is_identifier_char_start(ch: char) -> bool {
    UnicodeXID::is_xid_start(ch)
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
