use crate::print::ansi::{BOLD, DELIM, DIRECTIVE, KEYWORD, NUMBER, OPERATOR, PUNCT, RESET, STRING};
use crate::print::PrettyPrint;
use crate::source::SourceSpan;

use phf::phf_map;
use std::fmt::Display;
use ustr::Ustr;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Integer(String, i32 /* radix */),
    Float(String),
    Bool(bool),
    String(String),
    Keyword(Keyword),
    Operator(Ustr),
    Identifier(Ustr),
    Comment(String),
    Directive(String),
    LDelim(&'static str),
    RDelim(&'static str),
    DirectiveStart, // #[
    DirectiveEnd,   // ]
    Assign,         // =
    RangeAssign,    // :=
    PathSep,        // ::
    Colon,          // :
    Comma,          // ,
    Space,          // ␣
    NewLine,        // \n
    EndOfFile,
}

impl Token {
    pub fn operator(s: &str) -> Self {
        Token::Operator(Ustr::from(s))
    }

    pub fn identifier(s: &str) -> Self {
        Token::Identifier(Ustr::from(s))
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Token::Integer(_, _) | Token::Float(_))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Token::Bool(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Token::String(_))
    }

    pub fn is_directive(&self) -> bool {
        matches!(self, Token::Directive(_))
    }

    pub fn is_directive_start(&self) -> bool {
        matches!(self, Token::DirectiveStart)
    }

    pub fn is_directive_end(&self) -> bool {
        matches!(self, Token::DirectiveEnd)
    }

    pub fn is_operator(&self) -> bool {
        matches!(self, Token::Operator(_) | Token::Assign | Token::Colon)
    }

    pub fn is_identifier(&self) -> bool {
        matches!(self, Token::Identifier(_))
    }

    pub fn is_eol(&self) -> bool {
        matches!(self, Token::NewLine | Token::EndOfFile)
    }

    pub fn is_eof(&self) -> bool {
        matches!(self, Token::EndOfFile)
    }

    pub fn get_operator(&self) -> Option<Ustr> {
        match self {
            Token::Operator(op) => Some(op.clone()),
            Token::Assign => Some(Ustr::from("=")),
            Token::Comma => Some(Ustr::from(",")),
            _ => None,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Integer(s, radix) => write!(f, "Integer({}#{})", s, radix),
            Token::Float(s) => write!(f, "{}", s),
            Token::Bool(b) => write!(f, "{}", b),
            Token::String(s) => write!(f, "{}", s),
            Token::Keyword(k) => write!(f, "{:?}", k),
            Token::Operator(op) => write!(f, "{}", op),
            Token::Identifier(s) => write!(f, "{}", s),
            Token::Comment(c) => write!(f, "{}", c),
            Token::Directive(d) => write!(f, "{:?}", d),
            Token::LDelim(s) => write!(f, "{}", s),
            Token::RDelim(s) => write!(f, "{}", s),
            Token::DirectiveStart => write!(f, "#["),
            Token::DirectiveEnd => write!(f, "]"),
            Token::Assign => write!(f, "="),
            Token::RangeAssign => write!(f, ":="),
            Token::PathSep => write!(f, "::"),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::Space => write!(f, " "),
            Token::NewLine => write!(f, "\n"),
            Token::EndOfFile => write!(f, ""),
        }
    }
}

impl<Ctx> PrettyPrint<Ctx> for Token {
    fn pretty_print<Output: std::io::Write>(
        &self,
        out: &mut Output,
        _: &Ctx,
        level: usize,
    ) -> std::io::Result<()> {
        match self {
            Token::Integer(s, radix) => write!(out, "Integer {NUMBER}{}{RESET}", s),
            Token::Float(s) => write!(out, "Float {NUMBER}{}{RESET}", s),
            Token::Bool(b) => write!(out, "Bool {NUMBER}{}{RESET}", b),
            Token::String(s) => write!(out, "String {STRING}{}{RESET}", s),
            Token::Keyword(k) => write!(out, "Keyword {KEYWORD}{:?}{RESET}", k),
            Token::Operator(op) => write!(out, "Operator {OPERATOR}{}{RESET}", op),
            Token::Identifier(s) => write!(out, "Identifier {KEYWORD}{}{RESET}", s),
            Token::Comment(_) => write!(out, "Comment {PUNCT}Comment{RESET}"),
            Token::Directive(d) => write!(out, "Directive {DIRECTIVE}{:?}{RESET}", d),
            Token::LDelim(s) => write!(out, "LDelim {DELIM}{}{RESET}", s),
            Token::RDelim(s) => write!(out, "RDelim {DELIM}{}{RESET}", s),
            Token::DirectiveStart => write!(out, "DirectiveStart {PUNCT}#[{RESET}"),
            Token::DirectiveEnd => write!(out, "DirectiveEnd {PUNCT}] {RESET}"),
            Token::Assign => write!(out, "Assign {OPERATOR}={RESET}"),
            Token::RangeAssign => write!(out, "RangeAssign {OPERATOR}:={RESET}"),
            Token::PathSep => write!(out, "PathSep {OPERATOR}::{RESET}"),
            Token::Colon => write!(out, "Colon {PUNCT}:{RESET}"),
            Token::Comma => write!(out, "Comma {PUNCT},{RESET}"),
            Token::Space => write!(out, "{BOLD}Space{RESET}"),
            Token::NewLine => write!(out, "{BOLD}Newline{RESET}"),
            Token::EndOfFile => Ok(()),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Keyword {
    Base,
    Const,
    Dimension,
    Else,
    Fn,
    For,
    If,
    Import,
    Infix,
    Operator,
    Postfix,
    Prefix,
    Unit,
}

impl Keyword {
    pub fn is_op_decl(&self) -> bool {
        matches!(self, Keyword::Infix | Keyword::Postfix | Keyword::Prefix)
    }
}

pub static KEYWORDS: phf::Map<&'static str, Keyword> = phf_map! {
    "base" => Keyword::Base,
    "const" => Keyword::Const,
    "dimension" => Keyword::Dimension,
    "else" => Keyword::Else,
    "fn" => Keyword::Fn,
    "for" => Keyword::For,
    "if" => Keyword::If,
    "import" => Keyword::Import,
    "infix" => Keyword::Infix,
    "operator" => Keyword::Operator,
    "postfix" => Keyword::Postfix,
    "prefix" => Keyword::Prefix,
    "unit" => Keyword::Unit,
};
