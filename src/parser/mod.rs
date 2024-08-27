mod parser;

use crate::ast::{Module, OpAssoc};
use crate::diag::{Error, IntoError};
use crate::lexer::Token;
use crate::runtime::{self, Context, DeclError, DirectiveError, NameError, ValueError};
use crate::source::{SourceId, SourcePos, SourceSpan};

pub type ParseResult<T> = Result<T, ParseError>;

/// An error that can occur during parsing.
#[derive(Debug)]
pub enum ParseError {
    DeclError(DeclError),
    DirectiveError(DirectiveError),
    NameError(NameError),
    SyntaxError(SyntaxError),
    ValueError(ValueError),
}

impl IntoError for ParseError {
    fn into_error(self) -> Error {
        match self {
            ParseError::DeclError(err) => err.into_error(),
            ParseError::DirectiveError(err) => err.into_error(),
            ParseError::NameError(err) => err.into_error(),
            ParseError::SyntaxError(err) => err.into_error(),
            ParseError::ValueError(err) => err.into_error(),
        }
    }
}

macro_rules! impl_from_error {
    ($error:ident) => {
        impl From<$error> for ParseError {
            fn from(err: $error) -> Self {
                ParseError::$error(err)
            }
        }
    };
}

impl_from_error!(DeclError);
impl_from_error!(DirectiveError);
impl_from_error!(NameError);
impl_from_error!(SyntaxError);
impl_from_error!(ValueError);

/// A syntax error.
#[derive(Debug)]
pub struct SyntaxError {
    pub msg: String,
    pub pos: SourcePos,
}

impl SyntaxError {
    pub fn new<S: ToString>(msg: S, pos: SourcePos) -> SyntaxError {
        SyntaxError {
            msg: msg.to_string(),
            pos,
        }
    }
}

impl IntoError for SyntaxError {
    fn into_error(self) -> Error {
        Error::new(format!("SyntaxError: {}", self.msg), self.pos.as_span())
    }
}

/// Configuration options for the parser.
pub struct ParserConfig {
    // The current associativity for operators.
    pub associativity: OpAssoc,
    // The current precedence for operators.
    pub precedence: isize,
}

impl Default for ParserConfig {
    fn default() -> Self {
        Self {
            associativity: OpAssoc::Left,
            precedence: 0,
        }
    }
}

pub fn parse(module: &mut runtime::Module, tokens: &[(Token, SourceSpan)]) -> ParseResult<Module> {
    let mut parser = parser::Parser::new(module, tokens);
    parser.parse()
}
