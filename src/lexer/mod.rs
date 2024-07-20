mod charset;
mod lexer;
pub mod token;

use crate::diag::{Error, IntoError};
use crate::runtime::Context;
use crate::source::{SourceFile, SourceId, SourcePos, SourceSpan};
pub use token::{Keyword, Token};

pub type LexResult<T> = Result<T, LexError>;

// MARK: LexError

pub struct LexError {
    pub msg: String,
    pub pos: SourcePos,
}

impl LexError {
    pub fn new(msg: &str, pos: SourcePos) -> LexError {
        LexError {
            msg: String::from(msg),
            pos,
        }
    }
}

impl IntoError for LexError {
    fn into_error(self) -> Error {
        Error::new(self.msg, self.pos.as_span())
    }
}

pub fn lex(source_id: SourceId, source_raw: &str) -> Result<Vec<(Token, SourceSpan)>, LexError> {
    match lexer::Lexer::new(source_id, source_raw).lex() {
        Ok(tokens) => {
            let tokens = TokenFilter::new(tokens.into_iter(), |(t, s)| {
                if matches!(t, Token::Comment(_)) {
                    //Some((Token::Space, s))
                    None
                } else {
                    Some((t, s))
                }
            })
            .collect::<Vec<_>>();
            Ok(tokens)
        }
        Err(err) => Err(err),
    }
}

// MARK: TokenFilter

pub struct TokenFilter<I, F, T>
where
    I: Iterator<Item = T>,
    F: FnMut(T) -> Option<T>,
{
    iter: I,
    pred: F,
}

impl<I, F, T> TokenFilter<I, F, T>
where
    I: Iterator<Item = T>,
    F: FnMut(T) -> Option<T>,
{
    pub fn new(iter: I, pred: F) -> Self {
        TokenFilter { iter, pred }
    }
}

impl<I, F, T> Iterator for TokenFilter<I, F, T>
where
    I: Iterator<Item = T>,
    F: FnMut(T) -> Option<T>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(item) = self.iter.next() {
            if let Some(filtered_item) = (self.pred)(item) {
                return Some(filtered_item);
            }
        }
        None
    }
}

// MARK: iter

pub mod iter {
    use std::collections::VecDeque;
    use std::iter::Peekable;

    /// An iterator that buffers the next `n` items, allowing peeking at the `n`th item.
    pub struct PeekableN<I: Iterator> {
        iter: Peekable<I>,
        buffer: VecDeque<I::Item>,
        n: usize,
    }

    impl<I> PeekableN<I>
    where
        I: Iterator,
        I::Item: Clone,
    {
        pub fn new(iter: I, n: usize) -> Self {
            let mut iter = iter.peekable();
            let mut buffer = VecDeque::new();

            for _ in 0..n {
                if let Some(item) = iter.next() {
                    buffer.push_back(item);
                }
            }

            PeekableN { iter, buffer, n }
        }

        pub fn peek(&mut self) -> Option<&I::Item> {
            self.buffer.front()
        }

        pub fn peek_n(&self, n: usize) -> Option<I::Item> {
            if n < self.n {
                self.buffer.get(n).cloned()
            } else {
                None
            }
        }
    }

    impl<I> Iterator for PeekableN<I>
    where
        I: Iterator,
        I::Item: Clone,
    {
        type Item = I::Item;

        fn next(&mut self) -> Option<Self::Item> {
            if let Some(next_item) = self.iter.next() {
                self.buffer.push_back(next_item);
                if self.buffer.len() > self.n {
                    self.buffer.pop_front()
                } else {
                    None
                }
            } else {
                self.buffer.pop_front()
            }
        }
    }
}
