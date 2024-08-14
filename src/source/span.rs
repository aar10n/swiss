use super::{SourceId, SourceLoc, SourcePos};

use std::fmt::{Debug, Display};
use std::ops::{Deref, DerefMut};
use ustr::Ustr;

// MARK: SourceSpan

/// A span in a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceSpan {
    pub source_id: SourceId,
    pub start: usize,
    pub end: usize,
}

impl SourceSpan {
    pub const INVALID: SourceSpan = SourceSpan::new(SourceId::INVALID, 0, 0);

    pub const fn new(source_id: SourceId, start: usize, end: usize) -> Self {
        Self {
            source_id,
            start,
            end,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    pub fn is_invalid(&self) -> bool {
        self.source_id == SourceId::INVALID
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn start_pos(&self) -> SourcePos {
        SourcePos::new(self.source_id, self.start)
    }

    pub fn end_pos(&self) -> SourcePos {
        SourcePos::new(self.source_id, self.end)
    }

    pub fn from_end(&self) -> Self {
        Self {
            source_id: self.source_id,
            start: self.end,
            end: self.end,
        }
    }

    pub fn into_spanned<T>(self, value: T) -> Spanned<T> {
        Spanned {
            raw: value,
            span: self,
        }
    }

    pub fn hash(&self) -> u64 {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        let mut hasher = DefaultHasher::new();
        Hash::hash(&self, &mut hasher);
        hasher.finish()
    }
}

impl Default for SourceSpan {
    fn default() -> Self {
        Self {
            source_id: SourceId::INVALID,
            start: 0,
            end: 0,
        }
    }
}

// MARK: Spanned

/// A wrapper type that adds a span to a raw value.
#[derive(Clone, PartialEq)]
pub struct Spanned<T> {
    pub raw: T,
    pub span: SourceSpan,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: SourceSpan) -> Self {
        Self { raw: value, span }
    }

    pub fn map<U: Clone, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U> {
        Spanned {
            span: self.span,
            raw: f(self.raw),
        }
    }

    pub fn map_ref<U: Clone, F: FnOnce(&T) -> U>(&self, f: F) -> Spanned<U> {
        Spanned {
            span: self.span,
            raw: f(&self.raw),
        }
    }

    pub fn map_ref_mut<U: Clone, F: FnOnce(&mut T) -> U>(&mut self, f: F) -> Spanned<U> {
        Spanned {
            span: self.span,
            raw: f(&mut self.raw),
        }
    }

    pub fn span(&self) -> SourceSpan {
        self.span
    }

    pub fn value(&self) -> &T {
        &self.raw
    }

    pub fn value_mut(&mut self) -> &mut T {
        &mut self.raw
    }

    pub fn into_value(self) -> T {
        self.raw
    }

    pub fn into_pair(self) -> (T, SourceSpan) {
        (self.raw, self.span)
    }
}

impl<T: ToString> Spanned<T> {
    pub fn to_string_inner(&self) -> Spanned<String> {
        Spanned {
            span: self.span,
            raw: self.raw.to_string(),
        }
    }
}

impl<T: AsRef<str>> Spanned<T> {
    pub fn to_ustr_inner(&self) -> Spanned<Ustr> {
        Spanned {
            span: self.span,
            raw: Ustr::from(self.as_ref()),
        }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.raw
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.raw
    }
}

impl<T: AsRef<str>> From<T> for Spanned<String> {
    fn from(value: T) -> Self {
        Spanned {
            raw: value.as_ref().to_string(),
            span: SourceSpan::default(),
        }
    }
}

impl<T: AsRef<str>> From<T> for Spanned<Ustr> {
    fn from(value: T) -> Self {
        Spanned {
            raw: Ustr::from(value.as_ref()),
            span: SourceSpan::default(),
        }
    }
}

// impl From<&str> for Spanned<String> {
//     fn from(value: &str) -> Self {
//         Spanned {
//             raw: value.to_string(),
//             span: SourceSpan::default(),
//         }
//     }
// }

impl<T: Copy> Copy for Spanned<T> {}

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.raw, f)
    }
}

impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.raw, f)
    }
}
