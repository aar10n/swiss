use std::cell::RefCell;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Clone)]
pub struct SharedStr {
    inner: Rc<SharedStrInner>,
    start: usize,
    end: usize,
}

struct SharedStrInner {
    data: Rc<str>,
    char_index: RefCell<Option<Rc<Vec<usize>>>>,
}

impl SharedStr {
    pub fn from_string(value: String) -> Self {
        let data: Rc<str> = Rc::from(value);
        let end = data.len();
        Self {
            inner: Rc::new(SharedStrInner {
                data,
                char_index: RefCell::new(None),
            }),
            start: 0,
            end,
        }
    }

    pub fn from_str(value: &str) -> Self {
        Self::from_string(value.to_string())
    }

    pub fn as_str(&self) -> &str {
        &self.inner.data[self.start..self.end]
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    pub fn len_chars(&self) -> usize {
        let offsets = self.char_offsets();
        match self.slice_char_bounds(&offsets) {
            Some((start, end)) => end - start,
            None => self.as_str().chars().count(),
        }
    }

    pub fn slice_chars(&self, start: usize, end: usize) -> Self {
        if let Some((slice_start, slice_end)) = self.slice_char_bounds(&self.char_offsets()) {
            let slice_len = slice_end - slice_start;
            debug_assert!(start <= end && end <= slice_len);

            let start_abs = slice_start + start;
            let end_abs = slice_start + end;
            let byte_start = self.char_index_to_byte(start_abs);
            let byte_end = self.char_index_to_byte(end_abs);
            return self.slice_bytes_absolute(byte_start, byte_end);
        }

        self.slice_chars_slow(start, end)
    }

    pub fn slice_bytes_relative(&self, start: usize, end: usize) -> Self {
        let start_abs = self.start + start;
        let end_abs = self.start + end;
        self.slice_bytes_absolute(start_abs, end_abs)
    }

    pub fn slice_bytes_absolute(&self, start: usize, end: usize) -> Self {
        debug_assert!(start <= end);
        debug_assert!(start >= self.start && end <= self.end);
        Self {
            inner: self.inner.clone(),
            start,
            end,
        }
    }

    fn char_offsets(&self) -> Rc<Vec<usize>> {
        if let Some(existing) = self.inner.char_index.borrow().clone() {
            return existing;
        }

        let mut offsets = Vec::new();
        for (idx, _) in self.inner.data.char_indices() {
            offsets.push(idx);
        }

        let offsets = Rc::new(offsets);
        *self.inner.char_index.borrow_mut() = Some(offsets.clone());
        offsets
    }

    fn slice_char_bounds(&self, offsets: &[usize]) -> Option<(usize, usize)> {
        let data_len = self.inner.data.len();
        let start_idx = Self::byte_to_char_index(offsets, data_len, self.start)?;
        let end_idx = Self::byte_to_char_index(offsets, data_len, self.end)?;
        Some((start_idx, end_idx))
    }

    fn char_index_to_byte(&self, char_index: usize) -> usize {
        let offsets = self.char_offsets();
        let data_len = self.inner.data.len();
        if char_index == offsets.len() {
            data_len
        } else {
            offsets[char_index]
        }
    }

    fn byte_to_char_index(offsets: &[usize], data_len: usize, byte: usize) -> Option<usize> {
        if byte == data_len {
            return Some(offsets.len());
        }
        offsets.binary_search(&byte).ok()
    }

    fn slice_chars_slow(&self, start: usize, end: usize) -> Self {
        let mut start_byte = None;
        let mut end_byte = None;
        if start == 0 {
            start_byte = Some(0);
        }
        if end == 0 {
            end_byte = Some(0);
        }
        let mut count = 0usize;
        for (idx, _) in self.as_str().char_indices() {
            if count == start {
                start_byte = Some(idx);
            }
            if count == end {
                end_byte = Some(idx);
                break;
            }
            count += 1;
        }

        let slice_len = self.as_str().len();
        let start_byte = start_byte.unwrap_or(slice_len);
        let end_byte = end_byte.unwrap_or(slice_len);
        self.slice_bytes_relative(start_byte, end_byte)
    }
}

impl Default for SharedStr {
    fn default() -> Self {
        SharedStr::from_str("")
    }
}

impl Deref for SharedStr {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl From<String> for SharedStr {
    fn from(value: String) -> Self {
        SharedStr::from_string(value)
    }
}

impl From<&str> for SharedStr {
    fn from(value: &str) -> Self {
        SharedStr::from_str(value)
    }
}

impl PartialEq for SharedStr {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for SharedStr {}

impl fmt::Debug for SharedStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.as_str())
    }
}

impl fmt::Display for SharedStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
