use std::collections::HashSet;
use std::ops::RangeInclusive;

#[derive(Clone, Debug)]
pub struct CharSet {
    ranges: Vec<RangeInclusive<char>>,
    chars: HashSet<char>,
}

impl CharSet {
    pub fn new() -> CharSet {
        CharSet {
            ranges: Vec::new(),
            chars: HashSet::new(),
        }
    }

    pub fn add_range(mut self, range: RangeInclusive<char>) -> Self {
        self.ranges.push(range);
        self
    }

    pub fn add_char(mut self, c: char) -> Self {
        self.chars.insert(c);
        self
    }

    pub fn add_chars(mut self, chars: &str) -> Self {
        for c in chars.chars() {
            self.chars.insert(c);
        }
        self
    }

    pub fn contains(&self, c: char) -> bool {
        if self.chars.contains(&c) {
            return true;
        }

        for range in &self.ranges {
            if range.contains(&c) {
                return true;
            }
        }
        false
    }
}
