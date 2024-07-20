use super::{source_id, SourceFile, SourceId, SourceLoc, SourceRef, SourceSpan};

use std::collections::{BTreeMap, HashMap};
use std::io;
use std::ops::Index;

/// A map of program sources that can be indexed by spans.
#[derive(Debug, Clone)]
pub struct SourceMap {
    sources: BTreeMap<SourceId, SourceFile>,
    names: HashMap<String, SourceId>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self {
            names: HashMap::new(),
            sources: BTreeMap::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.sources.len()
    }

    pub fn get(&self, id: SourceId) -> Option<&SourceFile> {
        self.sources.get(&id)
    }

    pub fn get_by_name(&self, name: &str) -> Option<&SourceFile> {
        self.names.get(name).and_then(|id| self.get(*id))
    }

    pub fn load_source(&mut self, file_name: &str) -> std::io::Result<SourceId> {
        let source = SourceFile::from_file(file_name)?;
        let id = source.id();
        self.sources.insert(id, source);
        Ok(id)
    }

    pub fn add_source(&mut self, name: String, source: String) -> SourceId {
        let id = source_id::next();
        let file = SourceFile::new(id, name, source);
        self.sources.insert(id, file);
        id
    }

    pub fn lookup_span(&self, span: SourceSpan) -> Option<SourceRef> {
        let file = self.sources.get(&span.source_id)?;
        Some(SourceRef::new(file, span.start, span.end))
    }
}

impl Default for SourceMap {
    fn default() -> Self {
        Self::new()
    }
}

impl Index<SourceId> for SourceMap {
    type Output = SourceFile;

    fn index(&self, id: SourceId) -> &Self::Output {
        self.get(id).expect("invalid source id")
    }
}
