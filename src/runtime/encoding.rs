use super::{Exception, Function};

use std::collections::HashMap;
use ustr::Ustr;

/// Registry for encoding implementations keyed by name.
#[derive(Default, Clone, Debug)]
pub struct EncodingRegistry {
    encodings: HashMap<Ustr, (Function, Function)>,
}

impl EncodingRegistry {
    pub fn new() -> Self {
        Self {
            encodings: HashMap::new(),
        }
    }

    /// Register an encoding implementation under a name.
    pub fn register(
        &mut self,
        name: Ustr,
        encode_fn: Function,
        decode_fn: Function,
    ) -> Result<(), Exception> {
        self.encodings.insert(name, (encode_fn, decode_fn));
        Ok(())
    }

    /// Look up an encoding implementation.
    pub fn get(&self, name: &Ustr) -> Option<&(Function, Function)> {
        self.encodings.get(name)
    }
}
