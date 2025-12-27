use crate::source::Spanned;
use crate::runtime::{DeclError, Function};

use std::collections::HashMap;
use ustr::Ustr;

#[derive(Clone, Debug)]
pub struct UserTypeDef {
    pub name: Spanned<Ustr>,
    methods: HashMap<Ustr, Function>,
    constructor: Option<Function>,
}

impl UserTypeDef {
    pub fn new(name: Spanned<Ustr>) -> Self {
        Self {
            name,
            methods: HashMap::new(),
            constructor: None,
        }
    }

    pub fn clone_with_name(&self, name: Spanned<Ustr>) -> Self {
        Self {
            name,
            methods: self.methods.clone(),
            constructor: self.constructor.clone(),
        }
    }

    pub fn register_method(&mut self, func: Function) -> Result<(), DeclError> {
        if let Some(existing) = self.methods.get(&func.name.raw) {
            return Err(DeclError::new(
                "type method",
                func.name.to_string_inner(),
                existing.name.span(),
            ));
        }
        self.methods.insert(func.name.raw, func);
        Ok(())
    }

    pub fn register_constructor(&mut self, func: Function) -> Result<(), DeclError> {
        if let Some(existing) = &self.constructor {
            return Err(DeclError::new(
                "constructor",
                func.name.to_string_inner(),
                existing.name.span(),
            ));
        }
        self.constructor = Some(func);
        Ok(())
    }

    pub fn get_method(&self, name: Ustr) -> Option<Function> {
        self.methods.get(&name).cloned()
    }

    pub fn get_constructor(&self) -> Option<Function> {
        self.constructor.clone()
    }

    pub fn method_names(&self) -> Vec<Ustr> {
        let mut names = self.methods.keys().cloned().collect::<Vec<_>>();
        names.sort();
        names
    }
}
