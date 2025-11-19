use super::{Context, Exception, Function, Ty};
use crate::{ast::Spanned, print::PrettyString};

use std::{collections::HashMap, f32::consts::E};

use phf::OrderedMap;
use ustr::{Ustr, UstrMap, UstrSet};

// MARK: Interface

#[derive(Debug, Clone)]
pub struct Interface {
    pub name: Spanned<Ustr>,
    pub functions: Vec<Function>,
    pub optional_functions: UstrSet,
}

impl Interface {
    pub fn new(name: Spanned<Ustr>, functions: Vec<Function>, optional_functions: UstrSet) -> Self {
        Self { name, functions, optional_functions }
    }

    pub fn validate(&self, ctx: &Context, functions: Vec<&Function>) -> Result<(), Exception> {
        let interface_funcs: HashMap<Ustr, &Function> =
            HashMap::from_iter(self.functions.iter().map(|f| (f.name.raw, f)));

        // Check that all required functions are present
        for expected_func in &self.functions {
            let is_optional = self.optional_functions.contains(&expected_func.name.raw);

            if !is_optional {
                // Required function must be present
                if !functions.iter().any(|f| f.name.raw == expected_func.name.raw) {
                    return Err(Exception::new(
                        "TypeError",
                        format!(
                            "interface '{}' requires function '{}' to be implemented",
                            self.name, expected_func.name
                        ),
                    ));
                }
            }
        }

        // Validate all provided functions match expected signatures
        for func in &functions {
            if let Some(expected) = interface_funcs.get(&func.name.raw) {
                // Function is part of the interface, validate signature
                if expected.params.len() != func.params.len() {
                    return Err(Exception::new(
                        "TypeError",
                        format!(
                            "function '{}' expects {} parameter(s), got {}",
                            func.name,
                            expected.params.len(),
                            func.params.len()
                        ),
                    )
                    .with_extra(format!("for interface '{}'", self.name).into()));
                }

                expected
                    .params
                    .iter()
                    .zip(func.params.iter())
                    .try_for_each(|(a, b)| {
                        // Allow implementation params to have no type annotation (any type)
                        // Check type compatibility rather than exact equality
                        match (&a.ty, &b.ty) {
                            (Some(expected_ty), Some(actual_ty)) => {
                                // Check if types are compatible
                                let compatible = expected_ty.raw == actual_ty.raw ||
                                    matches!((&expected_ty.raw, &actual_ty.raw),
                                        (Ty::Num, Ty::Dim(_)) |  // num can accept dimensioned quantities
                                        (Ty::Num, Ty::Int) |      // num can accept int
                                        (Ty::Num, Ty::Float) |    // num can accept float
                                        (Ty::Any, _) |            // any accepts anything
                                        (_, Ty::Any)              // anything can be passed as any
                                    );

                                if !compatible {
                                    return Err(Exception::new(
                                        "TypeError",
                                        format!(
                                            "function '{}' parameter '{}' has type '{}', but interface expects '{}'",
                                            func.name,
                                            b.name,
                                            actual_ty.pretty_string(ctx),
                                            expected_ty.pretty_string(ctx)
                                        ),
                                    )
                                    .with_extra(format!("for interface '{}'", self.name).into()));
                                }
                            }
                            (Some(_), None) => {
                                // Implementation has no type annotation - that's okay
                            }
                            _ => {
                                // Other cases are fine
                            }
                        }
                        Ok(())
                    })?;
            } else {
                // Function is not part of the interface
                return Err(Exception::new(
                    "TypeError",
                    format!(
                        "interface '{}' does not expect a function named '{}'",
                        self.name, func.name
                    ),
                ));
            }
        }
        Ok(())
    }
}

// MARK: InterfaceTable

/// A table that tracks defined interfaces.
#[derive(Clone, Debug)]
pub struct InterfaceTable {
    interfaces: UstrMap<Interface>,
}

impl InterfaceTable {
    pub fn new() -> Self {
        Self {
            interfaces: UstrMap::default(),
        }
    }

    pub fn insert(&mut self, interface: Interface) -> Option<Interface> {
        self.interfaces
            .insert(interface.name.clone().into(), interface)
    }

    pub fn get(&self, name: Ustr) -> Option<&Interface> {
        self.interfaces.get(&name)
    }
}
