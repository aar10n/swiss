use super::{Conversion, Dim, DimExpr, Number};
use crate::runtime::Context;
use std::collections::HashMap;
use ustr::Ustr;

/// A conversion graph that enables efficient unit conversions.
///
/// This graph precomputes direct conversion paths between all units
/// that share the same dimension, allowing O(1) lookup time for conversions.
#[derive(Clone, Debug)]
pub struct ConversionGraph {
    /// Maps (from_unit, to_unit) -> direct conversion edge
    edges: HashMap<(Ustr, Ustr), ConversionEdge>,

    /// Maps normalized dimension expressions to lists of unit names
    dimension_index: HashMap<String, Vec<Ustr>>,

    /// Maps unit names to their dimensional expressions
    unit_dimensions: HashMap<Ustr, DimExpr>,

    /// Maps unit names to their conversions (to base unit)
    unit_conversions: HashMap<Ustr, Conversion>,

    /// Maps dimension expressions to their base unit (if any)
    base_units: HashMap<String, Ustr>,
}

/// A direct conversion edge between two units.
#[derive(Clone, Debug)]
pub struct ConversionEdge {
    /// The conversion from source to target
    pub conversion: DirectConversion,
}

/// A direct conversion between two units.
#[derive(Clone, Debug)]
pub enum DirectConversion {
    /// Simple linear conversion: (value * from_scale) / to_scale
    Linear {
        from_scale: Number,
        to_scale: Number,
    },

    /// Conversion through base unit (for custom conversion functions)
    /// source -> base (using source conversion)
    /// base -> target (using target conversion)
    ThroughBase {
        source_to_base: Conversion,
        base_to_target: Conversion,
    },
}

impl ConversionGraph {
    /// Create a new empty conversion graph.
    pub fn new() -> Self {
        Self {
            edges: HashMap::new(),
            dimension_index: HashMap::new(),
            unit_dimensions: HashMap::new(),
            unit_conversions: HashMap::new(),
            base_units: HashMap::new(),
        }
    }

    /// Register a unit in the conversion graph.
    ///
    /// This should be called when a unit is defined. The graph will be
    /// incrementally updated with new conversion edges.
    pub fn register_unit(
        &mut self,
        name: Ustr,
        dimension: DimExpr,
        conversion: Conversion,
        is_base: bool,
    ) {
        let dim_key = dimension.normalized().to_string();

        // Store unit information
        self.unit_dimensions.insert(name, dimension.clone());
        self.unit_conversions.insert(name, conversion.clone());

        // Add to dimension index
        self.dimension_index
            .entry(dim_key.clone())
            .or_insert_with(Vec::new)
            .push(name);

        // Mark as base unit if specified
        if is_base {
            self.base_units.insert(dim_key.clone(), name);
        }

        // Build conversion edges to all other units with the same dimension
        // Clone the units vector to avoid borrow checker issues
        if let Some(units) = self.dimension_index.get(&dim_key).cloned() {
            for other_unit in units {
                if other_unit != name {
                    // Create bidirectional edges
                    self.add_edge(name, other_unit);
                    self.add_edge(other_unit, name);
                }
            }
        }
    }

    /// Add a conversion edge between two units.
    fn add_edge(&mut self, from: Ustr, to: Ustr) {
        let from_conv = self.unit_conversions.get(&from).unwrap().clone();
        let to_conv = self.unit_conversions.get(&to).unwrap().clone();

        // Try to compute a direct linear conversion factor
        let conversion = match (&from_conv, &to_conv) {
            (Conversion::Scale(from_scale), Conversion::Scale(to_scale)) => {
                // Direct conversion: (value * from_scale) / to_scale
                DirectConversion::Linear {
                    from_scale: from_scale.clone(),
                    to_scale: to_scale.clone(),
                }
            }
            _ => {
                // At least one uses custom conversion functions
                // Must go through base unit
                DirectConversion::ThroughBase {
                    source_to_base: from_conv,
                    base_to_target: to_conv,
                }
            }
        };

        self.edges.insert(
            (from, to),
            ConversionEdge { conversion },
        );
    }

    /// Find a conversion path between two units.
    ///
    /// Returns the conversion edge if the units are compatible, or None if
    /// they have incompatible dimensions.
    pub fn find_conversion(&self, from: Ustr, to: Ustr) -> Option<&ConversionEdge> {
        // Quick check: same unit
        if from == to {
            return None; // No conversion needed
        }

        // Look up direct edge
        self.edges.get(&(from, to))
    }

    /// Get all units that share the same dimension as the given unit.
    pub fn get_compatible_units(&self, unit: Ustr) -> Option<&[Ustr]> {
        let dim = self.unit_dimensions.get(&unit)?;
        let dim_key = dim.normalized().to_string();
        self.dimension_index.get(&dim_key).map(|v| v.as_slice())
    }

    /// Find units that match a given dimension expression.
    pub fn find_units_by_dimension(&self, dimension: &DimExpr) -> Option<&[Ustr]> {
        let dim_key = dimension.normalized().to_string();
        self.dimension_index.get(&dim_key).map(|v| v.as_slice())
    }

    /// Get the base unit for a given dimension expression.
    pub fn get_base_unit(&self, dimension: &DimExpr) -> Option<Ustr> {
        let dim_key = dimension.normalized().to_string();
        self.base_units.get(&dim_key).copied()
    }

    /// Convert a value from one unit to another.
    ///
    /// Returns None if the units are incompatible.
    pub fn convert(
        &self,
        ctx: &mut Context,
        value: Number,
        from: Ustr,
        to: Ustr,
    ) -> Result<Number, ConversionError> {
        // Same unit - no conversion
        if from == to {
            return Ok(value);
        }

        // Find conversion edge
        let edge = self.find_conversion(from, to)
            .ok_or(ConversionError::IncompatibleUnits { from, to })?;

        // Apply conversion
        match &edge.conversion {
            DirectConversion::Linear { from_scale, to_scale } => {
                // Convert: (value * from_scale) / to_scale
                let scaled = Number::safe_mul(ctx, value, from_scale.clone())
                    .map_err(|e| ConversionError::ConversionFailed(e.to_string()))?;
                Number::safe_div(ctx, scaled, to_scale.clone())
                    .map_err(|e| ConversionError::ConversionFailed(e.to_string()))
            }
            DirectConversion::ThroughBase { source_to_base, base_to_target } => {
                // Convert to base unit
                let base_value = source_to_base.to_base(ctx, value)
                    .map_err(|e| ConversionError::ConversionFailed(e.to_string()))?;

                // Convert from base to target
                base_to_target.from_base(ctx, base_value)
                    .map_err(|e| ConversionError::ConversionFailed(e.to_string()))
            }
        }
    }
}

/// Errors that can occur during unit conversion.
#[derive(Debug, Clone)]
pub enum ConversionError {
    /// The units have incompatible dimensions
    IncompatibleUnits { from: Ustr, to: Ustr },

    /// The conversion function failed
    ConversionFailed(String),
}

impl std::fmt::Display for ConversionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConversionError::IncompatibleUnits { from, to } => {
                write!(f, "Cannot convert from {} to {}: incompatible dimensions", from, to)
            }
            ConversionError::ConversionFailed(msg) => {
                write!(f, "Conversion failed: {}", msg)
            }
        }
    }
}

impl std::error::Error for ConversionError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_linear_conversion() {
        let mut graph = ConversionGraph::new();

        // Register meter as base unit
        let meter = Ustr::from("meter");
        let kilometer = Ustr::from("kilometer");
        let dim_l = DimExpr::Dimension(Ustr::from("L"));

        graph.register_unit(
            meter,
            dim_l.clone(),
            Conversion::Scale(Number::Int(1.into())),
            true,
        );

        graph.register_unit(
            kilometer,
            dim_l.clone(),
            Conversion::Scale(Number::Int(1000.into())),
            false,
        );

        // Test conversion: 5000 meters -> kilometers
        // 5000 meters * (1 / 1000) = 5 kilometers
        let result = graph.find_conversion(meter, kilometer);
        assert!(result.is_some());
    }

    #[test]
    fn test_dimension_indexing() {
        let mut graph = ConversionGraph::new();

        let meter = Ustr::from("meter");
        let kilometer = Ustr::from("kilometer");
        let foot = Ustr::from("foot");
        let dim_l = DimExpr::Dimension(Ustr::from("L"));

        graph.register_unit(meter, dim_l.clone(), Conversion::Scale(Number::Int(1.into())), true);
        graph.register_unit(kilometer, dim_l.clone(), Conversion::Scale(Number::Int(1000.into())), false);

        use rug::Float;
        let foot_factor = Number::Float(Float::with_val(53, 0.3048));
        graph.register_unit(foot, dim_l.clone(), Conversion::Scale(foot_factor), false);

        // All three units should be in the dimension index
        let units = graph.find_units_by_dimension(&dim_l).unwrap();
        assert_eq!(units.len(), 3);
        assert!(units.contains(&meter));
        assert!(units.contains(&kilometer));
        assert!(units.contains(&foot));
    }
}
