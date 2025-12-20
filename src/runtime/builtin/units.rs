use crate::print::PrettyString;
use crate::runtime::{Context, Conversion, Dim, Exception, Module, Quantity, Ty, Value, ValueRef};
use ustr::Ustr;

pub(super) fn register(ctx: &mut Context) {
    ctx.get_module_mut("builtin")
        .expect("builtin module should exist")
        .with_interface(builtin_interface![
            "UnitImpl"
            to_base: (num) -> num;
            from_base: (num) -> num;
            display_name?: () -> str;
        ])
        .with_function(builtin_fn_v2!("unitof", |&ctx, v: any| {
            match v {
                Value::Quantity(q) => {
                    if let Some(unit) = q.get_unit() {
                        Ok(Value::Unit(unit))
                    } else {
                        Ok(Value::Empty)
                        // Err(Exception::new("ValueError", "value has no unit".to_string())
                        //     .with_backtrace(ctx.backtrace()))
                    }
                }
                Value::Unit(u) => Ok(Value::Unit(u)),
                Value::Ref(r) => {
                    // Deref references to check the underlying value
                    let inner = r.borrow().clone();
                    if let Value::Quantity(q) = inner {
                        if let Some(unit) = q.get_unit() {
                            Ok(Value::Unit(unit))
                        } else {
                            Err(
                                Exception::new("ValueError", "value has no unit".to_string())
                                    .with_backtrace(ctx.backtrace()),
                            )
                        }
                    } else {
                        Err(Exception::new(
                            "TypeError",
                            format!(
                                "expected value with unit, got {}",
                                inner.ty().pretty_string(&ctx)
                            ),
                        )
                        .with_backtrace(ctx.backtrace()))
                    }
                }
                other => Err(Exception::new(
                    "TypeError",
                    format!(
                        "expected value with unit, got {}",
                        other.ty().pretty_string(&ctx)
                    ),
                )
                .with_backtrace(ctx.backtrace())),
            }
        }))
        .with_function(builtin_fn_v2!("conversions", |&ctx, unit: unit| {
            let module = ctx.active_module().unwrap();
            let resolved_unit = module.units.resolve_suffix(unit).ok_or_else(|| {
                Exception::new("NameError", format!("unknown unit: {}", unit))
                    .with_backtrace(ctx.backtrace())
            })?;

            let compatible = module
                .conversion_graph
                .get_compatible_units(resolved_unit.name.raw)
                .ok_or_else(|| {
                    Exception::new(
                        "ValueError",
                        format!(
                            "no conversions available for unit {}",
                            resolved_unit.name.raw
                        ),
                    )
                    .with_backtrace(ctx.backtrace())
                })?;

            let units = compatible
                .iter()
                .copied()
                .map(Value::Unit)
                .collect::<Vec<_>>();

            Ok(Value::list(units))
        }))
        .with_function(builtin_fn_v2!("unit_name", |&ctx, unit: unit| {
            let module = ctx.active_module().unwrap();
            let resolved = module.units.resolve_suffix(unit).cloned().ok_or_else(|| {
                Exception::new("NameError", format!("unknown unit: {}", unit))
                    .with_backtrace(ctx.backtrace())
            })?;

            let display = match &resolved.conversion {
                Conversion::Impl(unit_impl) => unit_impl
                    .display_name(ctx)
                    .ok()
                    .flatten()
                    .unwrap_or_else(|| resolved.name.raw.to_string()),
                _ => resolved.name.raw.to_string(),
            };

            Ok(display)
        }))
        .with_function(builtin_fn_v2!("unit_cast", |&ctx, v: num, u: unit| {
            // Extract target unit information and clone conversion graph
            let (target_name, target_dim, source_unit, base_unit_opt, conv_graph) = {
                let active_module = ctx.active_module().unwrap();

                let target_unit = active_module
                    .resolve_unit_suffix(u.into())
                    .map_err(|_| Exception::new("NameError", format!("unknown unit: {}", u)))?;

                // Ensure the dimensions are compatible
                Dim::unify(ctx, v.dim.clone(), target_unit.as_dim())?;

                let target_name = target_unit.name.raw;
                let target_dim = target_unit.as_dim();
                let source_unit = v.dim.unit.as_ref().and_then(|u| u.simple_unit_name());

                // Get base unit if needed (when source_unit is None)
                let base_unit_opt = if source_unit.is_none() {
                    active_module
                        .conversion_graph
                        .get_base_unit(&target_dim.expr)
                } else {
                    None
                };

                // Clone conversion graph to avoid borrow checker issues
                let conv_graph = active_module.conversion_graph.clone();

                (
                    target_name,
                    target_dim,
                    source_unit,
                    base_unit_opt,
                    conv_graph,
                )
            };

            // Use conversion graph for efficient direct conversion
            let converted_value = if let Some(source_unit) = source_unit {
                if source_unit == target_name {
                    // Same unit, no conversion needed
                    v.number.clone()
                } else {
                    // Use conversion graph to convert directly from source to target
                    conv_graph
                        .convert(ctx, v.number.clone(), source_unit, target_name)
                        .map_err(|e| Exception::new("ValueError", e.to_string()))?
                }
            } else {
                // No source unit specified - treat as base unit
                let base_unit = base_unit_opt.ok_or_else(|| {
                    Exception::new(
                        "ValueError",
                        format!(
                            "no base unit found for dimension {}",
                            target_dim.pretty_string(ctx)
                        ),
                    )
                })?;

                if base_unit == target_name {
                    // Already at target
                    v.number.clone()
                } else {
                    // Convert from base to target
                    conv_graph
                        .convert(ctx, v.number.clone(), base_unit, target_name)
                        .map_err(|e| Exception::new("ValueError", e.to_string()))?
                }
            };

            let quantity = Quantity::new(converted_value, target_dim);
            Ok(Value::Quantity(quantity))
        }));
}
