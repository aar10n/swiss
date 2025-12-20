use crate::interp;
use crate::runtime::{Context, Dim, Exception, Function, Module, Number, Quantity, Ty, Value};

use rug::{Float, Integer};
use serde_json::{self, Number as JsonNumber, Value as JsonValue};
use std::str::FromStr;
use ustr::Ustr;

pub(super) fn register(module: &mut Module) {
    let encoding_interface = builtin_interface!["encoding"
        encode: (any) -> str;
        decode: (str) -> any;
    ];

    module
        .with_interface(encoding_interface)
        .with_function(
            builtin_fn_v2!("register_encoding", |&ctx, name: str, encode_fn: fn, decode_fn: fn| {
                register_encoding(ctx, name, encode_fn, decode_fn)?;
                Ok(Value::default())
            }),
        )
        .with_function(builtin_fn_v2!("encode", |&ctx, name: str, value: any| {
            let result = dispatch_encode(ctx, name.as_str(), value)?;
            Ok(result)
        }))
        .with_function(builtin_fn_v2!("decode", |&ctx, name: str, text: str| {
            let result = dispatch_decode(ctx, name.as_str(), text)?;
            Ok(result)
        }))
        .with_function(builtin_fn_v2!("json_encode", |&ctx, value: any| {
            let json = value_to_json(ctx, &value)?;
            Ok(Value::String(json.to_string()))
        }))
        .with_function(builtin_fn_v2!("json_decode", |&ctx, text: str| {
            let parsed: JsonValue = serde_json::from_str(&text).map_err(|e| {
                Exception::new("ValueError", e.to_string()).with_backtrace(ctx.backtrace())
            })?;
            json_to_value(ctx, &parsed)
        }));
}

fn register_encoding(
    ctx: &mut Context,
    name: String,
    encode_fn: Function,
    decode_fn: Function,
) -> Result<(), Exception> {
    validate_encode_fn(ctx, &encode_fn)?;
    validate_decode_fn(ctx, &decode_fn)?;
    ctx.encodings
        .register(name.clone().into(), encode_fn, decode_fn)?;
    Ok(())
}

fn dispatch_encode(ctx: &mut Context, name: &str, value: Value) -> Result<Value, Exception> {
    let key: Ustr = name.into();
    let (encode_fn, _) = ctx.encodings.get(&key).cloned().ok_or_else(|| {
        Exception::new("NameError", format!("encoding '{}' not registered", name))
            .with_backtrace(ctx.backtrace())
    })?;

    let result = interp::call_function(ctx, &encode_fn, vec![value])?;
    match result {
        Value::String(_) => Ok(result),
        other => Err(Exception::new(
            "TypeError",
            format!(
                "encode for '{}' must return str, got {}",
                name,
                other.ty().to_string()
            ),
        )
        .with_backtrace(ctx.backtrace())),
    }
}

fn dispatch_decode(ctx: &mut Context, name: &str, text: String) -> Result<Value, Exception> {
    let key: Ustr = name.into();
    let (_, decode_fn) = ctx.encodings.get(&key).cloned().ok_or_else(|| {
        Exception::new("NameError", format!("encoding '{}' not registered", name))
            .with_backtrace(ctx.backtrace())
    })?;
    interp::call_function(ctx, &decode_fn, vec![Value::String(text)])
}

fn validate_encode_fn(ctx: &Context, func: &Function) -> Result<(), Exception> {
    validate_single_param(ctx, func, "encode", Ty::Any)
}

fn validate_decode_fn(ctx: &Context, func: &Function) -> Result<(), Exception> {
    validate_single_param(ctx, func, "decode", Ty::Str)
}

fn validate_single_param(
    ctx: &Context,
    func: &Function,
    label: &str,
    expected: Ty,
) -> Result<(), Exception> {
    if func.params.len() != 1 {
        return Err(Exception::new(
            "TypeError",
            format!("{} must take exactly one parameter", label),
        )
        .with_backtrace(ctx.backtrace()));
    }

    if let Some(param_ty) = &func.params[0].ty {
        let actual = &param_ty.raw;
        let is_expected = expected == Ty::Any || actual == &expected || actual == &Ty::Any;
        if !is_expected {
            return Err(Exception::new(
                "TypeError",
                format!(
                    "{} expects parameter of type '{}', got '{}'",
                    label,
                    expected.to_string(),
                    actual.to_string()
                ),
            )
            .with_backtrace(ctx.backtrace()));
        }
    }

    Ok(())
}

fn value_to_json(ctx: &Context, value: &Value) -> Result<JsonValue, Exception> {
    Ok(match value {
        Value::Empty => JsonValue::Null,
        Value::Boolean(b) => JsonValue::Bool(*b),
        Value::String(s) => JsonValue::String(s.clone()),
        Value::Quantity(q) => {
            if !q.is_dimless() {
                return Err(Exception::new(
                    "TypeError",
                    "cannot encode dimensioned quantity to json".to_owned(),
                )
                .with_backtrace(ctx.backtrace()));
            }
            JsonValue::Number(number_to_json(ctx, &q.number)?)
        }
        Value::Tuple(items) => {
            let mut arr = Vec::with_capacity(items.len());
            for item in items.iter() {
                arr.push(value_to_json(ctx, item)?);
            }
            JsonValue::Array(arr)
        }
        Value::List(list) => {
            let mut arr = Vec::new();
            for item in list.borrow_slice().iter() {
                arr.push(value_to_json(ctx, item)?);
            }
            JsonValue::Array(arr)
        }
        Value::Object(obj) => {
            let mut map = serde_json::Map::new();
            for (k, v) in obj.borrow().iter() {
                map.insert(k.to_string(), value_to_json(ctx, v)?);
            }
            JsonValue::Object(map)
        }
        other => {
            return Err(Exception::new(
                "TypeError",
                format!(
                    "cannot encode value of type '{}' to json",
                    other.ty().to_string()
                ),
            )
            .with_backtrace(ctx.backtrace()))
        }
    })
}

fn json_to_value(ctx: &Context, value: &JsonValue) -> Result<Value, Exception> {
    Ok(match value {
        JsonValue::Null => Value::Empty,
        JsonValue::Bool(b) => Value::Boolean(*b),
        JsonValue::String(s) => Value::String(s.clone()),
        JsonValue::Number(n) => Value::Quantity(Quantity::new(
            number_from_json(ctx, n)?,
            crate::runtime::Dim::none(),
        )),
        JsonValue::Array(items) => {
            let mut vals = Vec::with_capacity(items.len());
            for item in items {
                vals.push(json_to_value(ctx, item)?);
            }
            Value::list(vals)
        }
        JsonValue::Object(map) => {
            let mut vals = Vec::with_capacity(map.len());
            for (k, v) in map {
                vals.push((k.clone().into(), json_to_value(ctx, v)?));
            }
            Value::object(vals)
        }
    })
}

fn number_to_json(ctx: &Context, number: &Number) -> Result<JsonNumber, Exception> {
    match number {
        Number::Int(i) => JsonNumber::from_str(&i.to_string()).map_err(|_| {
            Exception::new(
                "ValueError",
                "integer too large to encode as json number".to_owned(),
            )
            .with_backtrace(ctx.backtrace())
        }),
        Number::Float(f) => {
            if f.is_nan() || f.is_infinite() {
                return Err(Exception::new(
                    "ValueError",
                    "cannot encode NaN or infinite float to json".to_owned(),
                )
                .with_backtrace(ctx.backtrace()));
            }
            JsonNumber::from_str(&f.to_string()).map_err(|_| {
                Exception::new(
                    "ValueError",
                    "float could not be encoded as json number".to_owned(),
                )
                .with_backtrace(ctx.backtrace())
            })
        }
    }
}

fn number_from_json(ctx: &Context, number: &JsonNumber) -> Result<Number, Exception> {
    if let Some(i) = number.as_i64() {
        return Ok(Number::Int(Integer::from(i)));
    }
    if let Some(u) = number.as_u64() {
        return Ok(Number::Int(Integer::from(u)));
    }
    if let Some(f) = number.as_f64() {
        return Ok(Number::Float(Float::with_val(
            ctx.config.float_precision,
            f,
        )));
    }

    Err(Exception::new(
        "ValueError",
        "unsupported json number representation".to_owned(),
    )
    .with_backtrace(ctx.backtrace()))
}
