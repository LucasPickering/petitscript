//! The PetitScript standard library

use crate::{
    error::RuntimeError, function::Varargs, scope::Scope, value::Object, Array,
    NativeFunctionTable, Process, Value,
};

/// Create a scope containing the entire standard library. This needs to be
/// recreated for every execution
pub fn stdlib(functions: &mut NativeFunctionTable) -> Scope {
    let mut scope = Scope::new();
    scope.declare("console", console(functions));
    #[cfg(feature = "serde")]
    {
        scope.declare("JSON", json(functions));
    }
    scope.declare_prototype("includes", array_includes);
    scope
}

/// `console` module
fn console(functions: &mut NativeFunctionTable) -> Object {
    Object::default()
        .insert("debug", functions.create_fn(console_debug))
        .insert("log", functions.create_fn(console_log))
}

/// Log values to stdout
fn console_log(
    _: &Process,
    Varargs(values): Varargs,
) -> Result<(), RuntimeError> {
    for (i, value) in values.iter().enumerate() {
        if i > 0 {
            print!(" ");
        }
        print!("{value}");
    }
    println!();
    Ok(())
}

/// Log values to stdout with their debug format
fn console_debug(
    _: &Process,
    Varargs(values): Varargs,
) -> Result<(), RuntimeError> {
    for (i, value) in values.iter().enumerate() {
        if i > 0 {
            print!(" ");
        }
        print!("{value:?}");
    }
    println!();
    Ok(())
}

/// `JSON` module
#[cfg(feature = "serde")]
fn json(functions: &mut NativeFunctionTable) -> Object {
    Object::default()
        .insert("parse", functions.create_fn(json_parse))
        .insert("stringify", functions.create_fn(json_stringify))
}

/// Parse a JSON string
/// TODO can we accept a &str?
#[cfg(feature = "serde")]
fn json_parse(
    _: &Process,
    input: String,
) -> Result<crate::Value, RuntimeError> {
    serde_json::from_str(&input).map_err(RuntimeError::other)
}

/// Stringify a value to JSON
/// TODO accept &Value
#[cfg(feature = "serde")]
fn json_stringify(
    _: &Process,
    value: crate::Value,
) -> Result<String, RuntimeError> {
    serde_json::to_string_pretty(&value).map_err(RuntimeError::other)
}

/// `Array.prototype.includes`
/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/includes>
fn array_includes(
    _: &Process,
    array: Array,
    value: Value,
) -> Result<bool, RuntimeError> {
    Ok(array.contains(&value))
}
