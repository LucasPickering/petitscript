//! The PetitScript standard library

use crate::{
    error::RuntimeError, function::Varargs, json, scope::Scope, value::Object,
    NativeFunctionTable, Process, Value,
};

/// Create a scope containing the entire standard library. This needs to be
/// recreated for every execution
pub fn stdlib(functions: &mut NativeFunctionTable) -> Scope {
    let mut scope = Scope::new();
    scope.declare("console", console(functions));
    scope.declare("JSON", json(functions));
    scope
}

/// `console` module
fn console(functions: &mut NativeFunctionTable) -> Object {
    Object::default()
        .insert("debug", functions.create_fn(console_debug))
        .insert("log", functions.create_fn(console_log))
}

/// Log values to stdout
fn console_log(_: &Process, Varargs(values): Varargs) {
    for (i, value) in values.iter().enumerate() {
        if i > 0 {
            print!(" ");
        }
        print!("{value}");
    }
    println!();
}

/// Log values to stdout with their debug format
fn console_debug(_: &Process, Varargs(values): Varargs) {
    for (i, value) in values.iter().enumerate() {
        if i > 0 {
            print!(" ");
        }
        print!("{value:?}");
    }
    println!();
}

/// `JSON` module
/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON
pub fn json(functions: &mut NativeFunctionTable) -> Object {
    Object::default()
        .insert("parse", functions.create_fn(json_parse))
        .insert("stringify", functions.create_fn(json_stringify))
}

/// Stringify a value to JSON
/// TODO accept &Value
fn json_stringify(
    _: &Process,
    value: Value,
    // TODO support `replace` and `space` args
) -> String {
    let mut buf = String::new();
    json::write_json(&mut buf, &value);
    buf
}

/// Parse a JSON string
/// TODO can we accept a &str?
fn json_parse(
    _: &Process,
    input: String,
    // TODO support `reviver` arg
) -> Result<Value, RuntimeError> {
    json::parse_json(&input).map_err(|error| RuntimeError::JsonParse { error })
}
