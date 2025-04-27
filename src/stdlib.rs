//! The PetitScript standard library

mod array;
mod object;
mod string;

use crate::{
    error::RuntimeError,
    execute::GlobalEnvironment,
    value::{function::Varargs, Number, Object, Value, ValueType},
    Process,
};

/// Create a scope containing the entire standard library. This needs to be
/// recreated for every execution
pub fn stdlib() -> GlobalEnvironment {
    let mut scope = GlobalEnvironment::default();
    scope.declare_fn("Boolean", boolean);
    scope.declare_fn("Number", number);
    scope.declare_fn("String", string::constructor);
    scope.declare("Object", object::module());
    scope.declare("console", console());
    scope.declare("JSON", json());
    scope.declare_prototype(ValueType::String, string::prototype());
    scope.declare_prototype(ValueType::Array, array::prototype());
    scope
}

/// Coerce a value to a boolean
fn boolean(_: &Process, value: Value) -> bool {
    value.to_bool()
}

/// Coerce a value to a number
/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number#number_coercion
fn number(_: &Process, value: Value) -> Number {
    value.to_number().unwrap_or(Number::NAN)
}

/// `console` module
fn console() -> Object {
    Object::default()
        .insert_fn("debug", console_debug)
        .insert_fn("log", console_log)
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
pub fn json() -> Object {
    Object::default()
        .insert_fn("parse", json_parse)
        .insert_fn("stringify", json_stringify)
}

/// Stringify a value to JSON
/// TODO accept &Value
fn json_stringify(
    _: &Process,
    value: Value,
    // TODO support `replace` and `space` args
) -> String {
    value.to_json()
}

/// Parse a JSON string
/// TODO can we accept a &str?
fn json_parse(
    _: &Process,
    input: String,
    // TODO support `reviver` arg
) -> Result<Value, RuntimeError> {
    Value::from_json(&input)
}
