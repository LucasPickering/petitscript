//! Standard library functions related to objects.

use crate::{Array, NativeFunctionTable, Object, Process, Value};

/// `Object` module
pub fn module(functions: &mut NativeFunctionTable) -> Object {
    Object::default()
        .insert("entries", functions.create_fn(entries))
        .insert("keys", functions.create_fn(keys))
        .insert("values", functions.create_fn(values))
}

/// `Object.entries`, to get the entries of an object as an array of `[[key,
/// value]]`
/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/entries
fn entries(_: &Process, object: Object) -> Array {
    object
        .into_iter()
        .map(|(k, v)| Array::from([k.into(), v]).into())
        .collect()
}

/// `Object.keys`, to get the keys of an object as an array
/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/keys
fn keys(_: &Process, object: Object) -> Array {
    object.into_keys().map(Value::from).collect()
}

/// `Object.values`, to get the values of an object as an array
/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/values
fn values(_: &Process, object: Object) -> Array {
    object.into_values().collect()
}
