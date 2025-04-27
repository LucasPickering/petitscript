//! Standard library functions related to objects.

use crate::{
    value::{Array, Object},
    Process, Value,
};

/// `Object` module
pub fn module() -> Object {
    Object::default()
        .insert_fn("entries", entries)
        .insert_fn("keys", keys)
        .insert_fn("values", values)
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
