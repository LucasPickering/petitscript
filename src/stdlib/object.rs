//! Standard library functions related to objects.

use crate::{
    Value, World,
    value::{Array, Object, PetitString},
};

/// `Object` module
pub fn module() -> Object {
    Object::default()
        .insert_fn("entries", entries)
        .insert_fn("hasOwn", has_own)
        .insert_fn("keys", keys)
        .insert_fn("values", values)
}

/// `Object.entries`: get the entries of an object as an array of `[[key,
/// value]]`
///
/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/entries>
fn entries(_: &World, object: Object) -> Array {
    object
        .into_iter()
        .map(|(k, v)| Array::from([k.into(), v]))
        .collect()
}

/// `Object.hasOwn`: check if an object contains a key
///
/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/hasOwn>
fn has_own(_: &World, (object, key): (Object, PetitString)) -> bool {
    object.get(&key).is_some()
}

/// `Object.keys`: get the keys of an object as an array
///
/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/keys>
fn keys(_: &World, object: Object) -> Array {
    object.into_keys().map(Value::from).collect()
}

/// `Object.values`: get the values of an object as an array
///
/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/values>
fn values(_: &World, object: Object) -> Array {
    object.into_values().collect()
}
