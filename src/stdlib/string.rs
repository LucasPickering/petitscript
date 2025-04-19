//! Standard library functions related to strings. This includes the `String`
//! module and its prototype

use crate::{
    scope::Prototype, NativeFunctionTable, PetitString, Process, Value,
};

/// Coerce a value to a string
/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/String
pub fn constructor(_: &Process, value: Value) -> PetitString {
    value.to_string()
}

/// Define all prototype functions for the `String` type.
pub fn prototype(functions: &mut NativeFunctionTable) -> Prototype {
    let mut prototype = Prototype::default();
    prototype.declare("trim", functions.create_bound(trim));
    prototype.declare("trimStart", functions.create_bound(trim_start));
    prototype.declare("trimEnd", functions.create_bound(trim_end));
    prototype
}

/// `String.trim`
/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/trim
fn trim(_: &Process, this: PetitString, _: ()) -> String {
    this.trim().to_owned()
}

/// `String.trimStart`
/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/trimStart
fn trim_start(_: &Process, this: PetitString, _: ()) -> String {
    this.trim_start().to_owned()
}

/// `String.trimEnd`
/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/trimEnd
fn trim_end(_: &Process, this: PetitString, _: ()) -> String {
    this.trim_end().to_owned()
}
