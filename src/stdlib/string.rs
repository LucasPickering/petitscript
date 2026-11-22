//! Standard library functions related to strings. This includes the `String`
//! module and its prototype

use crate::{
    Value, World,
    execute::Prototype,
    value::{PetitString, function::BoundFunction},
};

/// Coerce a value to a string
/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/String
pub fn constructor(_: &World, value: Value) -> PetitString {
    value.to_string()
}

/// Define all prototype functions for the `String` type.
pub fn prototype() -> Prototype {
    let mut prototype = Prototype::default();
    prototype.declare("trim", BoundFunction::new(trim));
    prototype.declare("trimStart", BoundFunction::new(trim_start));
    prototype.declare("trimEnd", BoundFunction::new(trim_end));
    prototype
}

/// `String.trim`
/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/trim
fn trim(_: &World, this: PetitString, _: ()) -> String {
    this.trim().to_owned()
}

/// `String.trimStart`
/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/trimStart
fn trim_start(_: &World, this: PetitString, _: ()) -> String {
    this.trim_start().to_owned()
}

/// `String.trimEnd`
/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/trimEnd
fn trim_end(_: &World, this: PetitString, _: ()) -> String {
    this.trim_end().to_owned()
}
