//! Standard library functions related to arrays. This includes the `Array`
//! module and its prototype

use crate::{
    execute::Prototype,
    value::{function::BoundFunction, Array},
    Process, Value,
};

/// Define all prototype functions for the `Array` type.
pub fn prototype() -> Prototype {
    let mut prototype = Prototype::default();
    prototype.declare("includes", BoundFunction::new(includes));
    prototype
}

/// `Array.includes`
/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/includes>
fn includes(_: &Process, this: Array, element: Value) -> bool {
    this.contains(&element)
}
