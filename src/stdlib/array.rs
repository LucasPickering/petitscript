//! Standard library functions related to arrays. This includes the `Array`
//! module and its prototype

use crate::{scope::Prototype, Array, NativeFunctionTable, Process, Value};

/// Define all prototype functions for the `Array` type.
pub fn prototype(functions: &mut NativeFunctionTable) -> Prototype {
    let mut prototype = Prototype::default();
    prototype.declare("includes", functions.create_bound(includes));
    prototype
}

/// `Array.includes`
/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/includes
fn includes(_: &Process, this: Array, element: Value) -> bool {
    // TODO take &Value instead
    this.contains(&element)
}
