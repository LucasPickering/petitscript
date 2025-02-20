//! The PetitJS standard library

use crate::{
    error::RuntimeError, function::Varargs, scope::Scope, value::Object,
    NativeFunction, Process,
};

/// Create a scope containing the entire standard library. This needs to be
/// recreated for every execution
pub fn stdlib() -> Scope {
    let mut scope = Scope::new();
    scope.declare("console", console().into(), false);
    scope
}

/// `console` module
fn console() -> Object {
    Object::default().insert("log", NativeFunction::new(console_log).into())
}

/// Log values to stdout
/// TODO support varargs
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
