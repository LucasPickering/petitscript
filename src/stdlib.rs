//! The PetitJS standard library

use crate::{
    error::RuntimeError, function::Varargs, scope::Scope, value::Object,
    NativeFunction, Process,
};

/// Create a scope containing the entire standard library. This needs to be
/// recreated for every execution
pub fn stdlib() -> Scope {
    let mut scope = Scope::global();
    scope.declare("console", console().into());
    scope
}

/// `console` module
fn console() -> Object {
    Object::default()
        .insert("debug", NativeFunction::new(console_debug).into())
        .insert("log", NativeFunction::new(console_log).into())
}

/// Log values to stdout
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

/// Log values to stdout with their debug format
fn console_debug(
    _: &Process,
    Varargs(values): Varargs,
) -> Result<(), RuntimeError> {
    for (i, value) in values.iter().enumerate() {
        if i > 0 {
            print!(" ");
        }
        print!("{value:?}");
    }
    println!();
    Ok(())
}
