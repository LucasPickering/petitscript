//! The PetitScript standard library

mod json;

use crate::{
    function::Varargs, scope::Scope, value::Object, NativeFunctionTable,
    Process,
};

/// Create a scope containing the entire standard library. This needs to be
/// recreated for every execution
pub fn stdlib(functions: &mut NativeFunctionTable) -> Scope {
    let mut scope = Scope::new();
    scope.declare("console", console(functions));
    scope.declare("JSON", json::json(functions));
    scope
}

/// `console` module
fn console(functions: &mut NativeFunctionTable) -> Object {
    Object::default()
        .insert("debug", functions.create_fn(console_debug))
        .insert("log", functions.create_fn(console_log))
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
