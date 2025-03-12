//! The PetitScript standard library

use crate::{
    error::RuntimeError, function::Varargs, scope::Scope, value::Object,
    Engine, Process,
};

/// Create a scope containing the entire standard library. This needs to be
/// recreated for every execution
pub fn stdlib(engine: &mut Engine) -> Scope {
    let mut scope = Scope::new();
    scope.declare("console", console(engine));
    scope
}

/// `console` module
fn console(engine: &mut Engine) -> Object {
    Object::default()
        .insert("debug", engine.create_fn(console_debug))
        .insert("log", engine.create_fn(console_log))
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
