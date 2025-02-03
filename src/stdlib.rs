//! The PetitJS standard library

use crate::{
    error::RuntimeResult,
    value::{Exports, Object},
    NativeFunction, Value,
};

/// TODO
pub fn stdlib() -> Exports {
    Exports {
        default: None,
        named: [("console".into(), console().into())].into_iter().collect(),
    }
}

/// `console` module
fn console() -> Object {
    Object::default().insert("log", NativeFunction::new(console_log).into())
}

/// Log values to stdout
fn console_log(args: Vec<Value>) -> RuntimeResult<Value> {
    for (i, value) in args.iter().enumerate() {
        if i > 0 {
            print!(" ");
        }
        print!("{value}");
    }
    println!();
    Ok(Value::Undefined)
}
