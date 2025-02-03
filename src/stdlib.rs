//! The PetitJS standard library

use crate::{
    error::RuntimeResult,
    value::{Exports, Object},
    IntoNativeFunction, Value,
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
    Object::default().insert("log", console_log.into_native_fn().into())
}

/// Log values to stdout
/// TODO support varargs
fn console_log(value: Value) -> RuntimeResult<Value> {
    println!("{value}");
    /* for (i, value) in args.iter().enumerate() {
        if i > 0 {
            print!(" ");
        }
        print!("{value}");
    }
    println!(); */
    Ok(Value::Undefined)
}
