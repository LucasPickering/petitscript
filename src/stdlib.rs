//! The PetitJS standard library

use crate::{
    error::RuntimeResult, runtime::exports::Exports, value::Object, Value,
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
    Object::default().insert("log", console_log.into())
}

/// Log values to stdout
fn console_log(args: &[Value]) -> RuntimeResult<Value> {
    for (i, value) in args.iter().enumerate() {
        if i > 0 {
            print!(" ");
        }
        print!("{value}");
    }
    println!();
    Ok(Value::Undefined)
}
