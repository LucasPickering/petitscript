//! The PetitJS standard library

use crate::{runtime::module::Module, value::Object, Result, Value};

/// TODO
pub fn stdlib() -> Module {
    Module {
        default: None,
        named: [("console".into(), console().into())].into_iter().collect(),
    }
}

fn console() -> Object {
    Object::default().insert("log", console_log.into())
}

/// Log values to stdout
fn console_log(args: &[Value]) -> Result<Value> {
    // TODO don't include quotes on strings here
    for (i, value) in args.iter().enumerate() {
        if i > 0 {
            print!(" ");
        }
        print!("{value}");
    }
    println!();
    Ok(Value::Undefined)
}
