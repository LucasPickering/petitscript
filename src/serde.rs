//! Serialization and deserialization via the [serde] crate
//!
//! This module is designed to imitate a Serde data format crate, and thus
//! follows [the conventions laid out by Serde](https://serde.rs/conventions.html),
//! with the exception of the `Error` and `Result` types: we use [ValueError]
//! instead and no `Result` alias is provided.

mod de;
mod impls;
mod ser;

pub use de::Deserializer;
pub use ser::Serializer;

use crate::{error::ValueError, value::Function, Value};
use serde::{Deserialize, Serialize};
use std::{cell::RefCell, collections::BTreeMap};

/// Serialize an instance of type `T` into a PetitScript value
pub fn to_value<T: Serialize>(data: &T) -> Result<Value, ValueError> {
    data.serialize(&Serializer)
}

/// Deserialize an instance of type `T` from a PetitScript value
pub fn from_value<'de, T: Deserialize<'de>>(
    value: Value,
) -> Result<T, ValueError> {
    T::deserialize(Deserializer::new(value))
}

/// TODO
/// https://lucumr.pocoo.org/2021/11/14/abusing-serde/
struct FunctionPool {
    next_id: u64,
    pool: BTreeMap<u64, Function>,
}

impl FunctionPool {
    /// TODO
    const STRUCT_NAME: &'static str = "$petitscript::Function";

    thread_local! {
        static POOL: RefCell<FunctionPool> = const {
            RefCell::new(FunctionPool::new())
        };
    }

    const fn new() -> Self {
        Self {
            next_id: 0,
            pool: BTreeMap::new(),
        }
    }

    /// TODO
    fn insert(function: Function) -> u64 {
        Self::POOL.with_borrow_mut(|pool| {
            let id = pool.next_id;
            // It's exceptionally unlikely we'll ever wrap around these IDs, but
            // if we do it's even more unlikely that the the original IDs are
            // still in the pool. Zero-cost preventative maintenance
            pool.next_id = pool.next_id.wrapping_add(1);
            pool.pool.insert(id, function);
            id
        })
    }

    /// TODO
    fn take(id: u64) -> Result<Function, ValueError> {
        Self::POOL
            .with_borrow_mut(|pool| pool.pool.remove(&id))
            .ok_or_else(|| todo!())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{
            Expression, FunctionBody, FunctionDefinition, FunctionParameter,
        },
        value::function::{BoundFunction, Function},
    };
    use indexmap::indexmap;
    use test_case::test_case;

    /// Serializing a `Value` to a `Value`, and deserializing back, should yield
    /// the same value each time. This is for the case when someone is
    /// serializing from or deserializing into a struct that stores a `Value`
    /// somewhere within.
    #[test_case(Value::Undefined; "undefined")]
    #[test_case(Value::Null; "null")]
    #[test_case(true; "bool_true")]
    #[test_case(false; "bool_false")]
    #[test_case(32; "int")]
    #[test_case(5.67; "float")]
    #[test_case(Value::String("hello".into()); "string")]
    #[test_case(
        vec![Value::Null, Value::Undefined, 32.into(), false.into()]; "array"
    )]
    #[test_case(indexmap! {
        "null".to_owned() => Value::Null,
        "undefined".to_owned() => Value::Undefined,
        "b".to_owned() => 32.into(),
        "c".to_owned() => indexmap! {"d".to_owned() => true.into()}.into()
    }; "object")]
    #[test_case(
        Function::user(
            Some("func".into()),
            FunctionDefinition::new(
                [FunctionParameter::identifier("x")],
                FunctionBody::expression(Expression::reference("x")),
            )
                .with_name("func")
                .with_captures(["cap"])
                .into(),
            indexmap! {"cap".to_owned() => 32.into()},
        );
        "function_user"
    )]
    #[test_case(
        // This will do pointer equality to ensure it's the same fn
        Function::native("func".into(), |_, _: Value| 0);
        "function_native"
    )]
    #[test_case(
        Function::bound(
            "func".into(),
            // This will do pointer equality to ensure it's the same fn
            BoundFunction::new(|_, _: Value, _: Value| 0),
            [1, 2, 3].into(),
        );
        "function_bound"
    )]
    fn identity(value: impl Into<Value>) {
        let value = value.into();
        let deserialized: Value = from_value(value.clone()).unwrap();
        assert_eq!(deserialized, value, "Deserialization did not math");
        let serialized = to_value(&value).unwrap();
        assert_eq!(serialized, value, "Serialization did not match");
    }
}
