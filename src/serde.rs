//! Serialization and deserialization
//!
//! This module is designed to imitate Serde data format crate, and thus follows
//! [the conventions laid out by Serde](https://serde.rs/conventions.html),
//! with the exception of the `Error` and `Result` types: we use [ValueError]
//! instead.

mod de;
mod impls;
mod ser;

pub use de::Deserializer;
pub use ser::Serializer;

use crate::{error::ValueError, Value};
use serde::{Deserialize, Serialize};

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        compile::FunctionDefinitionId,
        execute::ProcessId,
        function::{Function, UserFunctionId},
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
    #[test_case(Function::user(
        UserFunctionId {
            process_id: ProcessId(0),
            definition_id: FunctionDefinitionId(0),
        },
        None,
        indexmap! {"cap".to_owned() => 32.into()},
    ); "function")]
    fn identity(value: impl Into<Value>) {
        let value = value.into();
        let deserialized: Value = from_value(value.clone()).unwrap();
        assert_eq!(deserialized, value, "Deserialization did not math");
        let serialized = to_value(&value).unwrap();
        assert_eq!(serialized, value, "Serialization did not match");
    }
}
