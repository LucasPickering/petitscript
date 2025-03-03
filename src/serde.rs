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

use crate::{error::ValueError, FromJs, IntoJs, Value};
use serde::{Deserialize, Serialize};
use std::ops::{Deref, DerefMut};

/// Serialize an instance of type `T` into a PetitJS value
pub fn to_value<T: Serialize>(data: &T) -> Result<Value, ValueError> {
    data.serialize(&Serializer)
}

/// Deserialize an instance of type `T` from a PetitJS value
pub fn from_value<'de, T: Deserialize<'de>>(
    value: Value,
) -> Result<T, ValueError> {
    T::deserialize(Deserializer::new(value))
}

/// Helper for converting a value to/from JS using its
/// [Serialize](serde::Serialize) and/or [Deserialize](serde::Deserialize)
/// implementations. This is useful when defining native functions that need
/// to convert their args and/or return value using `serde`.
pub struct SerdeJs<T>(pub T);

impl<T> SerdeJs<T> {
    /// Move the inner value out of this wrapper
    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<T> Deref for SerdeJs<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for SerdeJs<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: Serialize> IntoJs for SerdeJs<T> {
    fn into_js(self) -> Result<Value, ValueError> {
        to_value(&self.0)
    }
}

impl<'de, T: Deserialize<'de>> FromJs for SerdeJs<T> {
    fn from_js(value: Value) -> Result<Self, ValueError> {
        Ok(Self(from_value(value)?))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        compile::FunctionDefinitionId,
        execute::ProcessId,
        function::{Function, FunctionId},
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
    #[test_case(Function::new(
        FunctionId {
            process_id: ProcessId(0),
            definition_id: FunctionDefinitionId(0),
        },
        None,
        indexmap! {"cap".to_owned() => 32.into()},
    ); "function")]
    fn test_identity(value: impl Into<Value>) {
        let value = value.into();
        let deserialized: Value = from_value(value.clone()).unwrap();
        assert_eq!(deserialized, value, "Deserialization did not math");
        let serialized = to_value(&value).unwrap();
        assert_eq!(serialized, value, "Serialization did not match");
    }
}
