//! Custom implementations of `Serialize` and `Deserialize`. This is in its own
//! module so the implementations don't clutter the main value modules.

use crate::{
    serde::FunctionPool,
    value::{function::Function, Array, Number, Object, Value},
};
use serde::{
    de::{
        self,
        value::{
            EnumAccessDeserializer, MapAccessDeserializer,
            SeqAccessDeserializer,
        },
        MapAccess, Visitor,
    },
    Deserialize, Serialize,
};
use std::fmt;

impl serde::Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            // undefined => (), null => None
            Value::Undefined => serializer.serialize_unit(),
            Value::Null => serializer.serialize_none(),
            Value::Boolean(b) => b.serialize(serializer),
            Value::Number(number) => number.serialize(serializer),
            Value::String(string) => string.serialize(serializer),
            Value::Array(array) => array.serialize(serializer),
            Value::Object(object) => object.serialize(serializer),
            #[cfg(feature = "buffer")]
            Value::Buffer(buffer) => buffer.serialize(serializer),
            Value::Function(function) => function.serialize(serializer),
        }
    }
}

impl<'de> serde::Deserialize<'de> for Value {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct ValueVisitor;

        impl<'de> Visitor<'de> for ValueVisitor {
            type Value = Value;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "any value")
            }

            fn visit_unit<E>(self) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Value::Undefined)
            }

            fn visit_none<E>(self) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Value::Null)
            }

            fn visit_bool<E>(self, b: bool) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Value::Boolean(b))
            }

            fn visit_i64<E>(self, i: i64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Value::Number(i.into()))
            }

            fn visit_u64<E>(self, i: u64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let number = Number::try_from(i).map_err(de::Error::custom)?;
                Ok(Value::Number(number))
            }

            fn visit_f64<E>(self, f: f64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Value::Number(f.into()))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Value::String(v.into()))
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Value::String(v.into()))
            }

            #[cfg(feature = "buffer")]
            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Value::Buffer(v.to_owned().into()))
            }

            #[cfg(feature = "buffer")]
            fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Value::Buffer(v.into()))
            }

            fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error>
            where
                A: de::SeqAccess<'de>,
            {
                Array::deserialize(SeqAccessDeserializer::new(seq))
                    .map(Value::Array)
            }

            fn visit_map<A>(self, map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                Object::deserialize(MapAccessDeserializer::new(map))
                    .map(Value::Object)
            }

            fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
            where
                A: de::EnumAccess<'de>,
            {
                // Deserialize into an externally tagged object:
                // {Variant1: {field1: 1, field2: 2}}
                Object::deserialize(EnumAccessDeserializer::new(data))
                    .map(Value::Object)
            }

            fn visit_newtype_struct<D>(
                self,
                deserializer: D,
            ) -> Result<Self::Value, D::Error>
            where
                D: de::Deserializer<'de>,
            {
                // TODO how to handle if this fails
                Function::deserialize(deserializer).map(Value::Function)
            }
        }

        deserializer.deserialize_any(ValueVisitor)
    }
}

impl Serialize for Function {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // Serialize a function into a function: we put it in the pool and the
        // serializer will take it back out
        let id = FunctionPool::insert(self.clone());
        serializer.serialize_newtype_struct(FunctionPool::STRUCT_NAME, &id)
    }
}

impl<'de> Deserialize<'de> for Function {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        struct FunctionVisitor;

        impl<'de> Visitor<'de> for FunctionVisitor {
            type Value = Function;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "newtype struct {}(u64)", FunctionPool::STRUCT_NAME)
            }

            // We end up hitting both of these visitor fns because of the
            // circuitous code path serde takes. I tried and failed to eliminate
            // one of them

            fn visit_u64<E>(self, id: u64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                FunctionPool::take(id).map_err(de::Error::custom)
            }

            fn visit_newtype_struct<D>(
                self,
                deserializer: D,
            ) -> Result<Self::Value, D::Error>
            where
                D: de::Deserializer<'de>,
            {
                let id = u64::deserialize(deserializer)?;
                self.visit_u64(id)
            }
        }

        // Deserialize a function into a function: the deserializer put it in
        // the pool and created a newtype struct to hold the ID. We'll take the
        // ID and redeem it for the original function.
        //
        // The struct name doesn't actually matter here because we can't check
        // it in visit_newtype_struct anyway. We're trusting that if a newtype
        // struct is actually present, it's the one put there for the function.
        // Otherwise we'll get an error trying to read the ID
        deserializer.deserialize_newtype_struct(
            FunctionPool::STRUCT_NAME,
            FunctionVisitor,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    /// Test serializing a function to an external data format. It should fail
    /// because we only support serialization back to PetitScript
    #[test]
    fn serialize_function_json() {
        let function = Function::native("func".into(), |_, _: Value| 0);
        serde_json::to_value(function).unwrap_err();
    }

    /// Test derializing a function from an external data format. It should fail
    /// because we only support deserialization from PetitScript
    #[test]
    fn deserialize_function_json() {
        // Insert a function into the pool. We *shouldn't* pull it out because
        // the JSON deserializer isn't allowed to touch that. This is totally
        // unrealistic because the JSON deserializer wouldn't be able to
        // populate the pool to begin with, but this makes the test a bit more
        // strenuous.
        let function = Function::native("func".into(), |_, _: Value| 0);
        let id = FunctionPool::insert(function);

        // This returns an error even though the ID is valid, because the pool
        // is for the PS deserializer ONLY
        serde_json::from_value::<Function>(json!(id)).unwrap_err();
    }
}
