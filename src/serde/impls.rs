//! Custom implementations of `Serialize` and `Deserialize`. This is in its own
//! module so the implementations don't clutter the main value modules.

use crate::{
    serde::FunctionPool,
    value::{function::Function, Array, IntoPetit, Number, Object, Value},
};
use serde::{
    de::{
        self,
        value::{MapAccessDeserializer, SeqAccessDeserializer},
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
                i.into_petit().map_err(|_| todo!())
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

            fn visit_enum<A>(self, _data: A) -> Result<Self::Value, A::Error>
            where
                A: de::EnumAccess<'de>,
            {
                todo!("Create externally tagged enum")
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

impl Serialize for Number {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Number::Int(i) => i.serialize(serializer),
            Number::Float(f) => f.serialize(serializer),
        }
    }
}

impl<'de> Deserialize<'de> for Number {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        // TODO deserialize as int first if possible
        let number = f64::deserialize(deserializer)?;
        Ok(Number::Float(number))
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
                write!(f, "u64")
            }

            // The newtype struct gets unwrapped by the call below to
            // deserialize_newtype_struct. We expect to find a u64 inside which
            // is the ID of the function in the pool
            fn visit_u64<E>(self, id: u64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                FunctionPool::take(id).map_err(de::Error::custom)
            }
        }

        // Deserialize a function into a function: the deserializer put it in
        // the pool and created a newtype struct to hold the ID. We'll take the
        // ID and redeem it for the original function
        deserializer.deserialize_newtype_struct(
            FunctionPool::STRUCT_NAME,
            FunctionVisitor,
        )
    }
}
