//! Custom implementations of `Serialize` and `Deserialize`. This is in its own
//! module so the implementations don't clutter the main value modules.

use crate::value::{
    function::Function, Array, IntoPetit, Number, Object, Value,
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
                #[derive(Deserialize)]
                #[serde(untagged)]
                enum FunctionOrObject {
                    Function(Function),
                    Object(Object),
                }

                // A map could hold a function OR an object. Try it as a
                // function first because it's much more specific, then fall
                // back to an object. We leverage serde's generated impl on the
                // enum to buffer the deserialized object. This is potentially
                // a bottleneck for deserializing objects; if so we can
                // deserialize directly into an IndexMap, then check if it looks
                // like a function
                let deserialized = FunctionOrObject::deserialize(
                    MapAccessDeserializer::new(map),
                )?;
                Ok(match deserialized {
                    FunctionOrObject::Function(function) => function.into(),
                    FunctionOrObject::Object(object) => object.into(),
                })
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

impl Function {
    // TODO explain
    // TODO include note about how these have to match the attributes on
    // FunctionInner

    /// Value to use for the type field for user functions
    pub(super) const TYPE_USER: &'static str = "__UserFunction";
    /// Value to use for the type field for native functions
    pub(super) const TYPE_NATIVE: &'static str = "__NativeFunction";
    /// Value to use for the type field for bound functions, which are
    /// native functions bound to a receiver
    pub(super) const TYPE_BOUND: &'static str = "__BoundFunction";
    /// A marker field to indicate that a map is actually a function definition.
    /// The value is one of the above types
    pub(super) const FIELD_TYPE: &'static str = "__type";
    pub(super) const FIELD_ID: &'static str = "id";
    pub(super) const FIELD_DEFINITION: &'static str = "definition";
    pub(super) const FIELD_NAME: &'static str = "name";
    pub(super) const FIELD_CAPTURES: &'static str = "captures";
    pub(super) const FIELD_RECEIVER: &'static str = "receiver";
    pub(super) const USER_FIELDS: &'static [&'static str] = &[
        Self::FIELD_TYPE,
        Self::FIELD_DEFINITION,
        Self::FIELD_NAME,
        Self::FIELD_CAPTURES,
    ];
    pub(super) const NATIVE_FIELDS: &'static [&'static str] =
        &[Self::FIELD_TYPE, Self::FIELD_ID, Self::FIELD_NAME];
    pub(super) const BOUND_FIELDS: &'static [&'static str] = &[
        Self::FIELD_TYPE,
        Self::FIELD_ID,
        Self::FIELD_RECEIVER,
        Self::FIELD_NAME,
    ];
}
