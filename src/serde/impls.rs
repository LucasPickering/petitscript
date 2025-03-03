//! Custom implementations of `Serialize` and `Deserialize`. This is in its own
//! module so the implementations don't clutter the main value modules.

use crate::{
    compile::FunctionDefinitionId,
    error::ValueError,
    execute::ProcessId,
    function::{Captures, Function, FunctionId},
    serde::from_value,
    Array, IntoJs, Number, Object, Value,
};
use serde::{
    de::{
        self,
        value::{MapAccessDeserializer, SeqAccessDeserializer},
        MapAccess, Visitor,
    },
    ser::SerializeStruct,
    Deserialize, Serialize,
};
use std::fmt;

impl Function {
    /// TODO
    pub(super) const SERDE_NAME: &'static str = "__Function";
    pub(super) const FIELD_ID: &'static str = "id";
    pub(super) const FIELD_NAME: &'static str = "name";
    pub(super) const FIELD_CAPTURES: &'static str = "captures";
    pub(super) const ALL_FIELDS: &'static [&'static str] =
        &[Self::FIELD_ID, Self::FIELD_NAME, Self::FIELD_CAPTURES];
}

impl Value {
    /// Deserialize this value into an arbitrary type, using the type's
    /// [Deserialize](serde::Deserialize) implementation
    pub fn deserialize<'de, T: Deserialize<'de>>(
        self,
    ) -> Result<T, ValueError> {
        from_value(self)
    }
}

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
            Value::Buffer(buffer) => buffer.serialize(serializer),
            Value::Function(function) => function.serialize(serializer),
            Value::Native(_) => todo!("not supported?"),
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
                i.into_js().map_err(|_| todo!())
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

            #[cfg(feature = "bytes")]
            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Value::Buffer(v.into()))
            }

            #[cfg(feature = "bytes")]
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

            fn visit_newtype_struct<D>(
                self,
                deserializer: D,
            ) -> Result<Self::Value, D::Error>
            where
                D: de::Deserializer<'de>,
            {
                // TODO explain
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
        let number = f64::deserialize(deserializer)?;
        Ok(Number::Float(number))
    }
}

impl Serialize for Function {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // TODO explain
        let mut strct = serializer.serialize_struct(Self::SERDE_NAME, 3)?;
        strct.serialize_field(Self::FIELD_ID, &self.id())?;
        strct.serialize_field(Self::FIELD_NAME, &self.name())?;
        strct.serialize_field(Self::FIELD_CAPTURES, self.captures())?;
        strct.end()
    }
}

impl<'de> Deserialize<'de> for Function {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct FunctionVisitor {
            id: Field<FunctionId>,
            name: Field<Option<String>>,
            captures: Field<Captures>,
        }

        impl<'v> Visitor<'v> for FunctionVisitor {
            type Value = Function;

            fn expecting(&self, f: &mut fmt::Formatter) -> std::fmt::Result {
                write!(f, "struct with the fields {:?}", Function::ALL_FIELDS)
            }

            fn visit_newtype_struct<D>(
                self,
                deserializer: D,
            ) -> Result<Self::Value, D::Error>
            where
                D: de::Deserializer<'v>,
            {
                deserializer
                    .deserialize_newtype_struct(Function::SERDE_NAME, self)
            }

            fn visit_map<A>(
                mut self,
                mut map: A,
            ) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'v>,
            {
                while let Some(key) = map.next_key()? {
                    match key {
                        Function::FIELD_ID => self.id.set(&mut map)?,
                        Function::FIELD_NAME => self.name.set(&mut map)?,
                        Function::FIELD_CAPTURES => {
                            self.captures.set(&mut map)?
                        }
                        _ => {
                            return Err(de::Error::unknown_field(
                                key,
                                Function::ALL_FIELDS,
                            ));
                        }
                    }
                }
                // If any of the fields weren't specified, we'll fail here
                Ok(Function::new(
                    self.id.get()?,
                    self.name.get()?,
                    self.captures.get()?,
                ))
            }
        }

        // Functions can only be deserialized directly from function values,
        // since they need to be bound to an active process. Tell the
        // deserializer we'll only accept an exact struct match
        // TODO update comment about newtype structs ^
        deserializer.deserialize_newtype_struct(
            Self::SERDE_NAME,
            FunctionVisitor {
                id: Field::new(Function::FIELD_ID),
                name: Field::new(Function::FIELD_NAME),
                captures: Field::new(Function::FIELD_CAPTURES),
            },
        )
    }
}

impl FunctionId {
    /// Pack this compound ID into a single u64 for serialization. The top 32
    /// bits are the process ID, bottom 32 bits are the function definition ID.
    /// It this a good idea? Who knows, but it's fun!
    pub(super) fn pack(&self) -> u64 {
        ((self.process_id.0 as u64) << 32) | (self.definition_id.0 as u64)
    }

    /// Unpack a u64 into a compound ID
    pub(super) fn unpack(packed: u64) -> Self {
        let process_id = (packed >> 32) as u32; // Top 32 bits
        let definition_id = (packed & 0xFFFF_FFFF) as u32; // Bottom 32 bits
        Self {
            process_id: ProcessId(process_id),
            definition_id: FunctionDefinitionId(definition_id),
        }
    }
}

impl Serialize for FunctionId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.pack().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for FunctionId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let packed = u64::deserialize(deserializer)?;
        Ok(Self::unpack(packed))
    }
}

/// TODO
struct Field<T> {
    name: &'static str,
    value: Option<T>,
}

impl<T> Field<T> {
    fn new(name: &'static str) -> Self {
        Self { name, value: None }
    }

    fn set<'de, A>(&mut self, map: &mut A) -> Result<(), A::Error>
    where
        T: Deserialize<'de>,
        A: MapAccess<'de>,
    {
        if self.value.is_some() {
            return Err(de::Error::duplicate_field(Function::FIELD_ID));
        }
        self.value = Some(map.next_value()?);
        Ok(())
    }

    fn get<E: de::Error>(self) -> Result<T, E> {
        self.value
            .ok_or_else(|| de::Error::missing_field(self.name))
    }
}
