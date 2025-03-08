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
        MapAccess, Unexpected, Visitor,
    },
    ser::SerializeStruct,
    Deserialize, Serialize,
};
use std::fmt;

impl Value {
    /// Deserialize this value into an arbitrary type, using the type's
    /// [Deserialize] implementation
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
    /// TODO
    pub(super) const STRUCT_NAME: &'static str = "__JsFunction";
    /// A marker field to indicate that a map is actually a function definition.
    /// The value is always [Self::STRUCT_NAME]
    pub(super) const FIELD_TYPE: &'static str = "__type";
    pub(super) const FIELD_ID: &'static str = "id";
    pub(super) const FIELD_NAME: &'static str = "name";
    pub(super) const FIELD_CAPTURES: &'static str = "captures";
    pub(super) const ALL_FIELDS: &'static [&'static str] = &[
        Self::FIELD_TYPE,
        Self::FIELD_ID,
        Self::FIELD_NAME,
        Self::FIELD_CAPTURES,
    ];
}

impl Serialize for Function {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // A function is serialized as a struct. In most formats this ends up
        // being the same as a map. We'll distinguish it from an object during
        // deserialization by the __type field
        let mut strct = serializer
            .serialize_struct(Self::STRUCT_NAME, Self::ALL_FIELDS.len())?;
        strct.serialize_field(Self::FIELD_TYPE, Self::STRUCT_NAME)?;
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
            /// This should always be set to [Function::STRUCT_NAME]
            type_: Field<String>,
            id: Field<FunctionId>,
            name: Field<Option<String>>,
            captures: Field<Captures>,
        }

        impl<'v> Visitor<'v> for FunctionVisitor {
            type Value = Function;

            fn expecting(&self, f: &mut fmt::Formatter) -> std::fmt::Result {
                write!(f, "map with the fields {:?}", Function::ALL_FIELDS)
            }

            fn visit_map<A>(
                mut self,
                mut map: A,
            ) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'v>,
            {
                while let Some(key) = map.next_key::<String>()? {
                    match key.as_str() {
                        Function::FIELD_TYPE => self.type_.set(&mut map)?,
                        Function::FIELD_ID => self.id.set(&mut map)?,
                        Function::FIELD_NAME => self.name.set(&mut map)?,
                        Function::FIELD_CAPTURES => {
                            self.captures.set(&mut map)?
                        }
                        key => {
                            return Err(de::Error::unknown_field(
                                key,
                                Function::ALL_FIELDS,
                            ));
                        }
                    }
                }

                // Make sure it had the __type field, and it held our name
                let type_ = self.type_.get()?;
                if type_ != Function::STRUCT_NAME {
                    return Err(de::Error::invalid_value(
                        Unexpected::Other(&format!(
                            "map with {} set to {type_}",
                            Function::FIELD_TYPE,
                        )),
                        &format!(
                            "map with {} set to {}",
                            Function::FIELD_TYPE,
                            Function::STRUCT_NAME
                        )
                        .as_str(),
                    ));
                }

                // If any of the fields weren't specified, we'll fail here
                Ok(Function::new(
                    self.id.get()?,
                    self.name.get()?,
                    self.captures.get()?,
                ))
            }
        }

        // We need to distinguish between a function and an object in serialized
        // formats, TODO
        deserializer.deserialize_map(FunctionVisitor {
            type_: Field::new(Function::FIELD_NAME),
            id: Field::new(Function::FIELD_ID),
            name: Field::new(Function::FIELD_NAME),
            captures: Field::new(Function::FIELD_CAPTURES),
        })
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
