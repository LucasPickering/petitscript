use crate::{
    error::ValueError,
    value::{
        function::{Function, FunctionInner},
        Number, Value,
    },
};
use indexmap::IndexMap;
use serde::de::{
    self,
    value::{
        MapDeserializer, SeqDeserializer, StrDeserializer, StringDeserializer,
    },
    Deserializer as _, IntoDeserializer, Unexpected,
};
use serde_json::json;
use std::fmt::Display;

impl de::Error for ValueError {
    fn custom<T: Display>(message: T) -> Self {
        ValueError::Custom(message.to_string())
    }
}

impl IntoDeserializer<'_, ValueError> for Value {
    type Deserializer = Deserializer;

    fn into_deserializer(self) -> Deserializer {
        Deserializer { value: self }
    }
}

/// Deserialize from [Value] to any type that implements
/// [Deserialize](serde::Deserialize)
pub struct Deserializer {
    value: Value,
}

impl Deserializer {
    /// Create a new deserializer to convert from the given value
    pub fn new(value: Value) -> Self {
        Self { value }
    }
}

impl<'de> serde::Deserializer<'de> for Deserializer {
    type Error = ValueError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // TODO either find a way to leverage owned values, or take a ref
        // instead
        match self.value {
            // Undefined => (), null => None
            Value::Undefined => visitor.visit_unit(),
            Value::Null => visitor.visit_none(),
            Value::Boolean(b) => visitor.visit_bool(b),
            Value::Number(Number::Int(i)) => visitor.visit_i64(i),
            Value::Number(Number::Float(f)) => visitor.visit_f64(f),
            // It'd be nice to be able to reuse the allocated string if we own
            // the last copy of the wrapping Arc, but I can't find a way to do
            // that since str is unsized, so we have to clone all the data
            Value::String(string) => visitor.visit_str(&string),
            Value::Array(array) => {
                visitor.visit_seq(&mut SeqDeserializer::new(array.into_iter()))
            }
            Value::Object(object) => {
                visitor.visit_map(&mut MapDeserializer::new(object.into_iter()))
            }
            #[cfg(feature = "buffer")]
            // TODO can we support zero-copy here?
            Value::Buffer(buffer) => visitor.visit_bytes(&buffer),
            // Functions are represented as a map of static fields, with a
            // __type field set to a specific value, to distinguish this from an
            // object. It's not perfect, but it's very unlikely to collide with
            // a real object
            // TODO update comment
            Value::Function(function) => {
                visitor.visit_enum(FunctionEnumDeserializer { function })
            }
        }
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        match self.value {
            Value::Undefined | Value::Null => visitor.visit_none(),
            _ => visitor.visit_some(self),
        }
    }

    fn deserialize_tuple<V>(
        self,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // TODO can we get rid of this?
        match self.value {
            Value::Object(object) => {
                // Deserialize an externally tagged enum
                let mut map: IndexMap<_, _> = object.into();
                // We expect a map with exactly one entry: {key: value}
                if map.len() != 1 {
                    return Err(de::Error::invalid_length(
                        map.len(),
                        // TODO include variant names in error msg
                        &"map of length 1 for an externally tagged enum",
                    ));
                }
                let (variant, value) = map.swap_remove_index(0).unwrap();
                visitor.visit_enum(EnumDeserializer {
                    variant,
                    value: Some(value),
                })
            }
            Value::String(variant) => visitor.visit_enum(EnumDeserializer {
                variant: variant.into(),
                value: None,
            }),
            _ => Err(de::Error::invalid_type(
                // TODO break this out into different variants of Unexpected
                Unexpected::Other("TODO"),
                &"string or object",
            )),
        }
    }

    serde::forward_to_deserialize_any! {
        unit bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str
        string bytes byte_buf identifier ignored_any unit_struct newtype_struct
        struct map seq
    }
}

impl<'de> de::VariantAccess<'de> for Deserializer {
    type Error = ValueError;

    // If the `Visitor` expected this variant to be a unit variant, the input
    // should have been the plain string case handled in `deserialize_enum`.
    fn unit_variant(self) -> Result<(), Self::Error> {
        todo!()
    }

    // Newtype variants are represented in JSON as `{ NAME: VALUE }` so
    // deserialize the value here.
    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        seed.deserialize(self)
    }

    // Tuple variants are represented in JSON as `{ NAME: [DATA...] }` so
    // deserialize the sequence of data here.
    fn tuple_variant<V>(
        self,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    // Struct variants are represented in JSON as `{ NAME: { K: V, ... } }` so
    // deserialize the inner map here.
    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }
}

/// Deserialize an enum
struct EnumDeserializer {
    variant: String,
    value: Option<Value>,
}

impl<'de> de::EnumAccess<'de> for EnumDeserializer {
    type Error = ValueError;
    type Variant = VariantDeserializer;

    fn variant_seed<V>(
        self,
        seed: V,
    ) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let key = seed.deserialize(StringDeserializer::<Self::Error>::new(
            self.variant,
        ))?;
        Ok((key, VariantDeserializer { value: self.value }))
    }
}

/// Deserialize an enum variant value
struct VariantDeserializer {
    value: Option<Value>,
}

impl<'de> de::VariantAccess<'de> for VariantDeserializer {
    type Error = ValueError;

    fn unit_variant(self) -> Result<(), Self::Error> {
        match self.value {
            Some(_) => Err(de::Error::invalid_type(
                de::Unexpected::NewtypeVariant,
                &"unit variant",
            )),
            None => Ok(()),
        }
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        match self.value {
            Some(value) => seed.deserialize(value.into_deserializer()),
            None => Err(de::Error::invalid_type(
                de::Unexpected::UnitVariant,
                &"newtype variant",
            )),
        }
    }

    fn tuple_variant<V>(
        self,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        match self.value {
            Some(value) => serde::Deserializer::deserialize_seq(
                value.into_deserializer(),
                visitor,
            ),
            None => Err(de::Error::invalid_type(
                de::Unexpected::UnitVariant,
                &"tuple variant",
            )),
        }
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        match self.value {
            Some(value) => serde::Deserializer::deserialize_map(
                value.into_deserializer(),
                visitor,
            ),
            None => Err(de::Error::invalid_type(
                de::Unexpected::UnitVariant,
                &"struct variant",
            )),
        }
    }
}

/// TODO
struct FunctionEnumDeserializer {
    /// TODO
    function: Function,
}

impl<'de> de::EnumAccess<'de> for FunctionEnumDeserializer {
    type Error = ValueError;
    type Variant = Self;

    fn variant_seed<V>(
        self,
        seed: V,
    ) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let variant = seed.deserialize(StrDeserializer::<Self::Error>::new(
            self.function.serde_type(),
        ))?;
        Ok((variant, self))
    }
}

// We'll produce a map of the static fields in a function
impl<'de> de::VariantAccess<'de> for FunctionEnumDeserializer {
    type Error = ValueError;

    fn unit_variant(self) -> Result<(), Self::Error> {
        // A function can only be a struct variant
        Err(de::Error::invalid_type(
            Unexpected::StructVariant,
            &"unit variant",
        ))
    }

    fn newtype_variant_seed<T>(self, _seed: T) -> Result<T::Value, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        // A function can only be a struct variant
        Err(de::Error::invalid_type(
            Unexpected::StructVariant,
            &"newtype variant",
        ))
    }

    fn tuple_variant<V>(
        self,
        _len: usize,
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // A function can only be a struct variant
        Err(de::Error::invalid_type(
            Unexpected::StructVariant,
            &"tuple variant",
        ))
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // TODO clean up
        let json = match &*self.function.0 {
            FunctionInner::User {
                definition,
                name,
                captures,
            } => {
                json!({
                    "definition": serde_json::to_value(definition).unwrap(),
                    "name": serde_json::to_value(name).unwrap(),
                    "captures": serde_json::to_value(captures).unwrap(),
                })
            }
            FunctionInner::Native { id, name } => json!({
                "id": serde_json::to_value(id).unwrap(),
                "name": serde_json::to_value(name).unwrap(),
            }),
            FunctionInner::Bound { id, receiver, name } => json!({
                "id": serde_json::to_value(id).unwrap(),
                "receiver": serde_json::to_value(receiver).unwrap(),
                "name": serde_json::to_value(name).unwrap(),
            }),
        };
        json.deserialize_any(visitor).map_err(ValueError::other)
    }
}
