use crate::{
    error::ValueError,
    serde::FunctionPool,
    value::{Number, Value, ValueType},
};
use indexmap::IndexMap;
use serde::de::{
    self,
    value::{
        MapDeserializer, SeqDeserializer, StringDeserializer, U64Deserializer,
    },
    IntoDeserializer, Unexpected,
};
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
        match self.value {
            // Undefined => (), null => None
            Value::Undefined => visitor.visit_unit(),
            Value::Null => visitor.visit_none(),
            Value::Boolean(b) => visitor.visit_bool(b),
            Value::Number(Number::Int(i)) => visitor.visit_i64(i),
            Value::Number(Number::Float(f)) => visitor.visit_f64(f),
            Value::String(string) => visitor.visit_string(string.into()),
            Value::Array(array) => {
                visitor.visit_seq(&mut SeqDeserializer::new(array.into_iter()))
            }
            Value::Object(object) => {
                visitor.visit_map(&mut MapDeserializer::new(object.into_iter()))
            }
            #[cfg(feature = "buffer")]
            Value::Buffer(buffer) => visitor.visit_byte_buf(buffer.into()),
            Value::Function(_) => self
                .deserialize_newtype_struct(FunctionPool::STRUCT_NAME, visitor),
        }
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        match self.value {
            // None serializes as null, but we accept undefined here too because
            // it's pretty intuitive
            Value::Undefined | Value::Null => visitor.visit_none(),
            _ => visitor.visit_some(self),
        }
    }

    fn deserialize_newtype_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        if name == FunctionPool::STRUCT_NAME {
            // If the Deserialize impl specifically requested our special
            // function name, we assume it's Function's impl. That means two
            // things:
            // - We need a function
            // - We're going to use out-of-band signalling
            let Value::Function(function) = self.value else {
                return Err(ValueError::Type {
                    expected: ValueType::Function,
                    actual: self.value.type_(),
                });
            };
            let id = FunctionPool::insert(function);
            visitor.visit_newtype_struct(U64Deserializer::new(id))
        } else {
            visitor.visit_newtype_struct(self)
        }
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        let unexpected = match self.value {
            Value::Object(object) => {
                // Deserialize an externally tagged enum
                let mut map: IndexMap<_, _> = object.into();
                // We expect a map with exactly one entry: {key: value}
                if map.len() != 1 {
                    return Err(de::Error::invalid_length(
                        map.len(),
                        &"map of length 1 for an externally tagged enum",
                    ));
                }
                let (variant, value) = map.swap_remove_index(0).unwrap();
                if !variants.contains(&variant.as_str()) {
                    return Err(de::Error::unknown_variant(&variant, variants));
                }
                return visitor.visit_enum(EnumDeserializer { variant, value });
            }
            Value::String(variant) => {
                if !variants.contains(&&*variant) {
                    return Err(de::Error::unknown_variant(&variant, variants));
                }
                return visitor.visit_enum(EnumDeserializer {
                    variant: variant.into(),
                    value: Value::Undefined,
                });
            }
            Value::Undefined => Unexpected::Other("undefined"),
            Value::Null => Unexpected::Other("null"),
            Value::Boolean(b) => Unexpected::Bool(b),
            Value::Number(Number::Int(int)) => Unexpected::Signed(int),
            Value::Number(Number::Float(float)) => Unexpected::Float(float),
            Value::Array(_) => Unexpected::Seq,
            #[cfg(feature = "buffer")]
            Value::Buffer(_) => Unexpected::Other("bytes"),
            Value::Function(_) => Unexpected::Other("function"),
        };
        Err(de::Error::invalid_type(unexpected, &"string or object"))
    }

    serde::forward_to_deserialize_any! {
        unit bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str
        string bytes byte_buf identifier ignored_any unit_struct struct map seq
        tuple tuple_struct
    }
}

/// Deserialize an enum
struct EnumDeserializer {
    /// Name of the variant
    variant: String,
    /// Associated value
    value: Value,
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
    /// - `Undefined` for unit variants
    /// - `Array` for tuple variants
    /// - `Object` for struct variants
    /// - Any `Value` for newtype variants (including above values)
    ///
    /// The Deserialize impl will tell us which variant type to look for, so
    /// the overlap between the cases isn't a problem
    value: Value,
}

impl<'de> de::VariantAccess<'de> for VariantDeserializer {
    type Error = ValueError;

    fn unit_variant(self) -> Result<(), Self::Error> {
        let unexpected = match self.value {
            Value::Undefined => return Ok(()),
            // Null is *not* treated like a unit variant. It could be, but I
            // decided to start with more conservative behavior.
            Value::Object(_) => de::Unexpected::StructVariant,
            Value::Array(_) => de::Unexpected::TupleVariant,
            // Anything else looks like a newtype variant
            _ => de::Unexpected::NewtypeVariant,
        };
        Err(de::Error::invalid_type(unexpected, &"unit variant"))
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        // Any value can be in a newtype including undefined, object or array
        seed.deserialize(self.value.into_deserializer())
    }

    fn tuple_variant<V>(
        self,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        let unexpected = match self.value {
            Value::Array(_) => {
                return serde::Deserializer::deserialize_seq(
                    self.value.into_deserializer(),
                    visitor,
                )
            }
            Value::Undefined => Unexpected::UnitVariant,
            Value::Object(_) => Unexpected::StructVariant,
            _ => Unexpected::NewtypeVariant,
        };
        Err(de::Error::invalid_type(unexpected, &"tuple variant"))
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        let unexpected = match self.value {
            Value::Object(_) => {
                return serde::Deserializer::deserialize_map(
                    self.value.into_deserializer(),
                    visitor,
                )
            }
            Value::Undefined => Unexpected::UnitVariant,
            Value::Array(_) => Unexpected::TupleVariant,
            _ => Unexpected::NewtypeVariant,
        };
        Err(de::Error::invalid_type(unexpected, &"struct variant"))
    }
}
