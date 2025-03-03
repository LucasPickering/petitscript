use crate::{
    error::ValueError,
    function::{Captures, Function, FunctionId},
    Array, Number, Object, Value,
};
use indexmap::IndexMap;
use serde::de::{
    self,
    value::{StrDeserializer, StringDeserializer, U64Deserializer},
    Deserializer as _, Error as _, IntoDeserializer,
};
use std::fmt::Display;

impl de::Error for ValueError {
    fn custom<T: Display>(msg: T) -> Self {
        ValueError::Custom(msg.to_string())
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
            Value::Array(_) => self.deserialize_seq(visitor),
            Value::Object(_) => self.deserialize_map(visitor),
            #[cfg(feature = "bytes")]
            // TODO can we support zero-copy here?
            Value::Buffer(buffer) => visitor.visit_bytes(&buffer),
            Value::Function(function) => FunctionDeserializer::new(function)
                .deserialize_newtype_struct(Function::SERDE_NAME, visitor),
            Value::Native(_) => todo!("not supported"),
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

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        let array = self.value.try_into_array()?;
        let mut deserializer = SeqDeserializer::new(array);
        visitor.visit_seq(&mut deserializer)
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

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        let object = self.value.try_into_object()?;
        let mut deserializer = MapDeserializer::new(object);
        visitor.visit_map(&mut deserializer)
    }

    fn deserialize_newtype_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        println!("Deserializer::deserialize_newtype_struct");
        if name == Function::SERDE_NAME {
            // Caller has asked for a function, let's see if we have one
            let function = self.value.try_into_function()?;
            // We have a function. Serialize it as a map (a sequence of keys and
            // values) and let the visitor recompose it back into a function
            let deserializer = FunctionDeserializer::new(function);
            visitor.visit_map(deserializer)
        } else {
            // User asked for an unknown struct type - treat it as a regular map
            self.deserialize_map(visitor)
        }
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
        match self.value {
            Value::Undefined | Value::Null => {
                todo!("deserialize unit enum")
            }
            Value::Object(object) => {
                // Deserialize an externally tagged enum
                visitor.visit_enum(EnumDeserializer::new(object))
            }
            _ => todo!("expected enum"),
        }
    }

    serde::forward_to_deserialize_any! {
        unit bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str
        string bytes byte_buf identifier ignored_any unit_struct struct
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

struct SeqDeserializer {
    /// Array holds a vec internally, so we can reuse its iterator
    iter: <Vec<Value> as IntoIterator>::IntoIter,
}

impl SeqDeserializer {
    fn new(array: Array) -> Self {
        Self {
            iter: Vec::from(array).into_iter(),
        }
    }
}

impl<'de> de::SeqAccess<'de> for SeqDeserializer {
    type Error = ValueError;

    fn next_element_seed<T>(
        &mut self,
        seed: T,
    ) -> Result<Option<T::Value>, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        if let Some(value) = self.iter.next() {
            seed.deserialize(value.into_deserializer()).map(Some)
        } else {
            Ok(None)
        }
    }

    fn size_hint(&self) -> Option<usize> {
        // Vec's iterator always knows its exact size
        Some(self.iter.size_hint().0)
    }
}

struct MapDeserializer {
    /// Object holds an IndexMap internally, so we can reuse its iterator
    iter: <IndexMap<String, Value> as IntoIterator>::IntoIter,
    /// TODO
    next_value: Option<Value>,
}

impl MapDeserializer {
    fn new(object: Object) -> Self {
        Self {
            iter: IndexMap::from(object).into_iter(),
            next_value: None,
        }
    }
}

impl<'de> de::MapAccess<'de> for MapDeserializer {
    type Error = ValueError;

    fn next_key_seed<K>(
        &mut self,
        seed: K,
    ) -> Result<Option<K::Value>, Self::Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        if let Some((key, value)) = self.iter.next() {
            self.next_value = Some(value);
            let key = seed.deserialize(Deserializer::new(key.into()))?;
            Ok(Some(key))
        } else {
            Ok(None)
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let value = self
            .next_value
            .take()
            .ok_or_else(|| ValueError::custom("Value requested before key"))?;
        seed.deserialize(value.into_deserializer())
    }

    fn size_hint(&self) -> Option<usize> {
        // IndexMap's iterator always knows its exact size
        // TODO note about half entry
        Some(self.iter.size_hint().0)
    }
}

struct EnumDeserializer {
    map: IndexMap<String, Value>,
}

impl EnumDeserializer {
    fn new(object: Object) -> Self {
        Self { map: object.into() }
    }
}

impl<'de> de::EnumAccess<'de> for EnumDeserializer {
    type Error = ValueError;
    type Variant = Deserializer;

    fn variant_seed<V>(
        mut self,
        seed: V,
    ) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        // Enums should be externally tagged values in an object, like:
        // {key: value}
        // So grab the first key in the object, and deserialize its value
        let (key, value) = self
            .map
            .swap_remove_index(0)
            .ok_or_else(|| ValueError::custom("TODO"))?;
        let key = seed.deserialize(Value::from(key).into_deserializer())?;
        Ok((key, value.into_deserializer()))
    }
}

/// TODO
struct FunctionDeserializer {
    // TODO explain options
    id: FunctionId,
    name: Option<String>,
    captures: Option<Captures>,
    /// TODO
    next_field: usize,
}

impl FunctionDeserializer {
    fn new(function: Function) -> Self {
        let (id, name, captures) = function.into_parts();
        Self {
            id,
            name,
            captures: Some(captures),
            next_field: 0,
        }
    }
}

impl<'de> de::Deserializer<'de> for FunctionDeserializer {
    type Error = ValueError;

    fn deserialize_any<V>(self, _: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        todo!("error")
    }

    fn deserialize_newtype_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        println!("FunctionDeserializer::deserialize_newtype_struct");
        // TODO explain
        if name == Function::SERDE_NAME {
            visitor.visit_map(self)
        } else {
            self.deserialize_any(visitor)
        }
    }

    serde::forward_to_deserialize_any! {
        unit bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str
        string bytes byte_buf identifier ignored_any unit_struct option seq map
        tuple tuple_struct struct enum
    }
}

// We'll produce a map of the static fields in a function
impl<'de> de::MapAccess<'de> for FunctionDeserializer {
    type Error = ValueError;

    fn next_key_seed<K>(
        &mut self,
        seed: K,
    ) -> Result<Option<K::Value>, Self::Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        // Emit the next field in the sequence
        let Some(field) = Function::ALL_FIELDS.get(self.next_field) else {
            return Ok(None);
        };
        seed.deserialize(StrDeserializer::new(field)).map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let field = Function::ALL_FIELDS
            .get(self.next_field)
            .ok_or_else(|| ValueError::custom("TODO impossible"))?;
        self.next_field += 1;
        match *field {
            Function::FIELD_ID => {
                // Pack the ID into a single u64
                seed.deserialize(U64Deserializer::new(self.id.pack()))
            }
            Function::FIELD_NAME => {
                if let Some(name) = self.name.take() {
                    seed.deserialize(StringDeserializer::new(name))
                } else {
                    // TODO this probably isn't right - we should serialize
                    // None instead of ()
                    seed.deserialize(().into_deserializer())
                }
            }
            Function::FIELD_CAPTURES => {
                if let Some(captures) = self.captures.take() {
                    seed.deserialize(captures.into_deserializer())
                } else {
                    todo!("error!")
                }
            }
            _ => todo!(),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        Some(Function::ALL_FIELDS.len() - self.next_field)
    }
}
