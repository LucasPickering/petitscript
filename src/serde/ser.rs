#![allow(unused)] // TODO remove

use crate::{error::ValueError, IntoJs, Value};
use indexmap::IndexMap;
use serde::ser;
use std::fmt::Display;

impl ser::Error for ValueError {
    fn custom<T: Display>(msg: T) -> Self {
        ValueError::Custom(msg.to_string())
    }
}

/// TODO
pub struct Serializer;

/// Serialize a `T` into `Value` using its `From<T>` impl
macro_rules! serialize_into {
    ($name:ident, $t:ty) => {
        #[inline]
        fn $name(self, value: $t) -> Result<Value, ValueError> {
            Ok(value.into())
        }
    };
}

/// Serialize a `T` into `Value` using its `IntoJs` impl
macro_rules! serialize_into_fallible {
    ($name:ident, $t:ty) => {
        #[inline]
        fn $name(self, value: $t) -> Result<Value, ValueError> {
            value.into_js()
        }
    };
}

impl serde::Serializer for &Serializer {
    type Ok = Value;
    type Error = ValueError;
    type SerializeSeq = SerializeSeq;
    type SerializeTuple = SerializeSeq;
    type SerializeTupleStruct = SerializeSeq;
    type SerializeTupleVariant = Self;
    type SerializeMap = SerializeMap;
    type SerializeStruct = SerializeMap;
    type SerializeStructVariant = Self;

    serialize_into!(serialize_bool, bool);

    serialize_into!(serialize_i8, i8);
    serialize_into!(serialize_u8, u8);
    serialize_into!(serialize_i16, i16);
    serialize_into!(serialize_u16, u16);
    serialize_into!(serialize_i32, i32);
    serialize_into!(serialize_u32, u32);
    serialize_into!(serialize_i64, i64);
    // These are fallible because they may be out of range
    serialize_into_fallible!(serialize_u64, u64);
    serialize_into_fallible!(serialize_i128, i128);
    serialize_into_fallible!(serialize_u128, u128);

    serialize_into!(serialize_f32, f32);
    serialize_into!(serialize_f64, f64);

    serialize_into!(serialize_char, char);
    serialize_into!(serialize_str, &str);
    serialize_into!(serialize_bytes, &[u8]);

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Null)
    }

    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Null)
    }

    fn serialize_unit_struct(
        self,
        _name: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Null)
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        self.serialize_str(variant)
    }

    fn serialize_newtype_struct<T>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        todo!()
    }

    fn serialize_seq(
        self,
        len: Option<usize>,
    ) -> Result<Self::SerializeSeq, Self::Error> {
        // Serialize the sequence into an array
        Ok(SerializeSeq::new(len))
    }

    fn serialize_tuple(
        self,
        len: usize,
    ) -> Result<Self::SerializeTuple, Self::Error> {
        Ok(SerializeSeq::new(Some(len)))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Ok(SerializeSeq::new(Some(len)))
    }

    fn serialize_tuple_variant(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        todo!()
    }

    fn serialize_map(
        self,
        len: Option<usize>,
    ) -> Result<Self::SerializeMap, Self::Error> {
        Ok(SerializeMap::new(len))
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Ok(SerializeMap::new(Some(len)))
    }

    fn serialize_struct_variant(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        todo!()
    }
}

/// Serialize a sequence into an array
pub struct SerializeSeq {
    values: Vec<Value>,
}

impl SerializeSeq {
    fn new(len: Option<usize>) -> Self {
        let values = if let Some(len) = len {
            Vec::with_capacity(len)
        } else {
            Vec::new()
        };
        Self { values }
    }

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), ValueError>
    where
        T: ?Sized + ser::Serialize,
    {
        let value = value.serialize(&Serializer)?;
        self.values.push(value);
        Ok(())
    }
}

impl ser::SerializeSeq for SerializeSeq {
    type Ok = Value;
    type Error = ValueError;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + ser::Serialize,
    {
        self.serialize_element(value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.values.into())
    }
}

impl ser::SerializeTuple for SerializeSeq {
    type Ok = Value;
    type Error = ValueError;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + ser::Serialize,
    {
        self.serialize_element(value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.values.into())
    }
}

impl ser::SerializeTupleStruct for SerializeSeq {
    type Ok = Value;
    type Error = ValueError;

    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + ser::Serialize,
    {
        self.serialize_element(value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.values.into())
    }
}

impl ser::SerializeTupleVariant for &Serializer {
    type Ok = Value;
    type Error = ValueError;

    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + ser::Serialize,
    {
        todo!()
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        todo!()
    }
}

/// Serialize a map into an object
pub struct SerializeMap {
    map: IndexMap<String, Value>,
    next_key: Option<String>,
}

impl SerializeMap {
    fn new(len: Option<usize>) -> Self {
        let map = if let Some(len) = len {
            IndexMap::with_capacity(len)
        } else {
            IndexMap::new()
        };
        Self {
            map,
            next_key: None,
        }
    }
}

impl ser::SerializeMap for SerializeMap {
    type Ok = Value;
    type Error = ValueError;

    fn serialize_key<T>(&mut self, key: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + ser::Serialize,
    {
        let key = key.serialize(&Serializer)?.try_into_string().expect("TODO");
        self.next_key = Some(key.into());
        Ok(())
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + ser::Serialize,
    {
        let key = self.next_key.take().expect("TODO");
        let value = value.serialize(&Serializer)?;
        self.map.insert(key, value);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.map.into())
    }
}

impl ser::SerializeStruct for SerializeMap {
    type Ok = Value;
    type Error = ValueError;

    fn serialize_field<T>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error>
    where
        T: ?Sized + ser::Serialize,
    {
        let value = value.serialize(&Serializer)?;
        self.map.insert(key.into(), value);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.map.into())
    }
}

impl ser::SerializeStructVariant for &Serializer {
    type Ok = Value;
    type Error = ValueError;

    fn serialize_field<T>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error>
    where
        T: ?Sized + ser::Serialize,
    {
        todo!()
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        todo!()
    }
}
