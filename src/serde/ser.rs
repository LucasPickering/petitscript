use crate::{
    serde::{Error, Result},
    Value,
};
use indexmap::IndexMap;
use serde::ser;
use std::fmt::Display;

impl ser::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        todo!()
    }
}

/// TODO
pub struct Serializer {}

/// Serialize a `T` into `Value` using its `From<T>` impl
macro_rules! serialize_into {
    ($name:ident, $t:ty) => {
        #[inline]
        fn $name(self, value: $t) -> Result<Value> {
            Ok(value.into())
        }
    };
}

impl<'a> serde::Serializer for &'a Serializer {
    type Ok = Value;
    type Error = Error;
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
    serialize_into!(serialize_u64, u64);
    serialize_into!(serialize_i128, i128);
    serialize_into!(serialize_u128, u128);

    serialize_into!(serialize_f32, f32);
    serialize_into!(serialize_f64, f64);

    serialize_into!(serialize_char, char);
    serialize_into!(serialize_str, &str);

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok> {
        todo!()
    }

    fn serialize_none(self) -> Result<Self::Ok> {
        Ok(Value::Null)
    }

    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok>
    where
        T: ?Sized + serde::Serialize,
    {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok> {
        Ok(Value::Null)
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok> {
        Ok(Value::Null)
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok> {
        self.serialize_str(variant)
    }

    fn serialize_newtype_struct<T>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Self::Ok>
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
    ) -> Result<Self::Ok>
    where
        T: ?Sized + serde::Serialize,
    {
        todo!()
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq> {
        // Serialize the sequence into an array
        Ok(SerializeSeq::new(len))
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
        Ok(SerializeSeq::new(Some(len)))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        Ok(SerializeSeq::new(Some(len)))
    }

    fn serialize_tuple_variant(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        todo!()
    }

    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap> {
        Ok(SerializeMap::new(len))
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct> {
        Ok(SerializeMap::new(Some(len)))
    }

    fn serialize_struct_variant(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        todo!()
    }
}

/// Serialize a sequence into an array
struct SerializeSeq {
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

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + ser::Serialize,
    {
        let value = value.serialize(&Serializer {})?;
        self.values.push(value);
        Ok(())
    }
}

impl ser::SerializeSeq for SerializeSeq {
    type Ok = Value;
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + ser::Serialize,
    {
        self.serialize_element(value)
    }

    fn end(self) -> Result<Self::Ok> {
        Ok(self.values.into())
    }
}

impl ser::SerializeTuple for SerializeSeq {
    type Ok = Value;
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + ser::Serialize,
    {
        self.serialize_element(value)
    }

    fn end(self) -> Result<Self::Ok> {
        Ok(self.values.into())
    }
}

impl<'a> ser::SerializeTupleStruct for SerializeSeq {
    type Ok = Value;
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + ser::Serialize,
    {
        self.serialize_element(value)
    }

    fn end(self) -> Result<Self::Ok> {
        Ok(self.values.into())
    }
}

impl<'a> ser::SerializeTupleVariant for &'a Serializer {
    type Ok = Value;
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + ser::Serialize,
    {
        todo!()
    }

    fn end(self) -> Result<Self::Ok> {
        todo!()
    }
}

/// Serialize a map into an object
struct SerializeMap {
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

impl<'a> ser::SerializeMap for SerializeMap {
    type Ok = Value;
    type Error = Error;

    fn serialize_key<T>(&mut self, key: &T) -> Result<()>
    where
        T: ?Sized + ser::Serialize,
    {
        let key = key
            .serialize(&Serializer {})?
            .try_into_string()
            .expect("TODO");
        self.next_key = Some(key.into());
        Ok(())
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + ser::Serialize,
    {
        let key = self.next_key.take().expect("TODO");
        let value = value.serialize(&Serializer {})?;
        self.map.insert(key, value);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok> {
        Ok(self.map.into())
    }
}

impl<'a> ser::SerializeStruct for SerializeMap {
    type Ok = Value;
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + ser::Serialize,
    {
        let value = value.serialize(&Serializer {})?;
        self.map.insert(key.into(), value);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok> {
        Ok(self.map.into())
    }
}

impl<'a> ser::SerializeStructVariant for &'a Serializer {
    type Ok = Value;
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + ser::Serialize,
    {
        todo!()
    }

    fn end(self) -> Result<Self::Ok> {
        todo!()
    }
}
