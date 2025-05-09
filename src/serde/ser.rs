use crate::{
    error::ValueError,
    serde::FunctionPool,
    value::{IntoPetit, Object},
    Value,
};
use indexmap::IndexMap;
use serde::ser::{self, SerializeStruct};
use std::fmt::Display;

impl ser::Error for ValueError {
    fn custom<T: Display>(message: T) -> Self {
        ValueError::Custom(message.to_string())
    }
}

/// Serialize any type that implements [Serialize](serde::Serialize) into a
/// [Value]
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

/// Serialize a `T` into `Value` using its `IntoPetit` impl
macro_rules! serialize_into_fallible {
    ($name:ident, $t:ty) => {
        #[inline]
        fn $name(self, value: $t) -> Result<Value, ValueError> {
            value.into_petit()
        }
    };
}

impl serde::Serializer for Serializer {
    type Ok = Value;
    type Error = ValueError;
    type SerializeSeq = SerializeSeq;
    type SerializeTuple = SerializeSeq;
    type SerializeTupleStruct = SerializeSeq;
    type SerializeTupleVariant = SerializeVariant<SerializeSeq>;
    type SerializeMap = SerializeMap;
    type SerializeStruct = SerializeMap;
    type SerializeStructVariant = SerializeVariant<SerializeMap>;

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

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Undefined)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Null)
    }

    #[cfg(feature = "buffer")]
    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Buffer(v.to_owned().into()))
    }

    #[cfg(not(feature = "buffer"))]
    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        // TODO write a test for this case
        Ok(Value::Array(v.to_owned().into()))
    }

    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        value.serialize(self)
    }

    fn serialize_unit_struct(
        self,
        _name: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Undefined)
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
        name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        if name == FunctionPool::STRUCT_NAME {
            // A function can only be serialized from a function. The Serialize
            // impl should've put it into the function pool and we'll grab it
            // back out by ID
            let id: u64 = value.serialize(U64Serializer)?;
            // The only way we can hit an error here is:
            // - A bug in our code somewhere (impossible)
            // - User tried to serialize something that looks like a Function
            //   but isn't (i.e. serializes as a struct with our special name),
            //   which is extremely unlikely
            let function = FunctionPool::take(id)?;
            Ok(Value::Function(function))
        } else {
            value.serialize(self)
        }
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
        // Wrap the inner value in an external tag object: {Variant: inner}
        let value = value.serialize(self)?;
        Ok(Object::new().insert(variant, value).into())
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
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Ok(SerializeVariant {
            variant,
            inner: SerializeSeq::new(Some(len)),
        })
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
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Ok(SerializeVariant {
            variant,
            inner: SerializeMap::new(Some(len)),
        })
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
        let value = value.serialize(Serializer)?;
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
        // Keys must be strings
        let key = key.serialize(Serializer)?.try_into_string()?;
        self.next_key = Some(key.into());
        Ok(())
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + ser::Serialize,
    {
        let key = self.next_key.take().expect("TODO");
        let value = value.serialize(Serializer)?;
        self.map.insert(key, value);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.map.into())
    }
}

/// Serialize structs as maps
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
        // Serialize the inner &T to Value
        let value = value.serialize(Serializer)?;
        self.map.insert(key.to_owned(), value);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.map.into())
    }
}

/// Serialize an enum object with an external tag:
///
/// `{Tuple: [1, 2]}` or `{Struct: {a: 1}}`
pub struct SerializeVariant<T> {
    variant: &'static str,
    inner: T,
}

/// Serialize tuple variants as arrays
impl ser::SerializeTupleVariant for SerializeVariant<SerializeSeq> {
    type Ok = Value;
    type Error = ValueError;

    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + ser::Serialize,
    {
        self.inner.serialize_element(value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        // Wrap the inner array in an external tag object: {Variant: inner}
        Ok(Object::new().insert(self.variant, self.inner.values).into())
    }
}

/// Serialize struct variants as objects
impl ser::SerializeStructVariant for SerializeVariant<SerializeMap> {
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
        self.inner.serialize_field(key, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        // Wrap the inner map in an external tag object: {Variant: inner}
        Ok(Object::new().insert(self.variant, self.inner.map).into())
    }
}

/// TODO explain or figure out a way to get rid of this
struct U64Serializer;

#[allow(unused)] // TODO remove this
impl ser::Serializer for U64Serializer {
    type Ok = u64;
    type Error = ValueError;
    type SerializeSeq = ser::Impossible<u64, ValueError>;
    type SerializeTuple = ser::Impossible<u64, ValueError>;
    type SerializeTupleStruct = ser::Impossible<u64, ValueError>;
    type SerializeTupleVariant = ser::Impossible<u64, ValueError>;
    type SerializeMap = ser::Impossible<u64, ValueError>;
    type SerializeStruct = ser::Impossible<u64, ValueError>;
    type SerializeStructVariant = ser::Impossible<u64, ValueError>;

    fn serialize_bool(self, _v: bool) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_i8(self, _v: i8) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_i16(self, _v: i16) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_i32(self, _v: i32) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_i64(self, _v: i64) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_u8(self, _v: u8) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_u16(self, _v: u16) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_u32(self, _v: u32) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        Ok(v)
    }

    fn serialize_f32(self, _v: f32) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_f64(self, _v: f64) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_char(self, _v: char) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_str(self, _v: &str) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + ser::Serialize,
    {
        todo!()
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_unit_struct(
        self,
        name: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_unit_variant(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_newtype_struct<T>(
        self,
        name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + ser::Serialize,
    {
        todo!()
    }

    fn serialize_newtype_variant<T>(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + ser::Serialize,
    {
        todo!()
    }

    fn serialize_seq(
        self,
        len: Option<usize>,
    ) -> Result<Self::SerializeSeq, Self::Error> {
        todo!()
    }

    fn serialize_tuple(
        self,
        len: usize,
    ) -> Result<Self::SerializeTuple, Self::Error> {
        todo!()
    }

    fn serialize_tuple_struct(
        self,
        name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        todo!()
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
        todo!()
    }

    fn serialize_struct(
        self,
        name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        todo!()
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
