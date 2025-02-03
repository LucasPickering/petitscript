//! Serde-related implementations and helpers

use crate::{FromJs, IntoJs, RuntimeResult, Value};
use std::ops::{Deref, DerefMut};

impl serde::Serialize for Value {
    fn serialize<S>(&self, _: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        todo!()
    }
}

impl<'de> serde::Deserialize<'de> for Value {
    fn deserialize<D>(_: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        todo!()
    }
}

/// Helper for converting a value to/from JS using its
/// [Serialize](serde::Serialize) and/or [Deserialize](serde::Deserialize)
/// implementations.

pub struct SerdeJs<T>(T);

impl<T> SerdeJs<T> {}

impl<T> Deref for SerdeJs<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for SerdeJs<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: serde::Serialize> IntoJs for SerdeJs<T> {
    fn into_js(self) -> RuntimeResult<Value> {
        todo!()
    }
}

impl<'de, T: serde::Deserialize<'de>> FromJs for SerdeJs<T> {
    fn from_js(_: Value) -> RuntimeResult<Self> {
        todo!()
    }
}
