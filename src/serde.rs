//! Serialization and deserialization
//!
//! This module is designed to imitate Serde data format crate, and thus follows
//! [the conventions laid out by Serde](https://serde.rs/conventions.html).

mod de;
mod ser;

pub use de::Deserializer;
pub use ser::Serializer;

use crate::{FromJs, IntoJs, RuntimeResult, Value};
use serde::{Deserialize, Serialize};
use std::ops::{Deref, DerefMut};
use thiserror::Error;

/// A representation of all possible errors that could occur while serializing
/// to or deserializing from PetitJS values
///
/// TODO merge into RuntimeError?
#[derive(Debug, Error)]
pub enum Error {}

pub type Result<T> = std::result::Result<T, Error>;

/// Serialize the given data structure as a PetitJS value
pub fn to_value<T: Serialize>(data: &T) -> Result<Value> {
    todo!()
}

/// Deserialize an instance of type `T` from a PetitJS value
pub fn from_value<'de, T: Deserialize<'de>>(value: &Value) -> Result<T> {
    todo!()
}

impl Value {
    /// Deserialize this value into an arbitrary type, using the type's
    /// [Deserialize](serde::Deserialize) implementation
    pub fn deserialize<'de, T: Deserialize<'de>>(&self) -> Result<T> {
        from_value(self)
    }
}

impl serde::Serialize for Value {
    fn serialize<S>(&self, _: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        todo!()
    }
}

impl<'de> serde::Deserialize<'de> for Value {
    fn deserialize<D>(_: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        todo!()
    }
}

/// Helper for converting a value to/from JS using its
/// [Serialize](serde::Serialize) and/or [Deserialize](serde::Deserialize)
/// implementations.
pub struct SerdeJs<T>(pub T);

impl<T> SerdeJs<T> {
    /// Move the inner value out of this wrapper
    pub fn into_inner(self) -> T {
        self.0
    }
}

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
