use crate::serde::Error;
use serde::de;
use std::fmt::Display;

impl de::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        todo!()
    }
}

/// TODO
pub struct Deserializer {}

impl serde::Deserializer for Deserializer {}
