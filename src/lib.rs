#![forbid(unsafe_code)]
#![deny(clippy::all)]

mod error;
mod runtime;
mod stdlib;
mod value;

pub use crate::{
    error::{Error, Result},
    runtime::{module::Module, Runtime},
    value::{Array, Function, JsString, Number, Object, Value},
};
