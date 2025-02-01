#![forbid(unsafe_code)]
#![deny(clippy::all)]

mod ast;
mod error;
mod parse;
mod runtime;
mod stdlib;
mod value;

pub use crate::{
    error::RuntimeError,
    runtime::module::Module,
    value::{Array, Function, JsString, Number, Object, Value},
};
use crate::{
    error::{Error, LoadError},
    runtime::state::RuntimeState,
};
use std::{borrow::Cow, fs, path::Path};

/// TODO
#[derive(Clone, Debug, Default)]
pub struct Engine {}

impl Engine {
    /// TODO
    pub fn new() -> Self {
        Self {}
    }

    /// TODO
    pub fn load(&self, source: impl Source) -> Result<Module, Error> {
        let script = parse::parse(source)?;
        let mut state = RuntimeState::new();
        state.exec(&script)?;
        let module = state.into_module()?;
        Ok(module)
    }
}

/// A source of source code. E.g. a string literal or a file path
pub trait Source {
    /// TODO
    fn name(&self) -> Option<&str>;

    /// TODO
    fn text(&self) -> Result<Cow<'_, str>, LoadError>;
}

impl Source for String {
    fn name(&self) -> Option<&str> {
        None
    }

    fn text(&self) -> Result<Cow<'_, str>, LoadError> {
        Ok(self.as_str().into())
    }
}

impl Source for &Path {
    fn name(&self) -> Option<&str> {
        self.to_str()
    }

    fn text(&self) -> Result<Cow<'_, str>, LoadError> {
        Ok(fs::read_to_string(self)?.into())
    }
}
