#![forbid(unsafe_code)]
#![deny(clippy::all)]

mod ast;
mod error;
mod execute;
mod parse;
mod stdlib;
mod value;

pub use crate::{
    error::RuntimeError,
    value::{Array, Exports, Function, JsString, Number, Object, Value},
};

use crate::{error::Error, execute::RuntimeState, value::NativeFunctionTrait};
use std::{borrow::Cow, fs, path::Path};

/// TODO
#[derive(Clone, Debug, Default)]
pub struct Engine {}

impl Engine {
    /// TODO
    pub fn new() -> Self {
        Self {}
    }

    /// Register a function to be available in the global namespace for all
    /// code execution in this runtime
    pub fn register_fn(&mut self, name: String, f: impl NativeFunctionTrait) {
        todo!()
    }

    /// TODO
    pub fn load(&self, source: impl Source) -> Result<Exports, Error> {
        let script = parse::parse(source)?;
        let mut state = RuntimeState::new();
        state.exec(&script)?;
        let module = state.into_exports()?;
        Ok(module)
    }
}

/// A source of source code. E.g. a string literal or a file path
pub trait Source {
    /// TODO
    fn name(&self) -> Option<&str>;

    /// TODO
    fn text(&self) -> Result<Cow<'_, str>, Error>;
}

impl Source for String {
    fn name(&self) -> Option<&str> {
        None
    }

    fn text(&self) -> Result<Cow<'_, str>, Error> {
        Ok(self.as_str().into())
    }
}

impl Source for &Path {
    fn name(&self) -> Option<&str> {
        self.to_str()
    }

    fn text(&self) -> Result<Cow<'_, str>, Error> {
        Ok(fs::read_to_string(self)?.into())
    }
}
