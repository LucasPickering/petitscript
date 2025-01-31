#![forbid(unsafe_code)]
#![deny(clippy::all)]

mod ast;
mod error;
mod parse;
mod runtime;
mod stdlib;
mod value;

use crate::runtime::state::RuntimeState;
pub use crate::{
    error::{Error, Result},
    runtime::module::Module,
    value::{Array, Function, JsString, Number, Object, Value},
};
use std::{borrow::Cow, ffi::OsStr, fs, path::Path};

/// TODO
#[derive(Clone, Debug, Default)]
pub struct Engine {}

impl Engine {
    /// TODO
    pub fn new() -> Self {
        Self {}
    }

    /// TODO
    pub fn load(&self, source: impl Source) -> Result<Module> {
        let script = parse::parse(source)?;
        let mut state = RuntimeState::new();
        state.exec(&script)?;
        state.into_module()
    }
}

/// A source of source code. E.g. a string literal or a file path
pub trait Source {
    /// TODO
    fn name(&self) -> Option<&str>;

    /// TODO
    fn text(&self) -> Result<Cow<'_, str>>;
}

impl Source for String {
    fn name(&self) -> Option<&str> {
        None
    }

    fn text(&self) -> Result<Cow<'_, str>> {
        Ok(self.as_str().into())
    }
}

impl Source for &Path {
    fn name(&self) -> Option<&str> {
        self.file_name().and_then(OsStr::to_str)
    }

    fn text(&self) -> Result<Cow<'_, str>> {
        Ok(fs::read_to_string(self)?.into())
    }
}
