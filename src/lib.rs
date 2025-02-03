#![forbid(unsafe_code)]
#![deny(clippy::all)]

mod ast;
mod error;
mod execute;
mod parse;
mod stdlib;
mod value;

#[cfg(feature = "serde")]
pub use crate::value::cereal::SerdeJs;
pub use crate::{
    error::{RuntimeError, RuntimeResult},
    value::{
        Array, AsyncNativeFunction, Exports, FromJs, Function,
        IntoAsyncNativeFunction, IntoJs, IntoNativeFunction, JsString,
        NativeFunction, Number, Object, Value,
    },
};

use crate::{error::Error, execute::RuntimeState};
use std::{borrow::Cow, collections::HashMap, fs, path::Path};

/// TODO
#[derive(Clone, Debug, Default)]
pub struct Engine {
    /// User-defined global values. These will be made available to all code
    /// execution
    globals: HashMap<String, Value>,
}

impl Engine {
    /// TODO
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
        }
    }

    /// Register a value in the global namespace. This will be made available
    /// to all code executed in this engine. This can be used to register
    /// anything that implements [IntoJs], including primitive values, objects,
    /// and functions. Objects can be used to namespace functions for
    /// organization.
    /// TODO update comment
    pub fn register_global(
        &mut self,
        name: impl ToString,
        value: impl IntoJs,
    ) -> RuntimeResult<()> {
        let value = value.into_js()?;
        self.globals.insert(name.to_string(), value);
        Ok(())
    }

    /// TODO
    pub fn register_fn<In, Out, Err>(
        &mut self,
        name: impl ToString,
        function: impl IntoNativeFunction<In, Out, Err>,
    ) {
        self.globals
            .insert(name.to_string(), function.into_native_fn().into());
    }

    /// TODO
    pub fn register_async_fn<In, Out, Err>(
        &mut self,
        name: impl ToString,
        function: impl IntoAsyncNativeFunction<In, Out, Err>,
    ) {
        self.globals
            .insert(name.to_string(), function.into_native_fn().into());
    }

    /// TODO
    pub async fn load(&self, source: impl Source) -> Result<Exports, Error> {
        let script = parse::parse(source)?;
        let mut state = RuntimeState::new();
        state.exec(&script).await?;
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
