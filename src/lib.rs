#![forbid(unsafe_code)]
#![deny(clippy::all)]

mod ast;
mod error;
mod execute;
mod parse;
mod scope;
mod stdlib;
mod util;
mod value;

#[cfg(feature = "serde")]
pub use crate::value::cereal::SerdeJs;
pub use crate::{
    error::{Error, RuntimeError, RuntimeResult},
    execute::{AppData, Process},
    value::{
        Array, AsyncNativeFunction, Exports, FromJs, Function, IntoJs,
        JsString, NativeFunction, Number, Object, Value, ValueType,
    },
};

use crate::{scope::Scope, stdlib::stdlib, value::FromJsArgs};
use std::{borrow::Cow, fs, future::Future, path::Path};

/// The main entrypoint for executing and evaluating PetitJS programs. An engine
/// defines how code should be executed. TODO more
#[derive(Clone, Debug, Default)]
pub struct Engine {
    /// User-defined global values. These will be made available to all code
    /// execution
    globals: Scope,
}

impl Engine {
    /// TODO
    pub fn new() -> Self {
        // Always start with the standard library
        Self { globals: stdlib() }
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
        self.globals.declare(name, value, false);
        Ok(())
    }

    /// TODO
    pub fn register_fn<F, Args, Out, Err>(
        &mut self,
        name: impl ToString,
        function: F,
    ) where
        F: 'static + Fn(&AppData, Args) -> Result<Out, Err> + Send + Sync,
        Args: FromJsArgs,
        Out: IntoJs,
        Err: Into<RuntimeError>,
    {
        self.globals
            .declare(name, NativeFunction::new(function).into(), false);
    }

    /// TODO
    pub fn register_async_fn<F, Args, Out, Err, Fut>(
        &mut self,
        name: impl ToString,
        function: F,
    ) where
        F: 'static + Fn(&AppData, Args) -> Fut + Send + Sync,
        Args: FromJsArgs,
        Out: IntoJs,
        Err: Into<RuntimeError>,
        Fut: 'static + Future<Output = Result<Out, Err>>,
    {
        self.globals.declare(
            name,
            AsyncNativeFunction::new(function).into(),
            false,
        );
    }

    /// Parse some source code into a loaded program. The returned [Process] can
    /// be used to execute the program.
    pub fn load(&self, source: impl Source) -> Result<Process, Error> {
        let program = parse::parse(source)?;
        Ok(Process::new(self.globals.clone(), program))
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
