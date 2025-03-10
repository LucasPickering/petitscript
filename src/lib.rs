#![forbid(unsafe_code)]
#![deny(clippy::all)]

mod ast;
mod compile;
pub mod error;
mod execute;
mod scope;
#[cfg(feature = "serde")]
pub mod serde;
mod stdlib;
mod value;

pub use crate::{
    ast::source::Source,
    error::Error,
    execute::Process,
    value::{
        function, Array, Exports, FromJs, Function, IntoJs, JsString, Number,
        Object, Value, ValueType,
    },
};

use crate::{
    error::RuntimeError,
    scope::Scope,
    stdlib::stdlib,
    value::function::{
        FromJsArgs, NativeFunctionDefinition, NativeFunctionTable,
    },
};

/// The main entrypoint for executing and evaluating PetitJS programs. An engine
/// defines how code should be executed. TODO more
#[derive(Clone, Debug)]
pub struct Engine {
    /// Global values available to all code execution. This includes both the
    /// standard library and user-defined values.
    globals: Scope,
    /// An intern pool of native functions. When a native function is defined
    /// (either by the stdlib or a user), the definition is stored here and
    /// references to that function simply use its ID. This can only be
    /// appended to, and never shortened, so each definition's unique ID is
    /// simply its index in this vec.
    native_functions: NativeFunctionTable,
}

#[cfg(test)]
static_assertions::assert_impl_all!(Engine: Send, Sync);

impl Engine {
    /// Initialize a new engine with default configuration and the standard
    /// library available
    pub fn new() -> Self {
        let mut engine = Self {
            globals: Default::default(),
            native_functions: Default::default(),
        };
        // Register the standard library
        stdlib(&mut engine);
        engine
    }

    /// Register a value in the global namespace. This will be made available
    /// to all code executed in this engine. This can be used to register
    /// anything that implements [IntoJs], including primitive values and
    /// objects. To register native functions, use
    /// [register_fn](Self::register_fn) instead.
    pub fn register_global(&mut self, name: impl ToString, value: Value) {
        self.globals.declare(name, value);
    }

    /// Register a Rust function as a global, allowing it to be used within
    /// PetitJS programs
    pub fn register_fn<F, Args, Out, Err>(
        &mut self,
        name: impl ToString,
        function: F,
    ) where
        F: 'static + Fn(&Process, Args) -> Result<Out, Err> + Send + Sync,
        Args: FromJsArgs,
        Out: IntoJs,
        Err: Into<RuntimeError>,
    {
        let mut function = self.create_fn(function);
        let name = name.to_string();
        function.set_name(name.clone()); // Label the function
        self.globals.declare(name.to_string(), function.into());
    }

    /// Add a Rust native function to the engine, but do *not* register it in
    /// the global namespace. The function will be returned instead, and can be
    /// used as a JS value anywhere.
    pub fn create_fn<F, Args, Out, Err>(&mut self, function: F) -> Function
    where
        F: 'static + Fn(&Process, Args) -> Result<Out, Err> + Send + Sync,
        Args: FromJsArgs,
        Out: IntoJs,
        Err: Into<RuntimeError>,
    {
        let id = self
            .native_functions
            .register(NativeFunctionDefinition::new(function));
        Function::native(id)
    }

    /// Compile some source code into a loaded program. The returned [Process]
    /// can be used to execute the program.
    pub fn compile(&self, source: impl Source) -> Result<Process, Error> {
        let program = compile::compile(source)?;
        Ok(Process::new(
            self.native_functions.clone(),
            self.globals.clone(),
            program,
        ))
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}
