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
        function, Array, Exports, FromPs, Function, IntoPs, Number, Object,
        PetitString, Value, ValueType,
    },
};

use crate::{
    ast::ModuleName,
    error::RuntimeError,
    scope::Scope,
    stdlib::stdlib,
    value::function::{FromPsArgs, NativeFunctionTable},
};
use indexmap::IndexMap;

/// The main entrypoint for executing and evaluating PetitScript programs. An
/// engine defines how code should be executed. TODO more
#[derive(Debug)]
pub struct Engine {
    /// Modules registered by the user that can be imported into any script.
    /// This is the only way to provide named module imports. All other imports
    /// must be by relative path.
    modules: IndexMap<ModuleName, Exports>,
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
        let mut native_functions = NativeFunctionTable::default();
        // Register the standard library
        let globals = stdlib(&mut native_functions);
        Self {
            modules: Default::default(),
            globals,
            native_functions,
        }
    }

    /// Register a native module, which can be imported into any PetitScript
    /// program executed by this engine. If a module with the given name is
    /// already registered, it will be replaced.
    ///
    /// ```
    /// engine.register_module("math", todo!()).unwrap();
    /// ```
    ///
    /// ```notrust
    /// import { add } from "math";
    /// ```
    ///
    /// ## Errors
    ///
    /// Return an error if the module name is invalid. TODO explain naming rules
    /// here.
    pub fn register_module(
        &mut self,
        name: impl ToString,
        module: Exports,
    ) -> Result<(), Error> {
        let name: ModuleName = name.to_string().try_into()?;
        self.modules.insert(name, module);
        Ok(())
    }

    /// Register a value in the global namespace. This will be made available
    /// to all code executed in this engine. This can be used to register
    /// anything that implements [IntoPs], including primitive values and
    /// objects. To register native functions, use
    /// [register_fn](Self::register_fn) instead.
    pub fn register_global(&mut self, name: impl ToString, value: Value) {
        self.globals.declare(name.to_string(), value);
    }

    /// Register a Rust function as a global, allowing it to be used within
    /// PetitScript programs
    pub fn register_fn<F, Args, Out, Err>(
        &mut self,
        name: impl ToString,
        function: F,
    ) where
        F: 'static + Fn(&Process, Args) -> Result<Out, Err> + Send + Sync,
        Args: FromPsArgs,
        Out: IntoPs,
        Err: Into<RuntimeError>,
    {
        let mut function = self.native_functions.create_fn(function);
        let name = name.to_string();
        function.set_name(name.clone()); // Label the function
        self.globals.declare(name.to_string(), function);
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
