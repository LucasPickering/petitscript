#![forbid(unsafe_code)]
#![deny(clippy::all)]

pub mod ast;
mod compile;
pub mod error;
mod execute;
#[cfg(feature = "serde")]
#[cfg_attr(docsrs, doc(cfg(feature = "serde")))] // TODO make this work
pub mod serde;
mod source;
mod stdlib;
pub mod value;

pub use crate::{
    error::Error,
    execute::{Exports, Process},
    source::Source,
    value::Value,
};

use crate::{
    ast::NativeModuleName,
    error::RuntimeError,
    execute::GlobalEnvironment,
    stdlib::stdlib,
    value::{
        function::{FromPetitArgs, IntoPetitResult, NativeFunctionTable},
        Function,
    },
};
use indexmap::IndexMap;
use std::sync::Arc;

/// The main entrypoint for executing and evaluating PetitScript programs
///
/// An engine defines how code should be executed. TODO more
#[derive(Debug)]
pub struct Engine {
    /// Modules registered by the user that can be imported into any script.
    /// This is the only way to provide named module imports. All other imports
    /// must be by relative path.
    modules: IndexMap<NativeModuleName, Exports>,
    /// Global values available to all code execution. This includes only the
    /// standard library, and cannot be modified after engine initialization.
    /// User-defined natives can only be exposed through modules. As such, we
    /// can use refcounting to share this among all processes and threads.
    globals: Arc<GlobalEnvironment>,
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
            globals: Arc::new(globals),
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
        name: impl Into<String>,
        module: Exports,
    ) -> Result<(), Error> {
        let name: NativeModuleName = name.into().try_into()?;
        self.modules.insert(name, module);
        Ok(())
    }

    /// Define a native function and return it as a value so it can be included
    /// in a module definition.
    ///
    /// ## Caveats
    ///
    /// The returned function can only be used within _this_ PetitScript engine.
    pub fn create_fn<F, Args, Out>(&mut self, function: F) -> Function
    where
        F: 'static + Fn(&Process, Args) -> Out + Send + Sync,
        Args: FromPetitArgs,
        Out: IntoPetitResult,
    {
        self.native_functions.create_fn(function)
    }

    /// Compile some source code into a loaded program. The returned [Process]
    /// can be used to execute the program.
    pub fn compile(&self, source: impl Source) -> Result<Process, Error> {
        let program = compile::compile(source)?;
        Ok(Process::new(
            self.modules.clone(),
            self.native_functions.clone(),
            Arc::clone(&self.globals),
            program,
        ))
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}
