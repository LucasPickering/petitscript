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
    ast::NativeModuleName,
    error::Error,
    execute::{Exports, Process},
    source::Source,
    value::Value,
};

use crate::{
    ast::{Module, Node},
    compile::Program,
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
/// An engine defines how code should be executed and what modules are available
/// to executed code. Engines are created using the [builder pattern](https://rust-unofficial.github.io/patterns/patterns/creational/builder.html)
/// and are immutable after creation. This immutability allows an engine to be
/// shared among many processes cheaply, through the joys of reference counting.
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
    /// Create a new [EngineBuilder]
    ///
    ///
    /// <div class="warning">
    ///
    /// By default, [EngineBuilder] does not include the PetitScript standard
    /// library in the built engine. Use [Engine::default] to get an engine with
    /// the standard library if you do not need customization. Otherwise, be
    /// sure to call [EngineBuilder::with_stdlib].
    ///
    /// </div>
    pub fn builder() -> EngineBuilder {
        EngineBuilder {
            modules: Default::default(),
            globals: GlobalEnvironment::default(),
            native_functions: NativeFunctionTable::default(),
        }
    }

    /// Parse some source code and return the parsed AST. The returned AST is
    /// not fully compiled and therefore can't be executed. Call
    /// [Engine::compile_ast] to complete compilation.
    pub fn parse(&self, source: impl Source) -> Result<Node<Module>, Error> {
        compile::parse(source)
    }

    /// Parse and compile some source code into an executable [Process]
    pub fn compile(&self, source: impl Source) -> Result<Process, Error> {
        let program = compile::compile(source)?;
        Ok(self.spawn(program))
    }

    /// Compile a prebuilt AST into an executable [Process]. Helpful for tests
    /// and other environments where an AST is built programatically rather
    /// than by parsing source code.
    pub fn compile_ast(&self, module: Node<Module>) -> Result<Process, Error> {
        let program = compile::compile_ast(module)?;
        Ok(self.spawn(program))
    }

    /// Create a new process to execute a compiled program
    fn spawn(&self, program: Program) -> Process {
        Process::new(
            self.modules.clone(),
            self.native_functions.clone(),
            Arc::clone(&self.globals),
            program,
        )
    }
}

impl Default for Engine {
    /// Initialize a new engine with default configuration and the standard
    /// library available
    fn default() -> Self {
        Self::builder().with_stdlib().build()
    }
}

/// A modular builder for construction [Engine]s. This builder can be used to
/// customize an engine, such as registering native modules with
/// [with_module](Self::with_module).
#[derive(Debug)]
pub struct EngineBuilder {
    // See Engine for field descriptions
    modules: IndexMap<NativeModuleName, Exports>,
    globals: GlobalEnvironment,
    native_functions: NativeFunctionTable,
}

impl EngineBuilder {
    /// Add the PetitScript standard library to the engine's global scope.
    pub fn with_stdlib(mut self) -> Self {
        // Register the standard library
        self.globals = stdlib(&mut self.native_functions);
        self
    }

    /// Build and register a native module with the built engine. The module can
    /// be imported into any PetitScript program executed by this engine. If
    /// a module with the given name is already registered, it will be replaced.
    ///
    /// ```
    /// fn add(_: &Process, (a, b): (Number, Number)) -> Number {
    ///     a + b
    /// }
    ///
    /// let name: NativeModuleName = "math".parse().unwrap();
    /// EngineBuilder::new()
    ///     .with_module(name, |builder| {
    ///         builder.export_fn("add", add);
    ///     })
    ///     .build();
    /// ```
    ///
    /// And now you can do this:
    ///
    /// ```notrust
    /// import { add } from "math";
    /// add(1, 2);
    /// ```
    pub fn with_module(
        mut self,
        name: NativeModuleName,
        builder: impl FnOnce(&mut NativeModuleBuilder),
    ) -> Self {
        let mut module_builder = NativeModuleBuilder {
            native_functions: &mut self.native_functions,
            exports: Exports::default(),
        };
        builder(&mut module_builder);
        self.modules.insert(name, module_builder.exports);
        self
    }

    /// Build the engine with the defined configuration
    pub fn build(self) -> Engine {
        Engine {
            modules: self.modules,
            globals: self.globals.into(),
            native_functions: self.native_functions,
        }
    }
}

/// A helper for constructing native modules. See [EngineBuilder::with_module]
/// for usage examples.
#[derive(Debug)]
pub struct NativeModuleBuilder<'a> {
    native_functions: &'a mut NativeFunctionTable,
    exports: Exports,
}

impl<'a> NativeModuleBuilder<'a> {
    /// Define a native function and return it as a value so it can be included
    /// in a module definition.
    ///
    /// ## Caveats
    ///
    /// The returned function can only be used within this PetitScript engine.
    pub fn create_fn<F, Args, Out>(&mut self, function: F) -> Function
    where
        F: 'static + Fn(&Process, Args) -> Out + Send + Sync,
        Args: FromPetitArgs,
        Out: IntoPetitResult,
    {
        self.native_functions.create_fn(function)
    }

    /// Define a native function and expose it as a named export from this
    /// module.
    ///
    /// ## Caveats
    ///
    /// The created function can only be used within this PetitScript engine.
    pub fn export_fn<F, Args, Out>(
        &mut self,
        name: impl Into<String>,
        function: F,
    ) where
        F: 'static + Fn(&Process, Args) -> Out + Send + Sync,
        Args: FromPetitArgs,
        Out: IntoPetitResult,
    {
        let function = self.create_fn(function);
        self.exports.named.insert(name.into(), function.into());
    }
}
