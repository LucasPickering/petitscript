//! PetitScript is a **minimal** JavaScript subset designed for embeddability
//!
//! PS is intended as sidecar language for extensible scripting attached to
//! Rust programs. It serves a similar role as [Lua](https://www.lua.org/),
//! [Rhai](https://docs.rs/rhai/latest/rhai/), and others.
//!
//! The primary advantage of PS over other scripting languages is the
//! familiarity of its syntax and semantics. Most people already know
//! JavaScript, and PS avoids the complex features and footguns that JS novice's
//! may not know. This similarity to JS not only makes it easy to learn, it
//! also makes JS tooling applicable. You can use both JavaScript and TypeScript
//! language servers and formatters to write PetitScript.
//!
//! TODO how intro this list?
//! - Easily define native functions callable from PS code
//! - Introspect PS programs via the [abstract syntax tree](ast)
//! - Freely clone and share PS values across threads
//! - [Serde](crate::serde) integration
//!
//! Some language features that PetitScript supports:
//! - Primitive types
//! - Arrays and objects
//! - Local and native modules
//!
//! Here's an **incomplete** list of JavaScript features that are *not*
//! supported by PetitScript:
//! - Mutability
//! - Classes
//! - Prototypes
//! - Object methods
//! - Try/catch
//! - Async
//!
//! Here's a simple example of compiling and executing some PetitScript code:
//!
//! ```
//! use petitscript::Engine;
//!
//! let engine = Engine::new();
//! // TODO reevaluate the naming of everything:
//! // - Process is kinda clunky/confusing
//! // - execute() doesn't sound like it should return exports
//! let process = engine.compile("export default 4;").unwrap();
//! let exports = process.execute().unwrap();
//! assert_eq!(exports.default, Some(4.into()));
//! ```
//!
//! ## Examples
//!
//! - TODO Native modules
//! - TODO Local modules
//! - TODO Compile and inspect AST
//! - TODO value provenance (source span tracking)

#![forbid(unsafe_code)]
#![deny(clippy::all)]

pub mod ast;
mod compile;
pub mod error;
mod execute;
mod name;
#[cfg(feature = "serde")]
#[cfg_attr(docsrs, doc(cfg(feature = "serde")))] // TODO make this work
pub mod serde;
mod source;
mod stdlib;
#[cfg(test)]
mod test_util;
pub mod value;

pub use crate::{
    error::Error,
    execute::{Exports, Process},
    name::NativeModuleName,
    source::Source,
    value::Value,
};

use crate::{
    ast::{Module, Node},
    compile::Program,
    error::RuntimeError,
    execute::GlobalEnvironment,
    stdlib::stdlib,
};
use indexmap::IndexMap;
use std::sync::Arc;

/// Main entrypoint for executing and evaluating PetitScript programs
///
/// An engine defines how code should be executed and what modules are available
/// to executed code. Engines are created using the [builder pattern](https://rust-unofficial.github.io/patterns/patterns/creational/builder.html)
/// and are immutable after creation. This immutability allows an engine to be
/// shared among many processes cheaply through the joys of reference counting.
///
/// See [the crate root](crate) for a usage example.
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
}

#[cfg(test)]
static_assertions::assert_impl_all!(Engine: Send, Sync);

impl Engine {
    /// Initialize a new engine with default configuration
    ///
    /// This engine will include the standard library.
    pub fn new() -> Self {
        Self::builder().with_stdlib().build()
    }

    /// Create a new [EngineBuilder]
    ///
    ///
    /// <div class="warning">
    ///
    /// By default, [EngineBuilder] does not include the PetitScript standard
    /// library in the built engine. Use [Engine::new] to get an engine with
    /// the standard library if you do not need customization. Otherwise, be
    /// sure to call [EngineBuilder::with_stdlib].
    ///
    /// </div>
    pub fn builder() -> EngineBuilder {
        EngineBuilder {
            modules: Default::default(),
            globals: GlobalEnvironment::default(),
        }
    }

    /// Get a reference to a registered name module by its name
    pub fn module(&self, name: &str) -> Option<&Exports> {
        self.modules.get(name)
    }

    /// Parse source code into an [abstract syntax tree](crate::ast)
    ///
    /// The returned AST is
    /// not fully compiled and therefore can't be executed. Call
    /// [Engine::compile_ast] to complete compilation.
    ///
    /// This is only useful if you want to introspect the compiled program. If
    /// you're just executing code, you should call [Engine::compile] to get
    /// the compiled process in a single step.
    pub fn parse(
        &self,
        source: impl Into<Source>,
    ) -> Result<Node<Module>, Error> {
        compile::parse(source)
    }

    /// Parse and compile some source code into an executable [Process]
    pub fn compile(&self, source: impl Into<Source>) -> Result<Process, Error> {
        let program = compile::compile(source)?;
        Ok(self.spawn(program))
    }

    /// Compile a prebuilt AST into an executable [Process]. Helpful for tests
    /// and other environments where an AST is built programatically rather
    /// than by parsing source code.
    pub fn compile_ast(&self, module: Node<Module>) -> Result<Process, Error> {
        let program = compile::compile_ast(module);
        Ok(self.spawn(program))
    }

    /// Create a new process to execute a compiled program
    fn spawn(&self, program: Program) -> Process {
        Process::new(self.modules.clone(), Arc::clone(&self.globals), program)
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}

/// A modular builder for constructing [Engine]s
///
/// This builder can be used to customize an engine, such as registering native
/// modules with [with_module](Self::with_module). Create a builder with
/// [Engine::builder]:
///
/// ```
/// # use petitscript::Engine;
/// let engine = Engine::builder().with_stdlib().build();
/// ```
#[derive(Debug)]
pub struct EngineBuilder {
    // See Engine for field descriptions
    modules: IndexMap<NativeModuleName, Exports>,
    globals: GlobalEnvironment,
}

impl EngineBuilder {
    /// Add the PetitScript standard library to the engine's global scope.
    #[must_use]
    pub fn with_stdlib(mut self) -> Self {
        // Register the standard library
        self.globals = stdlib();
        self
    }

    /// Build and register a native module with the built engine. The module can
    /// be imported into any PetitScript program executed by this engine. If
    /// a module with the given name is already registered, it will be replaced.
    ///
    /// ```
    /// # use petitscript::{
    /// #     Engine,
    /// #     Exports,
    /// #     NativeModuleName,
    /// #     Process,
    /// #     value::Number
    /// # };
    ///
    /// let name: NativeModuleName = "math".parse().unwrap();
    /// let mut module = Exports::default();
    /// module.export_fn(
    ///     "add",
    ///     |_process, (a, b): (Number, Number)| a + b
    /// );
    /// Engine::builder().with_module(name, module).build();
    /// ```
    ///
    /// And now you can do this:
    ///
    /// ```notrust
    /// import { add } from "math";
    /// add(1, 2);
    /// ```
    #[must_use]
    pub fn with_module(
        mut self,
        name: NativeModuleName,
        exports: Exports,
    ) -> Self {
        self.modules.insert(name, exports);
        self
    }

    /// Build the engine with the defined configuration
    pub fn build(self) -> Engine {
        Engine {
            modules: self.modules,
            globals: self.globals.into(),
        }
    }
}
