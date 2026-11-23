// Enable automatic feature tagging in docs
#![cfg_attr(docsrs, feature(doc_cfg))]

//! PetitScript is a **minimal** JavaScript subset designed for embeddability
//!
//! PetitScript is intended as sidecar language for extensible scripting
//! attached to Rust programs. It serves a related role to [Lua](https://www.lua.org/),
//! [Rhai](https://docs.rs/rhai/latest/rhai/), and others. **However**,
//! PetitScript is more specifically designed for app configuration. The
//! [immutable semantics](#immutability) are meant to force declarative code
//! that is easy to read and reason about.
//!
//! The primary advantage of PS over other scripting languages is the
//! familiarity of its syntax and semantics. Most people already know
//! JavaScript, and PS avoids the complex features and footguns that JS novice's
//! may not know. This similarity to JS not only makes it easy to learn, it
//! also makes JS tooling applicable. You can use both JavaScript and TypeScript
//! language servers and formatters to write PetitScript.
//!
//! Some neat features of PetitScript:
//! - Easily define native functions callable from PS code
//! - Introspect and build programs via the minimal [abstract syntax tree](ast)
//! - Freely clone and share PS values across threads
//! - [Serde integration](crate::serde)
//!
//! TODO those should all link to examples^^
//!
//! Some language features that PetitScript supports:
//! - Primitive types
//! - Arrays and objects
//! - Local and native modules
//!
//! Here's an **incomplete** list of JavaScript features that are *not*
//! supported by PetitScript:
//! - Mutability
//! - Loops
//! - Classes
//! - Prototypes
//! - Object methods
//! - Try/catch
//! - Async
//!
//! # Semantics
//!
//! ## Simplicity
//!
//! ## Immutability
//!
//! TODO
//!
//! # Usage
//!
//! TODO explain core types
//! - Engine
//! - Source
//! - Program
//! - World
//! - Exports
//! - Value
//!
//! # Examples
//!
//! The simplest example of parsing and executing some PetitScript code:
//!
//! ```
//! let engine = petitscript::Engine::new();
//! let exports = engine.run("export default 4;").unwrap();
//! assert_eq!(exports.default, Some(4.into()));
//! ```
//!
//! ## Calling Functions
//!
//! Functions exported from an invocation can then be called later:
//!
//! ```
//! # use petitscript::{Engine, Program, Value};
//! let engine = Engine::new();
//! // Export a function
//! let program = Program::parse("export const f  = (n) => n * 2;").unwrap();
//! let world = engine.instantiate(program);
//! let exports = world.run().unwrap();
//! // Grab the exported function
//! let Value::Function(f) = &exports.named["f"] else {
//!     panic!("expected function!")
//! };
//! assert_eq!(f.to_string(), "[Function `f` (user)]");
//! // The function MUST be invoked in the world that produced it
//! assert_eq!(world.call(f, [4]).unwrap(), 8.into());
//! ```
//!
//! ## Native Modules
//!
//! Native modules allow you to expose Rust functions to PetitScript code.
//! PetitScript's standard library has no random library, so here's an example
//! of a random number generator:
//!
//! ```
//! # use petitscript::{Engine, Exports};
//! let mut module = Exports::default();
//! // Native functions always take two parameters: the executing World and
//! // the args passed from the PS call. The PS args will be packed into a
//! // tuple.
//! module.export_fn("random", |_, ()| {
//!     // chosen by fair dice roll
//!     // guaranteed to be random
//!     4
//! });
//! let engine = Engine::builder()
//!     .with_native_module("random", module)
//!     .build();
//!
//! let program = "import { random } from 'random';
//! export default random();";
//! let exports = engine.run(program).unwrap();
//! assert_eq!(exports.default.unwrap(), 4.into());
//! ```
//!
//! See [`Exports::export_fn`] for examples of arguments, return types, and
//! errors.
//!
//! ## App Data
//!
//! App data is arbitrary data attached to a [`World`]. Because the `World` is
//! exposed to each native function call, app data can be accessed in those
//! native calls, providing a conduit to outside program context.
//!
//! **App data is keyed by its type**, meaning you can only register one app
//! data value for any specific type `T`.
//!
//! ```
//! # use petitscript::{Engine, Exports, Program};
//! # use std::sync::atomic::{AtomicU32, Ordering};
//! struct Counter(AtomicU32);
//!
//! // Define your native module
//! let mut module = Exports::default();
//! module.export_fn("counter", |world, ()| {
//!     // App data is immutable, so we have to use an atomic primitive
//!     let counter = world.app_data::<Counter>().unwrap();
//!     counter.0.fetch_add(1, Ordering::Relaxed)
//! });
//! let engine = Engine::builder()
//!     .with_native_module("counter", module)
//!     .build();
//!
//! // Instantiate the program as a world
//! let program = Program::parse(
//!     "import { counter } from 'counter';
//! export default [counter(), counter(), counter()];",
//! )
//! .unwrap();
//! let mut world = engine.instantiate(program);
//!
//! // Attach app data and it will be available to counter()
//! world.set_app_data(Counter(0.into()));
//! let exports = world.run().unwrap();
//! assert_eq!(exports.default.unwrap(), [0, 1, 2].into());
//! ```
//!
//! ## Local Modules
//!
//! TODO
//!
//! # Feature Flags
//!
//! TODO
#![forbid(unsafe_code)]
#![deny(clippy::all)]

pub mod ast;
mod compile;
pub mod error;
mod execute;
mod name;
#[cfg(feature = "serde")]
pub mod serde;
mod source;
mod stdlib;
#[cfg(test)]
mod test_util;
mod util;
pub mod value;

pub use crate::{error::Error, value::Value};
// A bug in rustdoc shows re-exports from private modules if they're grouped
// with re-exports from public modules. Separate these out so they're ONLY
// inlined.
pub use crate::{
    compile::Program,
    execute::{Exports, World},
    name::NativeModuleName,
    source::Source,
};

use crate::{
    error::RuntimeErrorKind, execute::GlobalEnvironment, stdlib::stdlib,
};
use std::{collections::HashMap, sync::Arc};

/// Main entrypoint for executing and evaluating PetitScript programs
///
/// An engine defines how code should be executed and what modules are available
/// to executed code. Engines are created using the [builder pattern](https://rust-unofficial.github.io/patterns/patterns/creational/builder.html)
/// and are immutable after creation. This immutability allows an engine to be
/// shared among many threads cheaply through the joys of reference counting.
///
/// See [the crate root](crate) for a usage example.
#[derive(Clone, Debug)]
pub struct Engine {
    /// Modules registered by the user that can be imported into any script
    ///
    /// This is the only way to provide named module imports. All other imports
    /// must be by relative path.
    modules: Arc<HashMap<NativeModuleName, Exports>>,
    /// Global values available to all code execution
    ///
    /// This includes only the global library (which for the default engine is
    /// just the [standard library](stdlib)). User-defined natives can only be
    /// exposed through modules. As such, we can use refcounting to share this
    /// among all worlds and fibers.
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

    /// Create a new [`EngineBuilder`]
    ///
    ///
    /// <div class="warning">
    ///
    /// By default, [`EngineBuilder`] does not include the PetitScript standard
    /// library in the built engine. Use [`Engine::new`] to get an engine with
    /// the standard library if you do not need customization. Otherwise, be
    /// sure to call [`EngineBuilder::with_stdlib`].
    ///
    /// </div>
    pub fn builder() -> EngineBuilder {
        EngineBuilder {
            modules: Default::default(),
            globals: GlobalEnvironment::default(),
        }
    }

    /// Get a reference to a registered native module by its name
    pub fn native_module(&self, name: &str) -> Option<&Exports> {
        self.modules.get(name)
    }

    /// Create a new [`World`] to execute the given program
    pub fn instantiate(&self, program: Program) -> World {
        World::new(self.clone(), program)
    }

    /// TODO
    pub fn run(&self, source: impl Into<Source>) -> Result<Exports, Error> {
        let source = source.into();
        let program = source.parse()?;
        let world = self.instantiate(program);
        let exports = world.run()?;
        Ok(exports)
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}

/// A modular builder for constructing [`Engine`]s
///
/// This builder can be used to customize an engine, such as registering native
/// modules with [`with_native_module`](Self::with_native_module). Create a
/// builder with [`Engine::builder`]:
///
/// ```
/// # use petitscript::Engine;
/// let engine = Engine::builder().with_stdlib().build();
/// ```
#[derive(Debug)]
pub struct EngineBuilder {
    // See Engine for field descriptions
    modules: HashMap<NativeModuleName, Exports>,
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

    /// Build and register a [native module](crate#native-modules) in the engine
    ///
    /// The module can be imported into any PetitScript program executed by this
    /// engine. If a module with the given name is already registered, it
    /// will be replaced.
    ///
    /// <div class="warning">
    ///
    /// This accepts any value that implements `Into<NativeModuleName>` as the
    /// module name, which includes `&'static str` (a string literal). That
    /// conversion **panics** if the name is invalid. Because the string is
    /// defined statically, the panic is very easy to catch and fix. If you
    /// want to build the name dynamically or handle the parse error
    /// yourself, use [`str::parse`].
    ///
    /// </div>
    ///
    /// # Example
    ///
    /// ```
    /// # use petitscript::{
    /// #     Engine,
    /// #     Exports,
    /// #     NativeModuleName,
    /// #     World,
    /// #     value::Number
    /// # };
    /// let mut module = Exports::default();
    /// module.export_fn("add", |_world, (a, b): (Number, Number)| a + b);
    /// Engine::builder().with_native_module("math", module).build();
    /// ```
    ///
    /// And now you can do this:
    ///
    /// ```notrust
    /// import { add } from "math";
    /// add(1, 2);
    /// ```
    #[must_use]
    pub fn with_native_module(
        mut self,
        name: impl Into<NativeModuleName>,
        exports: Exports,
    ) -> Self {
        self.modules.insert(name.into(), exports);
        self
    }

    /// Build the engine with the defined configuration
    pub fn build(self) -> Engine {
        Engine {
            modules: Arc::new(self.modules),
            globals: self.globals.into(),
        }
    }
}
