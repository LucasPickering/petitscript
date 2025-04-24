//! Structures for declaring and accessing variables. This is an internal
//! module, not exposed to users.

use crate::{
    ast::{Binding, Identifier},
    value:: function::{Captures, NativeFunctionId},
    value::{Value, ValueType},
    RuntimeError,
};
use indexmap::IndexMap;
use std::{iter, sync::Arc};

/// A scope is a hierarchical list of sets of bindings. Each set of bindings is
/// known as an "environment". When we enter a new lexical scope (defined by
/// braces: `{}`), we add a new environment to the scope. When we exit the
/// scope, we remove the environment. Conceptually, the scope chain consists of
/// a single [GlobalScope] at the top, followed by one or more user scopes.
///
/// A user scope is a scope in which a user can declare names.
#[derive(Debug)]
pub struct Scope {
    globals: Arc<GlobalEnvironment>,
    /// The first scope in a program. This is the only scope that allows
    /// exports. Conceptually this is part of the stack below, but by storing
    /// it separately we ensure it can never be popped.
    root: Environment,
    /// Sub-environments of the root. This gets pushed onto whenever entering
    /// a new lexical scope `{ }`, and popped whenever leaving that scope.
    ///
    /// Note: This has no relation to the call stack. This stack tracks lexical
    /// scopes, whereas the call stack tracks function calls.
    stack: Vec<Environment>,
}

impl Scope {
    /// Are we in the root scope of the process? The root scope is the topmost
    /// lexical scope in a source file. It is always the _second_ scope in the
    /// chain (after the global scope), and is the only scope that allows
    /// exports.
    pub fn is_root(&self) -> bool {
        self.stack.is_empty()
    }

    /// Replace the current scope with a subscope of itself. This should be
    /// called right before entering a new lexical scope block.
    pub fn push_subscope(&mut self) {
        self.stack.push(Environment::default());
    }

    /// Replace the current scope with its parent, throwing away this scope's
    /// bindings. This should be called immediately after exiting a lexical
    /// scope. This should never be called on the global scope; that indicates
    /// we're popping more than we're pushing
    pub fn pop_subscope(&mut self) {
        if self.stack.pop().is_none() {
            panic!("Cannot pop the global scope");
        }
    }

    /// Declare a single name in scope, and bind it to a variable
    pub fn declare(
        &mut self,
        name: impl Into<String>,
        value: impl Into<Value>,
    ) {
        // Grab the lowest user env
        let current = self.stack.last_mut().unwrap_or(&mut self.root);
        current.declare(name.into(), value.into());
    }

    /// Get the value of a binding. Return an error if the binding doesn't exist
    /// in scope.
    pub fn get(&self, name: &str) -> Result<Value, RuntimeError> {
        // Search up the chain until we find the name
        self.chain()
            .find_map(|environment| environment.get(name).cloned())
            .ok_or_else(|| RuntimeError::Reference {
                name: name.to_owned(),
            })
    }

    /// Get a function definition from a specific type's prototype
    pub fn get_prototype(
        &self,
        value_type: ValueType,
        name: &str,
    ) -> Option<NativeFunctionId> {
        let definition_id = self
            .globals
            .prototypes
            .get(&value_type)
            .and_then(|prototype| prototype.0.get(name))?;
        Some(*definition_id)
    }

    /// Declare a new binding in this scope. The binding can be a single
    /// identifier, or a structured identifier, in which case multiple names
    /// may be bound. This is a convenience method for calling [Self::declare]
    /// on each name in the binding.
    pub fn bind(
        &mut self,
        binding: &Binding,
        value: Value,
    ) -> Result<Vec<String>, RuntimeError> {
        match binding {
            Binding::Identifier(identifier) => {
                let name = identifier.as_str().to_owned();
                self.declare(name.clone(), value);
                Ok(vec![name])
            }
            Binding::Object(_) => {
                Err(RuntimeError::internal("TODO object bindings"))
            }
            Binding::Array(_) => {
                Err(RuntimeError::internal("TODO array bindings"))
            }
        }
    }

    /// Capture values for a closure. For each given identifier that the closure
    /// needs, grab its value. If an identifier is needed but not available,
    /// return an error. This indicates a compiler bug. The compiler should
    /// ensure that any captured names are available in a parent scope. An
    /// invalid reference here means the compiler messed up.
    pub fn captures(
        &self,
        identifiers: &[Identifier],
    ) -> Result<Captures, RuntimeError> {
        // It's possible this is slow, because we constantly restruct the
        // iterator to go up the chain. It may be faster to flip it around and
        // walk the chain once, searching for each unfound name in the process
        identifiers
            .iter()
            .map(|identifier| {
                let name = identifier.as_str();
                let value = self.get(name).map_err(|_| {
                    RuntimeError::internal(format!(
                        "Captured variable `{name}` not available in scope"
                    ))
                })?;
                Ok((name.to_owned(), value))
            })
            .collect()
    }

    /// Get an iterator over all environments in the scope, bottom-to-top
    fn chain(&self) -> impl Iterator<Item = &Environment> {
        self.stack
            .iter()
            .rev()
            .chain(iter::once(&self.root))
            .chain(iter::once(&self.globals.environment))
    }
}

/// The global immutable environment. This is defined one when an engine is
/// initialized and shared among all processes and threads. The global
/// environment includes only the PetitScript standard library. All
/// user-provided native functions must be defined in separate modules.
///
/// After construction, this should always be wrapped in an `Arc` so it can be
/// shared cheaply.
#[derive(Debug, Default)]
pub struct GlobalEnvironment {
    environment: Environment,
    /// A set of bindings scoped to a particular type, such as `Array.map`.
    /// Prototypes are restricted to only storing functions, as there is no use
    /// for constants in the prototype. Prototypes cannot be extended or
    /// modified by users, so this exists only in the global environment.
    prototypes: IndexMap<ValueType, Prototype>,
}

impl GlobalEnvironment {
    /// Declare a single name in this scope and bind it to a value
    pub fn declare(
        &mut self,
        name: impl Into<String>,
        value: impl Into<Value>,
    ) {
        self.environment.declare(name.into(), value.into());
    }

    /// Set the entire prototype for a single type. If the type already has a
    /// prototype defined, panic as that's a bug
    pub fn declare_prototype(
        &mut self,
        value_type: ValueType,
        prototype: Prototype,
    ) {
        if self.prototypes.insert(value_type, prototype).is_some() {
            panic!("Prototype already defined for type {value_type}");
        }
    }

    /// Create a new lexical scope with this global environment as the parent
    pub fn scope(self: Arc<Self>) -> Scope {
        Scope {
            globals: self,
            root: Environment::default(),
            stack: Vec::new(),
        }
    }
}

/// A set of unique names, each bound to a value. This is a flat map, with no
/// hierarchy. For hierarchical scoping, see [Scope].
#[derive(Clone, Debug, Default)]
struct Environment(IndexMap<String, Value>);

impl Environment {
    fn declare(&mut self, name: String, value: Value) {
        self.0.insert(name, value);
    }

    fn get(&self, name: &str) -> Option<&Value> {
        self.0.get(name)
    }
}

/// A set of functions accessible to all values of a single type. Each of these
/// functions must be bound to a value before being called. As such, the
/// included native functions must point to a bound function, not a static one.
#[derive(Clone, Debug, Default)]
pub struct Prototype(IndexMap<String, NativeFunctionId>);

impl Prototype {
    /// Add a function to this prototype
    pub fn declare(
        &mut self,
        name: impl Into<String>,
        definition_id: NativeFunctionId,
    ) {
        self.0.insert(name.into(), definition_id);
    }
}
