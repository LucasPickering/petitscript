use crate::{
    ast::{Binding, Identifier},
    function::Captures,
    value::Value,
    RuntimeError,
};
use indexmap::IndexMap;
use std::mem;

/// A scope is a hierarchical linked list of bindings. Each set of bindings is
/// known as an "environment". When we enter a new lexical scope (defined by
/// braces: `{}`), we add a new environment to the scope. When we exit the
/// scope, we remove the environment.
#[derive(Clone, Debug, Default)]
pub struct Scope {
    /// TODO
    parent: Option<Box<Self>>,
    /// TODO
    bindings: Bindings,
}

impl Scope {
    /// Create a new empty scope
    pub fn new() -> Self {
        Self::default()
    }

    /// Is this the highest level scope? The global scope contains only the
    /// standard library and user-provided native functions.
    pub fn is_global(&self) -> bool {
        self.parent.is_none()
    }

    /// Is this the root scope of a process? The root scope is the topmost
    /// lexical scope in a source file. It is always the _second_ scope in the
    /// chain (after the global scope), and is the only scope that allows
    /// exports.
    pub fn is_root(&self) -> bool {
        self.parent
            .as_ref()
            .is_some_and(|parent| parent.is_global())
    }

    /// Create a new scope that's a child of this one. The child will have
    /// access to all of the parent's existing bindings, but any new
    /// declarations will occur in the child
    pub fn child(self) -> Self {
        if self.is_root() {
            panic!("Cannot create a child scope of the root scope");
        }
        Self {
            parent: Some(Box::new(self)),
            bindings: Bindings::default(),
        }
    }

    /// Replace the current scope with a subscope of itself. This should be
    /// called right before entering a new lexical scope block.
    pub fn push_subscope(&mut self) {
        let parent = mem::take(self);
        // Self is now the child
        self.parent = Some(Box::new(parent));
    }

    /// Replace the current scope with its parent, throwing away this scope's
    /// bindings. This should be called immediately after exiting a lexical
    /// scope. This should never be called on the global scope; that indicates
    /// we're popping more than we're pushing
    pub fn pop_subscope(&mut self) {
        if let Some(parent) = self.parent.take() {
            // In most cases we'll be the only pointer to the parent so we can
            // reclaim the original scope. If we got forked though, we'll have
            // to clone it
            *self = *parent;
        } else {
            panic!("Cannot pop the global scope");
        }
    }

    /// Declare a single name in scope, and bind it to a variable
    pub fn declare(
        &mut self,
        name: impl Into<String>,
        value: impl Into<Value>,
    ) {
        self.bindings.declare(name.into(), value.into());
    }

    /// Get the value of a binding. Return an error if the binding doesn't exist
    /// in scope.
    pub fn get(&self, name: &str) -> Result<Value, RuntimeError> {
        match self.bindings.get(name) {
            Some(value) => Ok(value.clone()),
            None => {
                if let Some(parent) = self.parent.as_ref() {
                    parent.get(name)
                } else {
                    Err(RuntimeError::Reference {
                        name: name.to_owned(),
                    })
                }
            }
        }
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
}

/// A set of unique names, each bound to a value. This is a flat map, with no
/// hierarchy. For hierarchical scoping, see [Scope].
/// TODO rename to not overlap with AST Binding type
#[derive(Clone, Debug, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
pub struct Bindings(IndexMap<String, Value>);

impl Bindings {
    /// TODO
    fn declare(&mut self, name: String, value: Value) {
        self.0.insert(name, value);
    }

    /// TODO
    fn get(&self, name: &str) -> Option<&Value> {
        self.0.get(name)
    }
}
