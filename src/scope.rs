use crate::{
    ast::{Binding, Identifier},
    function::Captures,
    value::Value,
    RuntimeError,
};
use indexmap::IndexMap;
use std::{mem, sync::Arc};

/// TODO
#[derive(Clone, Debug, Default)]
pub struct Scope {
    /// TODO
    parent: Option<Arc<Self>>,
    /// TODO
    bindings: Bindings,
}

impl Scope {
    /// Create a new empty global scope
    pub fn global() -> Self {
        Self::default()
    }

    /// Create a new scope that's a child of this one. The child will have
    /// access to all of the parent's existing bindings, but any new
    /// declarations will occur in the child
    pub fn child(self) -> Self {
        Self {
            parent: Some(Arc::new(self)),
            bindings: Bindings::default(),
        }
    }

    /// TODO
    pub fn subscope(&mut self) {
        let parent = mem::take(self);
        // Self is now the child
        self.parent = Some(Arc::new(parent));
    }

    /// TODO
    pub fn revert(&mut self) -> Result<(), RuntimeError> {
        if let Some(parent) = self.parent.take() {
            // In most cases we'll be the only pointer to the parent so we can
            // reclaim the original scope. If we got forked though, we'll have
            // to clone it
            *self = Arc::unwrap_or_clone(parent);
            Ok(())
        } else {
            Err(RuntimeError::internal("TODO"))
        }
    }

    /// Declare a single name in scope, and bind it to a variable
    pub fn declare(&mut self, name: impl ToString, value: Value) {
        self.bindings.declare(name.to_string(), value);
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
    /// may be bound.
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
    /// needs, grab its value. If the value is part of the global scope (either
    /// the standard library or a native function), it will _not_ be captured.
    /// The function must be executed in this same process, and therefore the
    /// same global scope will be available.
    ///
    /// If an identifier is needed but not available, return an error. This
    /// indicates a compiler bug. The compiler should ensure that any captured
    /// names are available in a parent scope. An invalid reference here means
    /// the compiler messed up.
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
