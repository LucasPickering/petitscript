use crate::{ast, error::RuntimeResult, value::Value, RuntimeError};
use indexmap::IndexMap;
use std::{
    mem,
    sync::{Arc, RwLock},
};

/// TODO
#[derive(Clone, Debug, Default)]
pub struct Scope {
    /// TODO
    parent: Option<Arc<Self>>,
    /// TODO
    bindings: Bindings,
}

impl Scope {
    /// Create a new empty scope
    pub fn new() -> Self {
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
    pub fn revert(&mut self) -> RuntimeResult<()> {
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

    /// TODO
    pub fn capture(&self) -> Self {
        // TODO flatten the scope
        self.clone()
    }

    /// TODO
    pub fn declare(
        &mut self,
        name: impl ToString,
        value: Value,
        mutable: bool,
    ) {
        self.bindings.declare(name.to_string(), value, mutable);
    }

    /// TODO
    pub fn get(&self, name: &str) -> RuntimeResult<Value> {
        match self.bindings.get(name) {
            Some(value) => Ok(value),
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

    /// TODO
    pub fn set(&self, name: &str, value: Value) -> RuntimeResult<()> {
        match self.bindings.set(name, value) {
            SetOutcome::NotDefined(value) => {
                if let Some(parent) = self.parent.as_ref() {
                    parent.set(name, value)
                } else {
                    // Var isn't defined anywhere
                    Err(RuntimeError::Reference {
                        name: name.to_owned(),
                    })
                }
            }
            SetOutcome::Ok => Ok(()),
            SetOutcome::Err(error) => Err(error),
        }
    }

    /// TODO
    pub fn bind(
        &mut self,
        binding: &ast::Binding,
        value: Value,
        mutable: bool,
    ) -> RuntimeResult<Vec<String>> {
        match binding {
            ast::Binding::Identifier(identifier) => {
                let name = identifier.to_str().to_owned();
                self.declare(name.clone(), value, mutable);
                Ok(vec![name])
            }
            ast::Binding::Object(_) => todo!(),
            ast::Binding::Array(_) => todo!(),
        }
    }
}

/// TODO
/// TODO rename to not overlap with AST Binding type
#[derive(Clone, Debug, Default)]
struct Bindings {
    bindings: IndexMap<String, Binding>,
}

impl Bindings {
    /// TODO
    fn declare(&mut self, name: String, value: Value, mutable: bool) {
        let binding = if mutable {
            Binding::Mutable(Arc::new(RwLock::new(value)))
        } else {
            Binding::Immutable(value)
        };
        self.bindings.insert(name, binding);
    }

    /// TODO
    fn get(&self, name: &str) -> Option<Value> {
        self.bindings.get(name).map(|binding| binding.value())
    }

    /// TODO
    fn set(&self, name: &str, value: Value) -> SetOutcome {
        match self.bindings.get(name) {
            Some(Binding::Mutable(binding)) => {
                *binding.write().expect("TODO") = value;
                SetOutcome::Ok
            }
            Some(Binding::Immutable(_)) => {
                SetOutcome::Err(RuntimeError::ImmutableAssign {
                    name: name.to_owned(),
                })
            }
            None => SetOutcome::NotDefined(value),
        }
    }
}

/// TODO
#[derive(Clone, Debug)]
enum Binding {
    Immutable(Value),
    Mutable(Arc<RwLock<Value>>),
}

impl Binding {
    /// Get a (refcounted) clone of the contained value
    fn value(&self) -> Value {
        match self {
            Self::Immutable(value) => value.clone(),
            Self::Mutable(value) => Value::clone(&*value.read().expect("TODO")),
        }
    }
}

enum SetOutcome {
    /// Value isn't defined in this scope. Hand the value back so the caller
    /// can walk up the scope tree
    NotDefined(Value),
    /// Value was set
    Ok,
    /// Fatal error setting the value
    Err(RuntimeError),
}
