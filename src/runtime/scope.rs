use crate::{
    runtime::state::SymbolResolver, stdlib::stdlib, value::Value, Error, Result,
};
use indexmap::IndexMap;
use std::{
    mem,
    sync::{Arc, RwLock},
};

/// TODO
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

    /// Create a global scope, which will include the standard library
    pub fn global() -> Self {
        let lib = stdlib();
        let mut scope = Self::new();
        for (name, value) in lib.named {
            scope.declare(name, value, false);
        }
        scope
    }

    /// TODO
    pub fn subscope(&mut self) {
        let parent = mem::take(self);
        // Self is now the child
        self.parent = Some(Box::new(parent));
    }

    /// TODO
    pub fn revert(&mut self) {
        *self = *self.parent.take().expect("TODO");
    }

    /// TODO
    pub fn capture(&self) -> Self {
        // TODO flatten the scope
        self.clone()
    }

    /// TODO
    pub fn declare(&mut self, name: String, value: Value, mutable: bool) {
        self.bindings.declare(name, value, mutable);
    }

    /// TODO
    pub fn get(&self, name: &str) -> Result<Value> {
        match self.bindings.get(name) {
            Some(value) => Ok(value),
            None => {
                if let Some(parent) = self.parent.as_ref() {
                    parent.get(name)
                } else {
                    Err(Error::Reference {
                        name: name.to_owned(),
                    })
                }
            }
        }
    }

    /// TODO
    pub fn set(&self, name: &str, value: Value) -> Result<()> {
        match self.bindings.set(name, value) {
            SetOutcome::NotDefined(value) => {
                if let Some(parent) = self.parent.as_ref() {
                    parent.set(name, value)
                } else {
                    // Var isn't defined anywhere
                    Err(Error::Reference {
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
        resolver: SymbolResolver,
        binding: &boa_ast::declaration::Binding,
        value: Value,
        mutable: bool,
    ) -> Result<Vec<String>> {
        match binding {
            boa_ast::declaration::Binding::Identifier(identifier) => {
                let name = resolver.resolve(identifier.sym()).to_owned();
                self.declare(name.clone(), value, mutable);
                Ok(vec![name])
            }
            boa_ast::declaration::Binding::Pattern(_) => todo!(),
        }
    }
}

/// TODO
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
                SetOutcome::Err(Error::ImmutableAssign {
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
    Err(Error),
}
