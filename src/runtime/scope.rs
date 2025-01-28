use crate::{stdlib::stdlib, value::Value, Error, Result};
use indexmap::IndexMap;
use std::{cell::RefCell, rc::Rc};

/// TODO
#[derive(Clone, Debug, Default)]
pub struct Scope(Rc<ScopeInner>);

impl Scope {
    /// Create a new empty scope
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a global scope, which will include the standard library
    pub fn global() -> Self {
        let lib = stdlib();
        let scope = Self::new();
        for (name, value) in lib.named {
            scope.declare(name, value, false);
        }
        scope
    }

    /// TODO
    pub fn child(&self) -> Self {
        Self(
            ScopeInner {
                parent: Some(self.clone()),
                bindings: Default::default(),
            }
            .into(),
        )
    }

    /// TODO
    pub fn declare(&self, name: String, value: Value, mutable: bool) {
        self.0.bindings.declare(name, value, mutable);
    }

    /// TODO
    pub fn get(&self, name: &str) -> Result<Value> {
        match self.0.bindings.get(name) {
            Some(value) => Ok(value),
            None => {
                if let Some(parent) = self.0.parent.as_ref() {
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
        match self.0.bindings.set(name, value) {
            SetOutcome::NotDefined(value) => {
                if let Some(parent) = self.0.parent.as_ref() {
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
}

#[derive(Debug, Default)]
struct ScopeInner {
    parent: Option<Scope>,
    bindings: Bindings,
}

/// TODO
#[derive(Debug, Default)]
struct Bindings {
    bindings: RefCell<IndexMap<String, Binding>>,
}

impl Bindings {
    /// TODO
    fn declare(&self, name: String, value: Value, mutable: bool) {
        self.bindings
            .borrow_mut()
            .insert(name, Binding { value, mutable });
    }

    /// TODO
    fn get(&self, name: &str) -> Option<Value> {
        self.bindings
            .borrow()
            .get(name)
            .map(|binding| binding.value.clone())
    }

    /// TODO
    fn set(&self, name: &str, value: Value) -> SetOutcome {
        let mut bindings = self.bindings.borrow_mut();
        match bindings.get_mut(name) {
            Some(binding) if binding.mutable => {
                binding.value = value;
                SetOutcome::Ok
            }
            Some(_) => SetOutcome::Err(Error::ImmutableAssign {
                name: name.to_owned(),
            }),
            None => SetOutcome::NotDefined(value),
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

/// TODO
#[derive(Debug)]
struct Binding {
    value: Value,
    mutable: bool,
}
