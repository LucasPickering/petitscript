use crate::{stdlib::stdlib, value::Value, Error, Result};
use indexmap::IndexMap;
use std::{cell::RefCell, rc::Rc};

/// TODO
#[derive(Clone, Debug, Default)]
pub struct Scope {
    parent: Option<Bindings>,
    inner: Bindings,
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
    pub fn child(&self) -> Self {
        Self {
            parent: Some(Bindings {
                bindings: Rc::clone(&self.inner.bindings),
            }),
            inner: Default::default(),
        }
    }

    /// TODO
    pub fn get(&self, name: &str) -> Result<Value> {
        self.inner
            .get(name)
            // Check the parent
            .or_else(|| self.parent.as_ref()?.get(name))
            // No go!
            .ok_or_else(|| Error::Reference {
                name: name.to_owned(),
            })
    }

    /// TODO
    pub fn declare(&mut self, name: String, value: Value, mutable: bool) {
        self.inner
            .bindings
            .borrow_mut()
            .insert(name, Binding { value, mutable });
    }

    /// TODO
    pub fn set(&mut self, name: &str, value: Value) -> Result<()> {
        // TODO recursion
        self.inner.set(name, value)
    }
}

/// TODO
#[derive(Clone, Debug, Default)]
struct Bindings {
    bindings: Rc<RefCell<IndexMap<String, Binding>>>,
}

impl Bindings {
    /// TODO
    fn get(&self, name: &str) -> Option<Value> {
        self.bindings
            .borrow()
            .get(name)
            .map(|binding| binding.value.clone())
    }

    /// TODO
    fn set(&self, name: &str, value: Value) -> Result<()> {
        let mut bindings = self.bindings.borrow_mut();
        let binding =
            bindings
                .get_mut(name)
                .ok_or_else(|| Error::ImmutableAssign {
                    name: name.to_owned(),
                })?;
        if binding.mutable {
            binding.value = value;
            Ok(())
        } else {
            todo!("error")
        }
    }
}

/// TODO
#[derive(Debug)]
struct Binding {
    value: Value,
    mutable: bool,
}
