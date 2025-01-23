use crate::value::ValueKind;
use boa_ast::declaration::VariableList;
use indexmap::IndexMap;
use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

/// TODO
#[derive(Debug, Default)]
pub struct Scope {
    parent: Option<Bindings>,
    inner: Bindings,
}

impl Scope {
    /// Create a new empty scope
    pub fn new() -> Self {
        Self::default()
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
    pub fn get(&self, name: &str) -> Option<Ref<'_, ValueKind>> {
        self.inner
            .get(name)
            .or_else(|| self.parent.as_ref()?.get(name))
    }

    /// TODO
    pub fn declare(&mut self, name: String, value: ValueKind, mutable: bool) {
        self.inner
            .bindings
            .borrow_mut()
            .insert(name, Binding { value, mutable });
    }

    // TODO
    pub fn declare_all(&mut self, variables: &VariableList, mutable: bool) {
        todo!()
    }

    /// TODO
    pub fn set(&mut self, value: ValueKind) {
        todo!()
    }
}

/// TODO
#[derive(Debug, Default)]
struct Bindings {
    bindings: Rc<RefCell<IndexMap<String, Binding>>>,
}

impl Bindings {
    /// TODO
    fn get(&self, name: &str) -> Option<Ref<'_, ValueKind>> {
        Ref::filter_map(self.bindings.borrow(), |bindings| {
            bindings.get(name).map(|binding| &binding.value)
        })
        .ok()
    }

    /// TODO
    fn set(&self, name: &str) {
        todo!()
    }
}

/// TODO
#[derive(Debug)]
struct Binding {
    value: ValueKind,
    mutable: bool,
}
