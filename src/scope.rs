use crate::{
    ast,
    function::{Function, FunctionDefinition, FunctionId},
    value::Value,
    RuntimeError,
};
use indexmap::IndexMap;
use std::{
    collections::HashMap,
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
    pub fn declare(
        &mut self,
        name: impl ToString,
        value: Value,
        mutable: bool,
    ) {
        self.bindings.declare(name.to_string(), value, mutable);
    }

    /// TODO
    pub fn create_function(
        &mut self,
        definition: FunctionDefinition,
    ) -> Function {
        let id: FunctionId = FunctionId::new(0); // TODO use real process ID
        let function = Function::new(id, definition.name.clone());
        self.bindings.functions.insert(id, definition.into());
        function
    }

    /// Look up a function definition by its unique ID. Return an error if the
    /// function doesn't exist in this scope. This can occur if a user tried
    /// to call a function from a process that it didn't originate from.
    ///
    /// TODO explain tuple
    pub fn get_function_definition(
        &self,
        function: &Function,
    ) -> Result<(Scope, Arc<FunctionDefinition>), RuntimeError> {
        match self.bindings.get_function(function.id()) {
            Some(definition) => Ok((self.clone(), definition)),
            None => {
                if let Some(parent) = self.parent.as_ref() {
                    parent.get_function_definition(function)
                } else {
                    Err(RuntimeError::UnknownFunction {
                        function: function.clone(),
                    })
                }
            }
        }
    }

    /// Get the value of a binding. Return an error if the binding doesn't exist
    /// in scope.
    pub fn get(&self, name: &str) -> Result<Value, RuntimeError> {
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

    /// Set the value of an existing binding. Return an error if the binding
    /// doesn't existing in scope or isn't mutable.
    pub fn set(&self, name: &str, value: Value) -> Result<(), RuntimeError> {
        match self.bindings.set(name, value) {
            SetOutcome::NotDefined(value) => {
                if let Some(parent) = self.parent.as_ref() {
                    parent.set(name, value)
                } else {
                    // Var isn't defined anywhere
                    Err(RuntimeError::Reference {
                        name: name.to_string(),
                    })
                }
            }
            SetOutcome::Ok => Ok(()),
            SetOutcome::Err(error) => Err(error),
        }
    }

    /// Declare a new binding in this scope. The binding can be a single
    /// identifier, or a structured identifier, in which case multiple names
    /// may be bound.
    pub fn bind(
        &mut self,
        binding: &ast::Binding,
        value: Value,
        mutable: bool,
    ) -> Result<Vec<String>, RuntimeError> {
        match binding {
            ast::Binding::Identifier(identifier) => {
                let name = identifier.as_str().to_owned();
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
    /// TODO
    bindings: IndexMap<String, Binding>,
    /// TODO
    functions: HashMap<FunctionId, Arc<FunctionDefinition>>,
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
    fn get_function(&self, id: FunctionId) -> Option<Arc<FunctionDefinition>> {
        self.functions.get(&id).map(Arc::clone)
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
                    name: name.to_string(),
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
