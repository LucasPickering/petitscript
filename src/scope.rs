use crate::{
    ast::{Binding, Identifier},
    function::{Captures, NativeFunctionId},
    value::Value,
    RuntimeError, ValueType,
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
    /// A set of bindings scoped to a particular type, such as `Array.map`.
    /// Prototypes are restricted to only storing functions, as there is no use
    /// for constants in the prototype. Prototypes can only be defined on the
    /// global scope, but are included in every scope for simplicity. This map
    /// should be empty for all child scopes.
    prototypes: IndexMap<ValueType, Prototype>,
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
            prototypes: IndexMap::default(),
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

    /// Get a function definition from a specific type's prototype
    pub(crate) fn get_prototype(
        &self,
        value_type: ValueType,
        name: &str,
    ) -> Option<NativeFunctionId> {
        let definition_id = self
            .prototype(value_type)
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

    /// Find the prototype for a specific type. Recursively walk up the scope
    /// stack until we find it (it should always be in the global scope).
    fn prototype(&self, value_type: ValueType) -> Option<&Prototype> {
        self.prototypes
            .get(&value_type)
            .or_else(|| self.parent.as_ref()?.prototype(value_type))
    }
}

/// A set of unique names, each bound to a value. This is a flat map, with no
/// hierarchy. For hierarchical scoping, see [Scope].
/// TODO rename to not overlap with AST Binding type
#[derive(Clone, Debug, Default)]
struct Bindings(IndexMap<String, Value>);

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

/// A set of functions accessible to all values of a single type. Each of these
/// functions must be bound to a value before being called. As such, the
/// included native functions must point to a bound function, not a static one.
#[derive(Clone, Debug, Default)]
pub struct Prototype(IndexMap<String, NativeFunctionId>);

impl Prototype {
    /// Add a function to this prototype
    pub(crate) fn declare(
        &mut self,
        name: impl Into<String>,
        definition_id: NativeFunctionId,
    ) {
        self.0.insert(name.into(), definition_id);
    }
}
