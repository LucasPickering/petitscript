//! Fibers are individual program invocations

use crate::{
    RuntimeErrorKind, World,
    ast::{ArrayPatternElementKind, Binding, BindingKind, Node, NodeId},
    compile::Program,
    error::{RuntimeError, StackTraceFrame},
    execute::{
        Exports,
        eval::Evaluate,
        scope::{GlobalEnvironment, Scope},
    },
    util::OptionExt,
    value::{Array, Function, Object, Value},
};
use indexmap::{IndexMap, map::Entry};
use std::{collections::VecDeque, sync::Arc};

/// Running invocation of a [`World`]
///
/// A fiber can be spawned at a program's entrypoint, or by invoking a function
/// that was previously returned from the world. The fiber holds all the state
/// that statement execution will modify.
///
/// This is an internal concept. Because fibers are not interruptible, they
/// exist only for the duration of a single external invocation.
#[derive(Debug)]
pub(super) struct Fiber<'world> {
    /// World that this fiber is executing in
    world: &'world World,
    /// Current function call stack
    call_stack: CallStack,
    /// Exported values
    ///
    /// This is built procedurally as we execute and is the final result of
    /// execution. `None` for invocations that don't support exporting
    /// (function calls).
    exports: Option<Exports>,
}

impl<'world> Fiber<'world> {
    /// Create a new fiber
    ///
    /// `can_export` is `true` for entrypoint invocations, `false` for function
    /// calls.
    pub fn new(
        globals: Arc<GlobalEnvironment>,
        world: &'world World,
        can_export: bool,
    ) -> Self {
        Self {
            world,
            call_stack: CallStack::new(globals),
            // Function invocations cannot export, since they're not at the root
            // scope
            exports: if can_export {
                Some(Exports::default())
            } else {
                None
            },
        }
    }

    /// Get the world this fiber is executing in
    pub fn world(&self) -> &'world World {
        self.world
    }

    /// Get the program being executed
    pub fn program(&self) -> &Program {
        &self.world.program
    }

    /// Set a named export value
    pub fn export(&mut self, name: String) -> Result<(), RuntimeErrorKind> {
        let value = self.scope().get(&name)?;
        let exports = self.exports_mut()?;
        match exports.named.entry(name) {
            Entry::Occupied(entry) => Err(RuntimeErrorKind::AlreadyExported {
                name: entry.key().clone(),
            }),
            Entry::Vacant(entry) => {
                entry.insert(value);
                Ok(())
            }
        }
    }

    /// Set the default export value
    pub fn export_default(
        &mut self,
        value: Value,
    ) -> Result<(), RuntimeErrorKind> {
        let exports = self.exports_mut()?;
        if exports.default.is_some() {
            return Err(RuntimeErrorKind::AlreadyExported {
                name: "(default)".into(),
            });
        }
        exports.default = Some(value);
        Ok(())
    }

    /// Move the exported values out of this fiber
    pub fn into_exports(self) -> Option<Exports> {
        self.exports
    }

    fn exports_mut(&mut self) -> Result<&mut Exports, RuntimeErrorKind> {
        if self.scope().is_root() {
            // If exports aren't defined, it means we were invoked from a
            // function entrypoint. Therefore we're not in the root scope and
            // exports aren't allowed
            self.exports.as_mut().ok_or(RuntimeErrorKind::IllegalExport)
        } else {
            // We're not at the root of the source file - no exports allowed!
            Err(RuntimeErrorKind::IllegalExport)
        }
    }

    /// Attach a stack trace to the given error, including the given call site
    /// as the most recent location in the trace
    ///
    /// Typically, `location` is the nearest AST [`Node`] to the error.
    pub fn trace_error(
        &self,
        error: RuntimeErrorKind,
        location: impl Into<Location>,
    ) -> RuntimeError {
        let current = location.into();
        let program = self.program();
        let trace = self
            .call_stack
            .0
            .iter()
            .enumerate()
            .map(|(i, frame)| {
                // Grab the call site from the _next_ stack frame, because that
                // tells us where we exited _this_ frame. If this is the topmost
                // frame, we have no exit point so use the given current span
                let next_frame = self.call_stack.0.get(i + 1);
                let call_site =
                    next_frame.map(|frame| frame.call_site).unwrap_or(current);
                // Convert bytes to lines and columns
                let span = match call_site {
                    // This lookup *should* always hit, but shit happens
                    Location::Node(node_id) => program.spans().get(node_id),
                    Location::Native => None,
                };

                let function_name = frame
                    .function_name
                    .as_deref()
                    .unwrap_or(Function::ANONYMOUS)
                    .to_owned();
                StackTraceFrame {
                    span,
                    function_name,
                }
            })
            .collect();
        RuntimeError {
            error,
            trace,
            source_table: program.sources().clone(),
        }
    }

    /// Get the current scope of bound names
    pub fn scope(&self) -> &Scope {
        self.call_stack.top()
    }

    /// Get a mutable reference to the current scope of bound names
    ///
    /// Use this to push, pop, and modify environments in the scope.
    pub fn scope_mut(&mut self) -> &mut Scope {
        self.call_stack.top_mut()
    }

    /// Execute a function within a new frame on the stack
    ///
    /// # Arguments
    ///
    /// - `scope` - New scope for the function execution
    /// - `call_site` - Source location from which this function was invoked
    /// - `function_id` - ID of the invoked function
    /// - `f` - Rust closure executing the PetitScript function
    pub fn with_frame<T>(
        &mut self,
        scope: Scope,
        call_site: Location,
        function_name: Option<String>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.call_stack.push(scope, call_site, function_name);
        let value = f(self);
        self.call_stack.pop();
        value
    }

    /// Execute a function in a new scope that's a child of the current scope.
    /// Use this for blocks such as ifs, loops, etc.
    pub fn with_subscope<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.scope_mut().push_subscope();
        let value = f(self);
        self.scope_mut().pop_subscope();
        value
    }

    /// Declare a new binding in the current scope
    ///
    /// The binding can be a single identifier, or a structured identifier, in
    /// which case multiple names may be bound. This is a convenience method for
    /// calling [`Self::declare`] on each name in the binding.
    ///
    /// Object/array bindings may involve evaluating inner expressions, such
    /// as:
    /// - `const [a = 1] = arr;`
    /// - `const { a = 1 } = o;`
    /// - `const { [f()]: a } = o;`
    pub(crate) async fn bind(
        &mut self,
        binding: &Binding,
        value: Value,
    ) -> Result<Vec<String>, RuntimeError> {
        match &binding.kind {
            BindingKind::Identifier(identifier) => {
                let name = identifier.as_str().to_owned();
                self.scope_mut().declare(name.clone(), value);
                Ok(vec![name])
            }
            BindingKind::Object { properties, rest } => {
                // Value must be an object. Copy its properties to another map
                // so we can drain them as we bind them. (neede
                // for ... bindings)
                let object = value
                    .try_into_object()
                    .map_err(|error| self.trace_error(error.into(), binding))?;
                let mut values = IndexMap::from(object);
                let mut names = vec![];

                for property in properties {
                    let key = property.key.eval(self).await?.to_string();
                    // Calculate the bound value:
                    // property value -> init expression -> undefined
                    let value = values
                        // shift_remove retains order (important!)
                        .shift_remove(key.as_str())
                        .or_else_try(|| {
                            property
                                .init
                                .as_ref()
                                .try_map(|init| init.eval(self))
                        })?
                        .unwrap_or(Value::Undefined);
                    names.extend(self.bind(&property.binding, value).await?);
                }
                if let Some(rest) = rest {
                    // Grab the remaining properties
                    let value = values.drain(..).collect::<Object>();
                    names.extend(self.bind(rest, value.into()).await?);
                }

                Ok(names)
            }
            BindingKind::Array { elements, rest } => {
                // Value must be an array. Copy its elements to another array
                // so we can drain them as we bind them. (needed for ...
                // bindings)
                let array = value
                    .try_into_array()
                    .map_err(|error| self.trace_error(error.into(), binding))?;
                // We'll be popping off the front, so a deque is better
                let mut values = VecDeque::from_iter(array);
                let mut names = vec![];

                for element in elements {
                    match &element.kind {
                        ArrayPatternElementKind::Identifier {
                            binding,
                            init,
                        } => {
                            // Calculate the bound value:
                            // element value -> init expression -> undefined
                            let value = values
                                .pop_front()
                                .or_else_try(|| {
                                    init.as_ref()
                                        .try_map(|init| init.eval(self))
                                })?
                                .unwrap_or(Value::Undefined);
                            names.extend(self.bind(binding, value).await?);
                        }
                        ArrayPatternElementKind::Skip => {
                            values.pop_front(); // Skip this element
                        }
                    }
                }

                if let Some(rest) = rest {
                    // Grab the remaining elements
                    let value = values.drain(..).collect::<Array>();
                    names.extend(self.bind(rest, value.into()).await?);
                }

                Ok(names)
            }
        }
    }
}

/// The function call stack
///
/// The most recent call is always the last element, i.e. on _top_ of the stack.
/// In our machine, a stack frame is mainly just a scope of names. This should
/// not be confused with the hierarchical parent/child structure of scopes.
/// Pushing a new frame is *not* the same as creating a subscope of the current
/// scope.
///
/// Invariant: A call stack can never be empty! The root scope is always the
/// bottom frame of the stack. For root fibers, this is the root scope of the
/// program. For function calls, it's the scope of the entrypoint function.
#[derive(Debug)]
pub struct CallStack(Vec<CallFrame>);

impl CallStack {
    pub fn new(globals: Arc<GlobalEnvironment>) -> Self {
        // This root frame can never be popped
        Self(vec![CallFrame {
            call_site: Location::Native,
            // There's no fn call yet, so we have to make up a name
            function_name: Some("<root>".into()),
            scope: globals.scope(),
        }])
    }

    /// Push a new frame onto the stack, indicating a new function invocation
    fn push(
        &mut self,
        scope: Scope,
        call_site: Location,
        function_name: Option<String>,
    ) {
        self.0.push(CallFrame {
            scope,
            function_name,
            call_site,
        });
    }

    fn pop(&mut self) {
        assert!(
            self.0.len() != 1,
            "Cannot pop final frame of the call stack"
        );
        self.0.pop();
    }

    /// Get the frame on top of the stack. As the stack can never be empty, this
    /// always returns something
    fn top(&self) -> &Scope {
        &self.0.last().expect("Call stack can never be empty").scope
    }

    /// Get a mutable reference to the frame on top of the stack. As the stack
    /// can never be empty, this always returns something
    fn top_mut(&mut self) -> &mut Scope {
        &mut self
            .0
            .last_mut()
            .expect("Call stack can never be empty")
            .scope
    }
}

/// A single frame in the call stack. This holds the frame's scope, as well as
/// metadata about the call
#[derive(Debug)]
struct CallFrame {
    /// Scope inside the function, including captured variables
    scope: Scope,
    /// The name of the *invoked* function. `None` only for the root frame,
    /// i.e. top-level module code. This will have to be cloned from the
    /// function for each call, which is potentially expensive. Could optimize
    /// by interning function names. `None` only for calls to anonymous
    /// functions
    function_name: Option<String>,
    /// The location from which this function *was invoked*, i.e. the location
    /// we'll return to when this frame is popped. Used to print the stack
    /// trace
    call_site: Location,
}

/// A location in executing code
///
/// This is used to track the locations of errors and function calls. Typically
/// this points to an AST node by ID, but it can also be a point in native code.
#[derive(Copy, Clone, Debug)]
pub enum Location {
    /// A location in the PetitScript AST
    Node(NodeId),
    /// An unknown location in native code
    Native,
}

impl<T: Node> From<&T> for Location {
    fn from(node: &T) -> Self {
        Self::Node(node.id())
    }
}
