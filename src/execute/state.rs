//! Program state management

use crate::{
    ast::{
        source::{QualifiedSpan, StackTraceFrame},
        NodeId,
    },
    compile::Program,
    error::TracedError,
    function::{FunctionId, NativeFunctionDefinition, NativeFunctionId},
    scope::{GlobalEnvironment, Scope},
    value::{Exports, Value},
    Process, RuntimeError,
};
use indexmap::map::Entry;
use std::sync::Arc;

/// TODO
#[derive(Debug)]
pub(super) struct ThreadState<'process> {
    process: &'process Process,
    call_stack: CallStack,
    // TODO comments
    exports: Option<Exports>,
}

impl<'process> ThreadState<'process> {
    pub fn new(
        globals: Arc<GlobalEnvironment>,
        process: &'process Process,
        can_export: bool,
    ) -> Self {
        Self {
            process,
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

    pub fn process(&self) -> &'process Process {
        self.process
    }

    /// Get the program being executed
    pub fn program(&self) -> &Program {
        &self.process.program
    }

    /// Get a native function from the table by its ID
    pub fn native_fn(
        &self,
        id: NativeFunctionId,
    ) -> Result<&NativeFunctionDefinition, RuntimeError> {
        self.process().native_functions.get(id)
    }

    /// Set a named export value
    pub fn export(&mut self, name: String) -> Result<(), RuntimeError> {
        let value = self.scope().get(&name)?;
        let exports = self.exports_mut()?;
        match exports.named.entry(name) {
            Entry::Occupied(entry) => Err(RuntimeError::AlreadyExported {
                name: entry.key().clone(),
            }),
            Entry::Vacant(entry) => {
                entry.insert(value);
                Ok(())
            }
        }
    }

    /// Set the default export value
    pub fn export_default(&mut self, value: Value) -> Result<(), RuntimeError> {
        let exports = self.exports_mut()?;
        if exports.default.is_some() {
            return Err(RuntimeError::AlreadyExported {
                name: "(default)".into(),
            });
        }
        exports.default = Some(value);
        Ok(())
    }

    /// Move the exported values out of this thread state
    pub fn into_exports(self) -> Option<Exports> {
        self.exports
    }

    fn exports_mut(&mut self) -> Result<&mut Exports, RuntimeError> {
        if self.scope().is_root() {
            // If exports aren't defined, it means we were invoked from a
            // function entrypoint. Therefore we're not in the root scope and
            // exports aren't allowed
            self.exports.as_mut().ok_or(RuntimeError::IllegalExport)
        } else {
            // We're not at the root of the source file - no exports allowed!
            Err(RuntimeError::IllegalExport)
        }
    }

    /// Attach a stack trace to the given error, including the given span as the
    /// most recent location in the trace.
    pub fn trace_error(
        &self,
        error: RuntimeError,
        current: impl Into<CallSite>,
    ) -> TracedError {
        let current = current.into();
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
                let span = program.qualify(call_site);

                // Look up the function by its ID. If the lookup fails, this
                // indicates a bug but we're already unrolling an error so just
                // swallow it
                let function_name = frame
                    .function_id
                    .and_then(|id| match id {
                        FunctionId::User(id) => {
                            let function = self
                                .program()
                                .function_table()
                                .get(id.definition_id)
                                .ok()?;
                            let name = function.name.as_ref()?;
                            Some(name.as_str().to_owned())
                        }
                        FunctionId::Native(_) => {
                            // TODO native function definitions should have
                            // names!!
                            None
                        }
                    })
                    .unwrap_or_else(|| "<root>".to_owned());

                StackTraceFrame {
                    span,
                    function_name,
                }
            })
            .collect();
        TracedError { error, trace }
    }

    /// TODO
    pub fn scope(&self) -> &Scope {
        self.call_stack.top()
    }

    /// TODO
    pub fn scope_mut(&mut self) -> &mut Scope {
        self.call_stack.top_mut()
    }

    /// Execute a function within a new frame on the stack
    ///
    /// ## Arguments
    ///
    /// - `scope` - New scope for the function execution
    /// - `call_site` - Source location from which this function was invoked
    /// - `function_id` - ID of the invoked function
    /// - `f` - Rust closure executing the PetitScript function
    pub fn with_frame<T>(
        &mut self,
        scope: Scope,
        call_site: CallSite,
        function_id: FunctionId,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.call_stack.push(scope, call_site, function_id);
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
}

/// The function call stack. The most recent call is always the last element,
/// i.e. on _top_ of the stack. In our machine, a stack frame is mainly just a
/// scope of names. This should not be confused with the hierarchical
/// parent/child structure of scopes. Pushing a new frame is *not* the same as
/// creating a subscope of the current scope.
///
/// Invariant: A call stack can never be empty! The root scope is always the
/// bottom frame of the stack. For root processes, this is the root scope of
/// the program. For function calls, it's the scope of the entrypoint function.
#[derive(Debug)]
pub struct CallStack(Vec<CallFrame>);

impl CallStack {
    pub fn new(globals: Arc<GlobalEnvironment>) -> Self {
        // This root frame can never be popped
        Self(vec![CallFrame {
            call_site: CallSite::Native,
            function_id: None, // This is the entrypoint, so there's no fn yet
            scope: globals.scope(),
        }])
    }

    /// Push a new frame onto the stack, indicating a new function invocation
    fn push(
        &mut self,
        scope: Scope,
        call_site: CallSite,
        function_id: FunctionId,
    ) {
        self.0.push(CallFrame {
            call_site,
            // All stacks after the first must be function calls, so the ID is
            // always present
            function_id: Some(function_id),
            scope,
        });
    }

    fn pop(&mut self) {
        if self.0.len() == 1 {
            // Bug!!
            panic!("Cannot pop final frame of the call stack")
        }
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
    /// The ID of the *invoked* function. `None` only for the root frame, i.e.
    /// top-level module code. Used to get the function name for stack traces
    function_id: Option<FunctionId>,
    /// The location from which this function *was invoked*, i.e. the location
    /// we'll return to when this frame is popped. Used to print the stack
    /// trace
    call_site: CallSite,
}

/// A location from which a function can be called. Typically this is an AST
/// node pointing to the invoking [FunctionCall](super::FunctionCall), but it
/// can also be a point in native code.
#[derive(Copy, Clone, Debug)]
pub enum CallSite {
    /// The current function was invoked from another PS function
    Node(NodeId),
    /// The current functionw as invoked from native code
    Native,
}

impl From<NodeId> for CallSite {
    fn from(node_id: NodeId) -> Self {
        Self::Node(node_id)
    }
}

impl Program {
    /// Convert a call site to a qualified span. For AST nodes, look up the
    /// node's span then qualify that. For native call sites, just return a
    /// native span.
    fn qualify(&self, call_site: CallSite) -> QualifiedSpan {
        match call_site {
            CallSite::Node(node_id) => {
                let span = self.spans().get(node_id).expect("TODO panic here");
                self.sources().qualify(span)
            }
            CallSite::Native => QualifiedSpan::Native,
        }
    }
}
