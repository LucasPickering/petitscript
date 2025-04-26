//! Program state management

use crate::{
    ast::NodeId,
    compile::Program,
    error::{StackTraceFrame, TracedError},
    execute::{
        scope::{GlobalEnvironment, Scope},
        Exports,
    },
    source::QualifiedSpan,
    value::{
        function::{NativeFunctionDefinition, NativeFunctionId},
        Value,
    },
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

    /// Attach a stack trace to the given error, including the given call site
    /// as the most recent location in the trace.
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

                let function_name =
                    frame.function_name.as_deref().unwrap_or("???").to_owned();
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
            // There's no fn call yet, so we have to make up a name
            function_name: Some("<root>".into()),
            scope: globals.scope(),
        }])
    }

    /// Push a new frame onto the stack, indicating a new function invocation
    fn push(
        &mut self,
        scope: Scope,
        call_site: CallSite,
        function_name: Option<String>,
    ) {
        self.0.push(CallFrame {
            call_site,
            function_name,
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
    /// The name of the *invoked* function. `None` only for the root frame,
    /// i.e. top-level module code. This will have to be cloned from the
    /// function for each call, which is potentially expensive. Could optimize
    /// by interning function names. `None` only for calls to anonymous
    /// functions
    function_name: Option<String>,
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
