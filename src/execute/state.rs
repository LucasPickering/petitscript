//! Program state management

use crate::{
    ast::source::Span,
    compile::Program,
    error::TracedError,
    scope::Scope,
    value::{Exports, Value},
    Error, Process, RuntimeError,
};
use indexmap::map::Entry;
use std::iter;

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
        globals: Scope,
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
        current: Span,
    ) -> TracedError {
        let trace = self
            .call_stack
            .0
            .iter()
            .map(|frame| frame.call_site)
            // Append the exact error location to the end of the trace
            .chain(iter::once(current))
            .collect();
        TracedError { error, trace }
    }

    /// Map the raw byte offsets in each span of an error's the stack trace to
    /// lines and columns. This allows the error to be displayed prettily to
    /// the user.
    pub fn qualify_error(&self, error: TracedError) -> Error {
        let TracedError { error, trace } = error;
        Error::Runtime {
            error,
            trace: trace
                .into_iter()
                .map(|span| span.qualify(self.program().source()))
                .collect(),
        }
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
    pub fn with_frame<T>(
        &mut self,
        scope: Scope,
        call_site: Span,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.call_stack.push(scope, call_site);
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
    pub fn new(globals: Scope) -> Self {
        // This root frame can never be popped
        Self(vec![CallFrame {
            call_site: Span::Native,
            scope: globals.child(),
        }])
    }

    fn push(&mut self, scope: Scope, call_site: Span) {
        self.0.push(CallFrame { call_site, scope });
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
    /// The location from which this function was invoked, i.e. the location
    /// we'll return to when this frame is popped. Used to print the stack
    /// trace during unwinding
    call_site: Span,
}
