//! Program state management

use crate::{
    ast::source::Spanned,
    compile::Program,
    scope::Scope,
    value::{Exports, Value},
    Error, Process, RuntimeError,
};
use std::collections::hash_map::Entry;

/// TODO
#[derive(Debug)]
pub(super) struct ThreadState<'a> {
    process: &'a Process,
    call_stack: CallStack,
    // TODO comments
    exports: Option<Exports>,
}

impl<'a> ThreadState<'a> {
    pub fn new(globals: Scope, process: &'a Process, can_export: bool) -> Self {
        Self {
            process,
            call_stack: CallStack::new(globals),
            exports: if can_export {
                Some(Exports::default())
            } else {
                None
            },
        }
    }

    pub fn process(&self) -> &Process {
        self.process
    }

    /// Get the program being executed
    pub fn program(&self) -> &Program {
        &self.process.program
    }

    /// TODO
    pub fn export(&mut self, name: String) -> Result<(), RuntimeError> {
        let value = self.scope().get(&name)?;
        let exports =
            self.exports.as_mut().ok_or(RuntimeError::IllegalExport)?;

        // TODO only allow root scope to be exported
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

    /// TODO
    pub fn export_default(&mut self, value: Value) -> Result<(), RuntimeError> {
        let exports =
            self.exports.as_mut().ok_or(RuntimeError::IllegalExport)?;
        // TODO only allow root scope to be exported
        if exports.default.is_some() {
            return Err(RuntimeError::AlreadyExported {
                name: "(default)".into(),
            });
        }
        exports.default = Some(value);
        Ok(())
    }

    pub fn into_exports(self) -> Option<Exports> {
        self.exports
    }

    /// Map an error with a raw byte span to lines and columns, bound to a
    /// particular source. This allows the error to be returned to the user and
    /// displayed prettily.
    pub fn qualify_error(&self, error: Spanned<RuntimeError>) -> Error {
        Error::Runtime {
            error: error.data,
            span: error.span.qualify(self.program().source()),
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
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.call_stack.push(scope);
        let value = f(self);
        self.call_stack.pop();
        value
    }

    /// Execute a function in a new scope that's a child of the current scope.
    /// Use this for blocks such as ifs, loops, etc.
    pub fn with_subscope<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.scope_mut().subscope();
        let value = f(self);
        self.scope_mut().revert();
        value
    }
}

/// The function call stack. In our machine, a stack frame is simply a
/// scope of names. This should not be confused with the hierarchical
/// parent/child structure of frames. Pushing a new frame is *not* the same
/// as creating a subscope of the current scope.
///
/// Invariant: A call stack can never be empty! The root scope is always the
/// bottom frame of the stack. For root processes, this is the root scope of
/// the program. For function calls, it's the scope of the entrypoint function.
#[derive(Debug)]
pub struct CallStack(Vec<Scope>);

impl CallStack {
    pub fn new(globals: Scope) -> Self {
        // This root frame can never be popped
        Self(vec![globals.child()])
    }

    fn push(&mut self, scope: Scope) {
        self.0.push(scope);
    }

    fn pop(&mut self) {
        if self.0.len() == 1 {
            todo!("not allowed!!")
        }
        self.0.pop();
    }

    /// Get the frame on top of the stack. As the stack can never be empty, this
    /// always returns something
    fn top(&self) -> &Scope {
        self.0.last().expect("TODO")
    }

    /// Get a mutable reference to the frame on top of the stack. As the stack
    /// can never be empty, this always returns something
    fn top_mut(&mut self) -> &mut Scope {
        self.0.last_mut().expect("TODO")
    }
}
