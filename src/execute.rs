//! Code execution and evaluation

mod eval;
mod exec;

use crate::{
    ast::Program,
    error::RuntimeResult,
    execute::exec::Execute,
    scope::Scope,
    value::{Exports, Value},
    Function,
};
use std::{any::Any, sync::Arc};

/// TODO
/// TODO rename
#[derive(Debug)]
pub struct Process {
    /// The program we'll be executing
    program: Program,
    state: ProcessState,
}

impl Process {
    /// TODO
    pub(crate) fn new(globals: Scope, program: Program) -> Self {
        Self {
            program,
            state: ProcessState {
                context: Arc::new(()),
                root_scope: globals.child(),
                stack_frames: Vec::new(),
                export_default: None,
                export_names: Vec::new(),
            },
        }
    }

    /// Execute the loaded program
    pub async fn execute(&mut self) -> RuntimeResult<()> {
        self.program.statements.exec(&mut self.state).await?;
        Ok(())
    }

    /// Call a function that originated from this process
    pub async fn call(
        &mut self,
        function: &Function,
        args: &[Value],
    ) -> RuntimeResult<Value> {
        function.call(&mut self.state, args).await
    }

    /// Get the values exported by the root module of this <TODO name here>
    pub fn exports(&self) -> Exports {
        // Only values in the root scope can be exported
        let scope = &self.state.root_scope;
        Exports {
            default: self.state.export_default.clone(),
            named: self
                .state
                .export_names
                .iter()
                .map(|name| {
                    let value = scope.get(name)?;
                    Ok((name.clone(), value))
                })
                .collect::<RuntimeResult<_>>()
                .expect("TODO"),
        }
    }
}

/// TODO
#[derive(Debug)]
pub struct ProcessState {
    /// Arbitrary data the user wants to attach to this process. This needs to
    /// Arc'd so the execution state can be cloned in order for processes to be
    /// forked.
    context: Arc<dyn Any>,
    /// The topmost scope in a script/module. Root scope is unique in a few
    /// ways:
    /// - If the scope stack is empty, this will still be available
    /// - Only root names can be exported
    ///
    /// This is *not* the same as the global scope. Global scope comes from
    /// outside this script (stdlib and user-provided values), and cannot be
    /// mutated.
    root_scope: Scope,
    /// The function call stack. In our machine, a stack frame is simply a
    /// scope of names. This should not be confused with the hierarchical
    /// parent/child structure of frames. Pushing a new frame is *not* the same
    /// as creating a subscope of the current scope.
    stack_frames: Vec<Scope>,
    /// TODO
    export_default: Option<Value>,
    /// TODO
    export_names: Vec<String>,
}

impl ProcessState {
    /// Get the process's attached used context, downcasted to a static type.
    /// Return an error if the context is of the wrong type.
    pub fn context<T: Any>(&self) -> RuntimeResult<&T> {
        self.context.downcast_ref().ok_or_else(|| todo!())
    }

    /// TODO
    fn scope(&self) -> &Scope {
        if let Some(last) = self.stack_frames.last() {
            last
        } else {
            &self.root_scope
        }
    }

    /// TODO
    fn scope_mut(&mut self) -> &mut Scope {
        if let Some(last) = self.stack_frames.last_mut() {
            last
        } else {
            &mut self.root_scope
        }
    }

    /// Execute a function within a new frame on the stack
    async fn with_frame<T>(
        &mut self,
        scope: Scope,
        f: impl AsyncFnOnce(&mut Self) -> T,
    ) -> T {
        self.stack_frames.push(scope);
        let value = f(self).await;
        self.stack_frames.pop();
        value
    }

    /// Execute a function in a new scope that's a child of the current scope.
    /// Use this for blocks such as ifs, loops, etc.
    async fn with_subscope<T>(
        &mut self,
        f: impl AsyncFnOnce(&mut Self) -> T,
    ) -> T {
        self.scope_mut().subscope();
        let value = f(self).await;
        self.scope_mut().revert();
        value
    }

    /// TODO
    fn export(&mut self, name: String) -> RuntimeResult<()> {
        // TODO error on duplicate export
        self.export_names.push(name);
        Ok(())
    }

    /// TODO
    fn export_default(&mut self, value: Value) -> RuntimeResult<()> {
        // TODO error if something is already exported
        self.export_default = Some(value);
        Ok(())
    }
}
