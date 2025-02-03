//! Code execution and evaluation

mod eval;
mod exec;
pub mod scope;

use crate::{
    ast::Module,
    error::RuntimeResult,
    execute::{exec::Execute, scope::Scope},
    value::{Exports, Value},
};

/// TODO
#[derive(Debug)]
pub struct RuntimeState {
    /// The topmost scope in a script/module. Global scope is unique in a few
    /// ways:
    /// - If the scope stack is empty, this will still be available
    /// - Only global names can be exported
    global_scope: Scope,
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

impl RuntimeState {
    pub fn new() -> Self {
        Self {
            global_scope: Scope::global(),
            stack_frames: Vec::new(),
            export_default: None,
            export_names: Vec::new(),
        }
    }

    /// Execute a parsed module
    /// TODO combine with into_exports?
    pub async fn exec(&mut self, script: &Module) -> RuntimeResult<()> {
        script.statements.exec(self).await?;
        Ok(())
    }

    /// TODO
    pub fn into_exports(mut self) -> RuntimeResult<Exports> {
        // Only values in the global scope can be exported
        let scope = &self.global_scope;
        Ok(Exports {
            default: self.export_default.take(),
            named: self
                .export_names
                .drain(..)
                .map(|name| {
                    let value = scope.get(&name)?;
                    Ok((name, value))
                })
                .collect::<RuntimeResult<_>>()?,
        })
    }

    /// TODO
    fn scope(&self) -> &Scope {
        if let Some(last) = self.stack_frames.last() {
            last
        } else {
            &self.global_scope
        }
    }

    /// TODO
    fn scope_mut(&mut self) -> &mut Scope {
        if let Some(last) = self.stack_frames.last_mut() {
            last
        } else {
            &mut self.global_scope
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
