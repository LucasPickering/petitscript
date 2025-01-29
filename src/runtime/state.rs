use crate::{
    error::Result,
    runtime::{module::Module, scope::Scope},
    value::Value,
};
use boa_interner::{Interner, Sym};
use std::sync::Arc;

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
    /// TODO
    resolver: SymbolResolver,
}

impl RuntimeState {
    pub fn new(resolver: SymbolResolver) -> Self {
        Self {
            global_scope: Scope::global(),
            stack_frames: Vec::new(),
            export_default: None,
            export_names: Vec::new(),
            resolver,
        }
    }

    /// TODO
    pub fn scope(&self) -> &Scope {
        if let Some(last) = self.stack_frames.last() {
            last
        } else {
            &self.global_scope
        }
    }

    /// TODO
    pub fn scope_mut(&mut self) -> &mut Scope {
        if let Some(last) = self.stack_frames.last_mut() {
            last
        } else {
            &mut self.global_scope
        }
    }

    /// Execute a function within a new frame on the stack
    pub fn with_frame<T>(
        &mut self,
        scope: Scope,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.stack_frames.push(scope);
        let value = f(self);
        self.stack_frames.pop();
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

    /// TODO
    pub fn export(&mut self, name: String) -> Result<()> {
        // TODO error on duplicate export
        self.export_names.push(name);
        Ok(())
    }

    /// TODO
    pub fn export_default(&mut self, value: Value) -> Result<()> {
        // TODO error if something is already exported
        self.export_default = Some(value);
        Ok(())
    }

    /// Get an owned copy of the symbol resolver, so we can detach from the
    /// state's lifetime
    pub fn resolver(&self) -> SymbolResolver {
        self.resolver.clone()
    }

    /// TODO
    pub fn into_module(mut self) -> Result<Module> {
        // Only values in the global scope can be exported
        let scope = &self.global_scope;
        Ok(Module {
            default: self.export_default.take(),
            named: self
                .export_names
                .drain(..)
                .map(|name| {
                    let value = scope.get(&name)?;
                    Ok((name, value))
                })
                .collect::<Result<_>>()?,
        })
    }
}

/// TODO remove this once we're off of boa
#[derive(Clone, Debug)]
pub struct SymbolResolver {
    interner: Arc<Interner>,
}

impl SymbolResolver {
    pub fn new(interner: Interner) -> Self {
        Self {
            interner: interner.into(),
        }
    }

    /// Resolve a Boa interner symbol into the corresponding string
    pub fn resolve(&self, symbol: Sym) -> &str {
        // This should only fail if the ID isn't valid UTF-8, or a bug
        // somewhere. It's not worth propagating the potential error everywhere,
        // since we plan to get rid of this eventually when replacing boa with
        // our own parser.
        self.interner.resolve_expect(symbol).utf8().expect("TODO")
    }
}
