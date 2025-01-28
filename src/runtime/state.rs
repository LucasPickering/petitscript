use crate::{
    error::Result,
    runtime::{module::Module, scope::Scope},
    value::Value,
};
use boa_interner::{Interner, Sym};

/// TODO
#[derive(Debug)]
pub struct RuntimeState<'int> {
    /// The topmost scope in a script/module. Global scope is unique in a few
    /// ways:
    /// - If the scope stack is empty, this will still be available
    /// - Only global names can be exported
    global_scope: Scope,
    /// TODO
    /// TODO use stack frames instead?
    scope_stack: Vec<Scope>,
    /// TODO
    export_default: Option<Value>,
    /// TODO
    export_names: Vec<String>,
    /// TODO
    interner: &'int Interner,
}

impl<'int> RuntimeState<'int> {
    pub fn new(interner: &'int Interner) -> Self {
        Self {
            global_scope: Scope::new(),
            scope_stack: Vec::new(),
            export_default: None,
            export_names: Vec::new(),
            interner,
        }
    }

    /// TODO
    pub fn scope(&self) -> &Scope {
        if let Some(last) = self.scope_stack.last() {
            last
        } else {
            &self.global_scope
        }
    }

    /// TODO
    pub fn scope_mut(&mut self) -> &mut Scope {
        if let Some(last) = self.scope_stack.last_mut() {
            last
        } else {
            &mut self.global_scope
        }
    }

    /// TODO
    pub fn push_scope(&mut self, scope: Scope) {
        self.scope_stack.push(scope);
    }

    /// TODO
    pub fn pop_scope(&mut self) {
        self.scope_stack.pop();
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

    /// Resolve a Boa interner symbol into the corresponding string
    pub fn resolve_sym(&self, symbol: Sym) -> &str {
        // This should only fail if the ID isn't valid UTF-8, or a bug
        // somewhere. It's not worth propagating the potential error everywhere,
        // since we plan to get rid of this eventually when replacing boa with
        // our own parser.
        self.interner.resolve_expect(symbol).utf8().expect("TODO")
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
