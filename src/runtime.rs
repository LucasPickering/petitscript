mod eval;
mod exec;
pub mod module;
pub mod scope;
mod state;

use crate::{
    error::Result,
    runtime::{
        exec::Execute,
        module::Module,
        state::{RuntimeState, SymbolResolver},
    },
};
use boa_interner::Interner;
use std::path::Path;

pub struct Runtime {
    resolver: SymbolResolver,
    source: boa_ast::Module,
}

impl Runtime {
    /// Parse the file at the given parse as a PetitJS module. Return a runtime
    /// that can be used to execute the code.
    pub fn parse(path: impl AsRef<Path>) -> Result<Self> {
        let source = boa_parser::Source::from_filepath(path.as_ref())?;
        let mut parser = boa_parser::Parser::new(source);
        let scope = boa_ast::scope::Scope::new_global();
        let mut interner = Interner::new();
        let source = parser.parse_module(&scope, &mut interner)?;
        Ok(Self {
            resolver: SymbolResolver::new(interner),
            source,
        })
    }

    /// TODO
    pub(crate) fn new(interner: Interner, source: boa_ast::Module) -> Runtime {
        Self {
            resolver: SymbolResolver::new(interner),
            source,
        }
    }

    /// TODO
    pub fn load(&mut self) -> Result<Module> {
        let mut state = RuntimeState::new(self.resolver.clone());
        self.source.items().items().exec(&mut state)?;
        state.into_module()
    }
}
