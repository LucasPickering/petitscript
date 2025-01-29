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

/// TODO
#[derive(Debug, Default)]
pub struct Runtime {}

impl Runtime {
    /// TODO
    pub fn new() -> Self {
        Self {}
    }

    /// TODO
    pub fn load(&self, path: impl AsRef<Path>) -> Result<Module> {
        let (source, mut state) = self.parse(path)?;
        source.items().items().exec(&mut state)?;
        state.into_module()
    }

    /// Parse the file at the given parse as a module. Return a runtime
    /// that can be used to execute the code.
    fn parse(
        &self,
        path: impl AsRef<Path>,
    ) -> Result<(boa_ast::Module, RuntimeState)> {
        let source = boa_parser::Source::from_filepath(path.as_ref())?;
        let mut parser = boa_parser::Parser::new(source);
        let scope = boa_ast::scope::Scope::new_global();
        let mut interner = Interner::new();
        let source = parser.parse_module(&scope, &mut interner)?;
        Ok((source, RuntimeState::new(SymbolResolver::new(interner))))
    }
}
