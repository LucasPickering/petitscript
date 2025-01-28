mod eval;
mod exec;
pub mod module;
pub mod scope;
mod state;

use crate::{
    error::Result,
    runtime::{exec::Execute, module::Module, state::RuntimeState},
};
use boa_interner::Interner;

pub struct Runtime {
    interner: Interner,
    source: boa_ast::Module,
}

impl Runtime {
    /// TODO
    pub fn new(interner: Interner, source: boa_ast::Module) -> Runtime {
        Self { interner, source }
    }

    /// TODO
    pub fn load(&mut self) -> Result<Module> {
        let mut state = RuntimeState::new(&self.interner);
        self.source.items().items().exec(&mut state)?;
        state.into_module()
    }
}
