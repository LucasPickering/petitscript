#![forbid(unsafe_code)]
#![deny(clippy::all)]

mod error;
mod runtime;
mod value;

use boa_ast::scope::Scope;
use boa_interner::Interner;
use boa_parser::{Parser, Source};
use std::path::Path;

use crate::runtime::module::Module;
pub use crate::{
    error::{Error, Result},
    runtime::Runtime,
};

/// TODO
pub fn run(path: impl AsRef<Path>) -> Result<()> {
    load(path)?;
    Ok(())
}

/// TODO
pub fn load(path: impl AsRef<Path>) -> Result<Module> {
    let source = Source::from_filepath(path.as_ref())?;
    let mut parser = Parser::new(source);
    let scope = Scope::new_global();
    let mut interner = Interner::new();
    let source = parser.parse_module(&scope, &mut interner)?;

    let mut runtime = Runtime::new(interner, source);
    runtime.load()
}
