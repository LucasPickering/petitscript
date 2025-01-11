#![forbid(unsafe_code)]
#![deny(clippy::all)]

mod error;
mod runtime;
mod value;

use boa_ast::scope::Scope;
use boa_interner::Interner;
use boa_parser::{Parser, Source};
use std::path::Path;

pub use error::{Error, Result};

pub fn run(path: impl AsRef<Path>) -> Result<()> {
    let source = Source::from_filepath(path.as_ref())?;
    let mut parser = Parser::new(source);
    let scope = Scope::new_global();
    let mut interner = Interner::new();
    let script = parser.parse_script(&scope, &mut interner)?;
    dbg!(script);
    Ok(())
}
