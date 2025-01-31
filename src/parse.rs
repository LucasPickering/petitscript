//! Parser-combinator go brrrrr

use crate::{ast::Script, Result, Source};

/// Parse source code into an Abstract Syntax Tree
pub fn parse(source: impl Source) -> Result<Script> {
    let name = source.name();
    let code = source.text()?;
    todo!()
}
