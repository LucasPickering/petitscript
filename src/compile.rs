//! The compile pipeline turns source code into an executable program.

mod lift;
mod parse;

pub use lift::FunctionDefinitionId;

use crate::{
    ast::{Ast, FunctionDefinition},
    compile::lift::FunctionTable,
    error::RuntimeError,
    function::Function,
    Error, Source,
};
use std::sync::Arc;

/// Compile source code into an executable [Program]
pub fn compile(source: impl Source) -> Result<Program, Error> {
    let mut ast = parse::parse(&source)?;
    let function_table = FunctionTable::lift(&mut ast);
    Ok(Program {
        source: Box::new(source),
        ast,
        function_table,
    })
}

/// An executable program. This is produced by [compile]
#[derive(Debug)]
pub struct Program {
    source: Box<dyn Source>,
    ast: Ast,
    function_table: FunctionTable,
}

impl Program {
    /// TODO
    pub fn source(&self) -> &dyn Source {
        &*self.source
    }

    /// TODO
    pub fn ast(&self) -> &Ast {
        &self.ast
    }

    /// Look up a function definition by its ID. This is analagous to
    /// derefencing a function pointer into the .text section
    pub fn get_function_definition(
        &self,
        function: &Function,
    ) -> Result<&Arc<FunctionDefinition>, RuntimeError> {
        self.function_table.get(function)
    }
}
