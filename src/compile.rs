//! The compile pipeline turns source code into an executable program.

mod lift;
mod parse;

pub use lift::FunctionId;

use crate::{
    ast::{Ast, FunctionDefinition},
    compile::lift::FunctionRegistry,
    error::RuntimeError,
    function::Function,
    Error, Source,
};
use std::{
    hash::{DefaultHasher, Hash, Hasher},
    sync::Arc,
};

/// Compile source code into an executable [Program]
pub fn compile(source: impl Source) -> Result<Program, Error> {
    let mut ast = parse::parse(source)?;
    let id = compute_id(&ast);
    let functions = FunctionRegistry::lift(id, &mut ast);
    Ok(Program { id, ast, functions })
}

/// An executable program. This is produced by [compile]
#[derive(Debug)]
pub struct Program {
    id: ProgramId,
    ast: Ast,
    functions: FunctionRegistry,
}

impl Program {
    /// TODO
    pub fn id(&self) -> ProgramId {
        self.id
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
        self.functions.get(function)
    }
}

/// A unique-ish ID for a program, which is derived from its content. This is
/// stable, such that the same source code parsed by the same version of PetitJS
/// will always give the same program ID, irrespective of whitespace.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct ProgramId(u64);

/// Calculate a program's ID, based on its AST
fn compute_id(ast: &Ast) -> ProgramId {
    let mut hasher = DefaultHasher::new();
    ast.hash(&mut hasher);
    ProgramId(hasher.finish())
}
