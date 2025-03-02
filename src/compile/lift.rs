use crate::{
    ast::{
        walk::{AstVisitor, Walk as _},
        Ast, FunctionDefinition, FunctionPointer,
    },
    error::RuntimeError,
    function::Function,
};
use std::{hash::Hash, mem, sync::Arc};

/// TODO
#[derive(Debug)]
pub struct FunctionTable {
    functions: Vec<Arc<FunctionDefinition>>,
}

impl FunctionTable {
    /// Lifting is the process of moving all function definitions in the AST
    /// into a single registry, and replacing the original definitions with
    /// IDs into the registry. This is akin to moving all functions into the
    /// .text section of a binary and generating pointers.
    pub fn lift(ast: &mut Ast) -> Self {
        let mut table = Self {
            functions: Vec::new(),
        };
        ast.walk(&mut table);
        table
    }

    /// Look up a function definition by its ID. This is analagous to
    /// derefencing a function pointer into the .text section. This returns
    /// an `Arc` so the lifetime can be detached from the program if necessary
    pub fn get(
        &self,
        function: &Function,
    ) -> Result<&Arc<FunctionDefinition>, RuntimeError> {
        // At this point someone higher up should've verified this function
        // came from this program, so an error here indicates a bug somewhere
        self.functions
            .get(function.id().definition_id.0 as usize)
            .ok_or_else(|| RuntimeError::UnknownFunction {
                function: function.clone(),
            })
    }
}

impl AstVisitor for FunctionTable {
    fn visit_function_pointer(&mut self, function: &mut FunctionPointer) {
        match function {
            FunctionPointer::Inline(definition) => {
                // The ID is just the next index in the vec
                let id = FunctionDefinitionId(self.functions.len() as u32);
                let name = definition.name.clone();
                let FunctionPointer::Inline(mut definition) = mem::replace(
                    function,
                    FunctionPointer::Lifted { id, name },
                ) else {
                    unreachable!()
                };
                // We have to manually continue the walk into the function
                // definition, because once we move it into the table, it's
                // no longer part of the primary AST walk
                definition.walk(self);
                self.functions.push(Arc::new(definition.data));
            }
            FunctionPointer::Lifted { id, .. } => {
                // Compiler bug!
                panic!("Function {id:?} has already been lifted!")
            }
        }
    }
}

/// A unique identifier for a function definition. This ID is unique only within
/// its originating program. The ID simply represents an index into the vec of
/// function definitions.
///
/// Use a u32 internally so this can be packed with a process ID into a single
/// word
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct FunctionDefinitionId(pub u32);
