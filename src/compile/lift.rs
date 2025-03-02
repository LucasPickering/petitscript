use crate::{
    ast::{
        walk::{AstVisitor, Walk as _},
        Ast, FunctionDefinition, FunctionPointer,
    },
    compile::ProgramId,
    error::RuntimeError,
    function::Function,
};
use std::{
    collections::HashMap,
    hash::Hash,
    mem,
    sync::{
        atomic::{AtomicU64, Ordering},
        Arc,
    },
};

/// TODO
#[derive(Debug)]
pub struct FunctionTable {
    program_id: ProgramId,
    functions: HashMap<FunctionId, Arc<FunctionDefinition>>,
}

impl FunctionTable {
    /// Lifting is the process of moving all function definitions in the AST
    /// into a single registry, and replacing the original definitions with
    /// IDs into the registry. This is akin to moving all functions into the
    /// .text section of a binary and generating pointers.
    pub fn lift(program_id: ProgramId, ast: &mut Ast) -> Self {
        let mut table = Self {
            program_id,
            functions: HashMap::new(),
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
        self.functions.get(&function.id()).ok_or_else(|| {
            RuntimeError::UnknownFunction {
                function: function.clone(),
            }
        })
    }
}

impl AstVisitor for FunctionTable {
    fn visit_function_pointer(&mut self, function: &mut FunctionPointer) {
        match function {
            FunctionPointer::Inline(definition) => {
                let id = FunctionId::new(self.program_id);
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
                self.functions.insert(id, Arc::new(definition.data));
            }
            FunctionPointer::Lifted { id, .. } => {
                // Compiler bug!
                panic!("Function {id:?} has already been lifted!")
            }
        }
    }
}

/// A unique identifier for a function definition. This ID provides the
/// following guarantees:
/// - Globally unique, so two identical functions in different programs will
///   _not_ share the same ID
/// - Composed of primitives so it can be serialized and deserialized, and still
///   reference the original function
/// - Stable across processes of the same program. This allows you to serialize
///   a function ID, then recreate its process later and still use the ID to
///   call the function, provided the process's program hasn't changed at all
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct FunctionId {
    /// A globally unique ID for the process in which this function originated.
    /// It can only be invoked in this process
    program_id: ProgramId,
    /// An ID for this function, unique only within the scope of its process
    function_id: u64,
}

impl FunctionId {
    /// TODO
    fn new(program_id: ProgramId) -> Self {
        static NEXT_ID: AtomicU64 = AtomicU64::new(0);
        // TODO is this the correct ordering?
        let function_id = NEXT_ID.fetch_add(1, Ordering::SeqCst);

        Self {
            program_id,
            function_id,
        }
    }

    /// Get the ID of the program from which this function originated
    pub(crate) fn program_id(&self) -> ProgramId {
        self.program_id
    }
}
