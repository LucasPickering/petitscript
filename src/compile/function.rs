//! Compiler steps related to functions

use crate::{
    ast::{
        source::Spanned,
        walk::{AstVisitor, Walk as _},
        Ast, Binding, Expression, FunctionDefinition, FunctionPointer,
        Identifier, ObjectProperty, PropertyName, Variable,
    },
    error::RuntimeError,
    function::FunctionId,
};
use std::{hash::Hash, mem, sync::Arc};

/// A convenience compiler step to apply a name to functions that aren't
/// declared using the syntax of `function f() {}`. The function name is
/// purely for printing/debugging, so this has no impact on semantics. This
/// covers these syntaxes:
///  - Function assignment: `const f = () => {};`
///  - Function as a default parameter value: `function(f = () => {})`
///  - Function in an object: `{f: () => {}}`
///
/// Any context in which we can trivially determine a static name for the
/// function, we should do so.
pub struct LabelFunctions;

impl LabelFunctions {
    /// If the expression is a function, set is name to the given identifier
    fn set_name(identifier: &Spanned<Identifier>, expression: &mut Expression) {
        if let Expression::ArrowFunction(function) = expression {
            let FunctionPointer::Inline(definition) = &mut function.data else {
                // This must be run before function lifting, because definitions
                // are immutable once they're in the function table
                unreachable!("Function labelling must run before lifting")
            };
            // It shouldn't be possible for this function to have a name already
            debug_assert!(
                definition.name.is_none(),
                "Function already has a name before labelling"
            );
            definition.name = Some(identifier.clone())
        }
    }
}

impl AstVisitor for LabelFunctions {
    fn visit_variable(&mut self, variable: &mut Variable) {
        // Look for `const f = () => {}` or `function(f = () => {}) {}`
        // The second case is rare, but it's easier to support it than to not
        if let Variable {
            binding: Binding::Identifier(identifier),
            init: Some(init),
        } = variable
        {
            Self::set_name(identifier, init);
        }
    }

    fn visit_object_property(&mut self, property: &mut ObjectProperty) {
        // Look for `{f: () => {}}`
        if let ObjectProperty::Property {
            property:
                Spanned {
                    data: PropertyName::Literal(identifier),
                    ..
                },
            expression,
        } = property
        {
            Self::set_name(identifier, expression);
        }
    }
}

/// A table of all function definitions in a program. The definitions are
/// moved to a single data struct in a process known as "lifting". This enables
/// us to return function values to the user without having to copy or move the
/// function's entire parameter and body definition.
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
        id: FunctionId,
    ) -> Result<&Arc<FunctionDefinition>, RuntimeError> {
        self.functions
            .get(id.definition_id.0 as usize)
            .ok_or_else(|| RuntimeError::UnknownFunction(id))
    }
}

impl AstVisitor for FunctionTable {
    fn visit_function_pointer(&mut self, function: &mut FunctionPointer) {
        match function {
            FunctionPointer::Inline(_) => {
                // The ID is just the next index in the vec
                let id = FunctionDefinitionId(self.functions.len() as u32);
                let FunctionPointer::Inline(mut definition) =
                    mem::replace(function, FunctionPointer::Lifted(id))
                else {
                    unreachable!()
                };
                // We have to manually continue the walk into the function
                // definition, because once we move it into the table, it's
                // no longer part of the primary AST walk
                definition.walk(self);
                self.functions.push(Arc::new(definition.data));
            }
            FunctionPointer::Lifted(id) => {
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
