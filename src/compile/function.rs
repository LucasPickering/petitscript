//! Compiler steps related to functions

#[cfg(test)]
mod tests;

use crate::{
    ast::{
        source::Spanned,
        walk::{AstVisitor, Walk as _},
        Ast, Binding, Declaration, Expression, FunctionDefinition,
        FunctionPointer, Identifier, ObjectProperty, PropertyName, Variable,
    },
    error::RuntimeError,
};
use std::{collections::HashSet, hash::Hash, mem, sync::Arc};

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

/// For each function in the AST, determine which identifiers it uses in it body
/// that must be captured from the parent scope. This doesn't capture *values*,
/// just identifiers. The runtime will use this list to determine which values
/// to capture.
pub struct CaptureFunctions;

impl AstVisitor for CaptureFunctions {
    fn visit_function_definition(
        &mut self,
        definition: &mut FunctionDefinition,
    ) {
        // For each function definition, add its captures to the AST node
        let mut captured = CaptureFunction {
            captured: HashSet::new(),
            declared: HashSet::new(),
        };
        // Start on the body of the function. We can't let it walk the function
        // definition itself because that will trigger infinite mutual recursion
        // TODO handle param init expressions as well
        definition.body.walk(&mut captured);
        definition.captures = captured.captured.into_iter().collect();
    }
}

/// Helper to capture identifiers for a single function body. This should never
/// walk the full AST, just a single function body.
struct CaptureFunction {
    captured: HashSet<Identifier>,
    declared: HashSet<Identifier>,
}

impl CaptureFunction {
    /// If the identifier isn't in the declared list, capture it from parent
    /// scope
    fn add(&mut self, identifier: &Identifier) {
        if !self.declared.contains(identifier) {
            self.captured.insert(identifier.clone());
        }
    }
}

impl AstVisitor for CaptureFunction {
    // This is predicated on the AST walking in execution order, meaning an
    // identifier encountered before its declaration must be captured. E.g.:
    // ```
    // function f() {
    //   console.log(x);
    //   const x = 3;
    // }
    // ```

    fn visit_function_definition(
        &mut self,
        definition: &mut FunctionDefinition,
    ) {
        // Recursion! Of the mutual variety! If we encounter a nested function
        // definition, we need to figure out what it wants to capture as well
        definition.walk(&mut CaptureFunctions)
    }

    fn visit_declaration(&mut self, declaration: &mut Declaration) {
        // Add to the declaration list
        match declaration {
            Declaration::Lexical(declaration) => {}
            Declaration::Function(declaration) => todo!(),
        }
    }

    // Check all the ways an identifier can be used to reference a bound value.
    // We can't just use visit_identifier, because that's also called for
    // identifiers in declarations

    fn visit_expression(&mut self, expression: &mut Expression) {
        if let Expression::Identifier(identifier) = expression {
            self.add(identifier);
        }
    }

    fn visit_object_property(&mut self, property: &mut ObjectProperty) {
        if let ObjectProperty::Identifier(identifier) = property {
            self.add(identifier);
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
        id: FunctionDefinitionId,
    ) -> Result<&Arc<FunctionDefinition>, RuntimeError> {
        self.functions
            .get(id.0 as usize)
            .ok_or_else(|| RuntimeError::UnknownFunction(id))
    }
}

impl AstVisitor for FunctionTable {
    fn visit_function_pointer(&mut self, function: &mut FunctionPointer) {
        match function {
            FunctionPointer::Inline(definition) => {
                // Lift all children in the params/body of the function *before*
                // this function. Once we've lifted this one, we can't mutate it
                // anymore.
                definition.walk(self);

                // The ID is just the next index in the vec
                let id = FunctionDefinitionId(self.functions.len() as u32);
                let FunctionPointer::Inline(definition) =
                    mem::replace(function, FunctionPointer::Lifted(id))
                else {
                    unreachable!()
                };
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

#[cfg(test)]
impl From<u32> for FunctionDefinitionId {
    fn from(id: u32) -> Self {
        Self(id)
    }
}
