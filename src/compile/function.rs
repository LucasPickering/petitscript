//! Compiler steps related to functions

#[cfg(test)]
mod tests;

use crate::{
    ast::{
        AstVisitor, Binding, Block, Expression, FunctionDefinition,
        FunctionPointer, Identifier, ImportDeclaration, Module, Node,
        ObjectProperty, PropertyName, Variable, Walk as _,
    },
    error::RuntimeError,
};
use indexmap::IndexSet;
use std::{hash::Hash, mem, sync::Arc};

/// A convenience compiler step to apply a name to functions. The function name
/// is purely for printing/debugging, so this has no impact on semantics. This
/// covers these syntaxes:
///  - Function assignment: `const f = () => {};`
///  - Function as a default parameter value: `(f = () => {}) => {}`
///  - Function in an object: `{f: () => {}}`
///
/// Any context in which we can trivially determine a static name for the
/// function, we should do so.
pub struct LabelFunctions;

impl LabelFunctions {
    /// If the expression is a function, set is name to the given identifier
    fn set_name(identifier: &Node<Identifier>, expression: &mut Expression) {
        if let Expression::ArrowFunction(function) = expression {
            let FunctionPointer::Inline(definition) = function.data_mut()
            else {
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
    fn enter_variable(&mut self, variable: &mut Variable) {
        // Look for `const f = () => {}` or `(f = () => {}) => {}`
        // The second case is rare, but it's easier to support it than to not
        if let Variable {
            binding,
            init: Some(init),
        } = variable
        {
            if let Binding::Identifier(identifier) = &**binding {
                Self::set_name(identifier, init);
            }
        }
    }

    fn enter_object_property(&mut self, property: &mut ObjectProperty) {
        // Look for `{f: () => {}}`
        if let ObjectProperty::Property {
            property,
            expression,
        } = property
        {
            if let PropertyName::Literal(identifier) = &**property {
                Self::set_name(identifier, expression);
            }
        }
    }
}

/// For each function in the AST, determine which identifiers it uses in it body
/// that must be captured from the parent scope. This doesn't capture *values*,
/// just identifiers. The runtime will use this list to determine which values
/// to capture.
///
/// This works by walking through every function body and visiting every
/// variable reference. For each reference, check if it's available in the
/// function's local scope. If not, assume it's in the outer scope and capture
/// it. This doesn't actually verify if it *is* available in the outer scope;
/// that will be enforced at runtime, as all reference errors are runtime
/// errors.
#[derive(Debug)]
pub struct CaptureFunctions {
    /// TODO
    /// Invariant: Never empty
    scope_stack: Vec<IndexSet<Identifier>>,
    /// TODO
    /// Invariant: empty iff we are not inside a function definition
    function_stack: Vec<Frame>,
}

impl CaptureFunctions {
    pub fn new() -> Self {
        Self {
            scope_stack: vec![IndexSet::new()], // Always start with root scope
            function_stack: vec![],
        }
    }

    /// Add a declaration to the outermost scope
    fn declare(&mut self, identifier: Identifier) {
        let scope = self.scope_stack.last_mut().expect("Scope stack is empty");
        scope.insert(identifier);
    }

    /// Track a reference to an identifier. We'll decide if it's accesible in
    /// the current scope or needs to be captured
    fn refer(&mut self, identifier: Identifier) {
        // Find the innermost (last) scope that provides this name
        let Some(providing_scope_index) = self
            .scope_stack
            .iter()
            .rev()
            .position(|scope| scope.contains(&identifier))
            // Found index is from the *end* - we need to flip it around
            .map(|index| self.scope_stack.len() - 1 - index)
        else {
            // The name doesn't exist anywhere; let's assume it's in the global
            // scope and therefore doesn't need to be captured. Otherwise, this
            // reference will trigger an error at runtime
            return;
        };

        // When capturing a name, we can't just capture it in the innermost
        // function. Every function between its declaration and usage has to
        // capture it.
        self.function_stack
            .iter_mut()
            .rev()
            // If the providing scope is lower on the stack than the function's
            // outermost scope, we have to capture
            .filter(|frame| providing_scope_index < frame.scope_index)
            .for_each(|frame| {
                frame.captures.insert(identifier.clone());
            });
    }
}

impl AstVisitor for CaptureFunctions {
    fn enter_block(&mut self, _: &mut Block) {
        // A block defines a new lexical scope
        self.scope_stack.push(IndexSet::new());
    }

    fn exit_block(&mut self, _: &mut Block) {
        // Exit the current lexical scope. this can only come after a push in
        // the enter_ fn above
        self.scope_stack.pop().expect("Scope stack is empty");
    }

    /// Create a new function frame on the stack
    fn enter_function_definition(&mut self, _: &mut FunctionDefinition) {
        // A function definition defines a new function (obviously) as well as
        // a new scope. Create the scope first, then create a function frame
        // that points to that scope. The function body may be wrapped in a
        // Block, but we need a new scope here anyway to contain the parameters
        // as well.
        self.scope_stack.push(IndexSet::new());
        self.function_stack.push(Frame {
            captures: IndexSet::new(),
            scope_index: self.scope_stack.len() - 1,
        });
    }

    /// Close out the function frame and finalize the list of captures
    fn exit_function_definition(
        &mut self,
        definition: &mut FunctionDefinition,
    ) {
        // We're done with this function - pop it off the stack and set its
        // captures. This must be called after enter, therefore the stack can't
        // be empty.
        let frame = self.function_stack.pop().expect("Function stack is empty");
        definition.captures = frame.captures.into_iter().collect();
    }

    /// Declare a lexical variable
    fn enter_variable(&mut self, variable: &mut Variable) {
        // Lexical declaration could have any number of names
        match &*variable.binding {
            Binding::Identifier(identifier) => {
                self.declare(identifier.data().clone())
            }
            Binding::Object(_) => todo!(),
            Binding::Array(_) => todo!(),
        }
    }

    /// Declare names from an import
    fn enter_import(&mut self, import: &mut ImportDeclaration) {
        if let Some(default) = &import.default {
            self.declare(default.data().clone());
        }
        for named in &import.named {
            let name = named.rename.as_ref().unwrap_or(&named.identifier);
            self.declare(name.data().clone());
        }
    }

    // Check all the ways an identifier can be used to reference its value. We
    // can't just use visit_identifier, because that's also called for
    // identifiers in declarations

    fn enter_expression(&mut self, expression: &mut Expression) {
        if let Expression::Identifier(identifier) = expression {
            self.refer(identifier.data().clone());
        }
    }

    fn enter_object_property(&mut self, property: &mut ObjectProperty) {
        if let ObjectProperty::Identifier(identifier) = property {
            self.refer(identifier.data().clone());
        }
    }
}

/// TODO rename
#[derive(Debug)]
struct Frame {
    /// TODO
    captures: IndexSet<Identifier>,
    /// A pointer into the scope stack for the top-level scope within this
    /// function. The scope includes all functions further up (higher index in)
    /// the stack as well
    scope_index: usize,
}

/// A table of all user function definitions in a program. The definitions are
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
    pub fn lift(module: &mut Module) -> Self {
        let mut table = Self {
            functions: Vec::new(),
        };
        module.walk(&mut table);
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
            .ok_or_else(|| RuntimeError::UnknownUserFunction(id))
    }
}

impl AstVisitor for FunctionTable {
    fn exit_function_pointer(&mut self, function: &mut FunctionPointer) {
        // Lift functions on _exit_, so we can do it inside-out. For each
        // function, we know its params and body have already been lifted.

        // The ID is just the next index in the vec
        let id = FunctionDefinitionId(self.functions.len() as u32);
        let FunctionPointer::Inline(definition) =
            mem::replace(function, FunctionPointer::Lifted(id))
        else {
            // Compiler bug!
            panic!("Function {id:?} has already been lifted!")
        };
        self.functions.push(Arc::new(definition));
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
