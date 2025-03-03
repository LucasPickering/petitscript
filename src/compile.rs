//! The compile pipeline turns source code into an executable program.

mod lift;
mod parse;

pub use lift::FunctionDefinitionId;

use crate::{
    ast::{
        source::Spanned,
        walk::{AstVisitor, Walk},
        Ast, Binding, Expression, FunctionDefinition, FunctionPointer,
        Identifier, ObjectProperty, PropertyName, Variable,
    },
    compile::lift::FunctionTable,
    error::RuntimeError,
    function::Function,
    Error, Source,
};
use std::sync::Arc;

/// Compile source code into an executable [Program]
pub fn compile(source: impl Source) -> Result<Program, Error> {
    let mut ast = parse::parse(&source)?;
    ast.walk(&mut FunctionLabel); // Apply function labelling
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

/// This is a convenience compiler step to apply a name to functions that aren't
/// declared using the syntax of `function f() {}`. The function name is
/// purely for printing/debugging, so this has no impact on semantics. This
/// covers these syntaxes:
///  - Function assignment: `const f = () => {};`
///  - Function as a default parameter value: `function(f = () => {})`
///  - Function in an object: `{f: () => {}}`
///
/// Any context in which we can trivially determine a static name for the
/// function, we should do so.
struct FunctionLabel;

impl FunctionLabel {
    /// If the expression is a function, set is name to the given identifier
    fn set_name(identifier: &Spanned<Identifier>, expression: &mut Expression) {
        if let Expression::ArrowFunction(function) = expression {
            let FunctionPointer::Inline(definition) = &mut function.data else {
                // This has to be run before function lifting, because lifting
                // duplicates the names so it becomes harder to label
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

impl AstVisitor for FunctionLabel {
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
