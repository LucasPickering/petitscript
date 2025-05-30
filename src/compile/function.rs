//! Compiler steps related to functions

use crate::ast::{
    AstVisitor, Binding, Block, Expression, FunctionDefinition, Identifier,
    ImportDeclaration, Node, ObjectProperty, PropertyName, Variable,
};
use indexmap::IndexSet;

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
        if let Expression::ArrowFunction(definition) = expression {
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

    /// Track a reference to an identifier. We'll decide if it's accesible in
    /// the current scope or needs to be captured
    fn visit_reference(&mut self, identifier: &mut Identifier) {
        // Find the innermost (last) scope that provides this name
        let Some(providing_scope_index) = self
            .scope_stack
            .iter()
            .rev()
            .position(|scope| scope.contains(identifier))
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{
            Declaration, FunctionBody, FunctionCall, FunctionParameter,
            IntoExpression, IntoNode, IntoStatement, Module, ObjectLiteral,
            Statement,
        },
        compile::{CapturedAst, Compiler, LabelledAst},
    };

    // We use vecs in a lot of args here in an attempt to cut down on
    // monomorphization copies for the builder methods

    /// Test that functions declared with an obvious associated name get
    /// labelled with that name.
    #[test]
    fn function_label() {
        let LabelledAst { module, .. } = Compiler::new(
            "
        const f = () => {};
        const o = {g: () => {}};
        // TODO make this work (param doesn't parse)
        //function param(h = () => {}) {}
        ",
        )
        .parse()
        .unwrap()
        .label()
        .program;
        let expected_statement_1 = Declaration::new(
            "f",
            FunctionDefinition::new(vec![], FunctionBody::empty())
                .with_name("f"),
        )
        .into();
        let expected_statement_2 = Declaration::new(
            "o",
            ObjectLiteral::new(vec![(
                "g",
                FunctionDefinition::new([], FunctionBody::empty())
                    .with_name("g")
                    .into_expr(),
            )]),
        )
        .into();

        assert_eq!(
            module.data(),
            &Module::new(vec![expected_statement_1, expected_statement_2])
        );
    }

    /// Test closure capturing
    #[test]
    fn function_capture() {
        let CapturedAst { module, .. } = Compiler::new(
            "
            import { add } from 'math';

            const log = (e) => {
                console.log(e);
            };

            const x = 2;
            const f = () => {
                const y = 3;
                return (z) => {
                    log(add(x, y, z))
                };
            };
            ",
        )
        .parse()
        .unwrap()
        .label()
        .capture()
        .program;

        assert_eq!(
            module.data(),
            &Module::new(vec![
                ImportDeclaration::native(
                    None::<Identifier>,
                    vec!["add"],
                    "math"
                )
                .into_stmt(),
                FunctionDefinition::new(
                    vec![FunctionParameter::identifier("e")],
                    FunctionBody::block(vec![Statement::Expression(
                        FunctionCall::new(
                            Expression::reference("console").property("log"),
                            vec![Expression::reference("e")]
                        )
                        .into_expr()
                        .s()
                    )])
                )
                .with_name("log")
                .declare("log")
                .into_stmt(),
                Declaration::new("x", 2).into_stmt(),
                FunctionDefinition::new(
                    vec![],
                    FunctionBody::block(vec![
                        Declaration::new("y", 3).into_stmt(),
                        FunctionDefinition::new(
                            vec![FunctionParameter::identifier("z")],
                            FunctionBody::block(vec![Statement::Expression(
                                FunctionCall::named(
                                    "log",
                                    vec![FunctionCall::named(
                                        "add",
                                        vec![
                                            Expression::reference("x"),
                                            Expression::reference("y"),
                                            Expression::reference("z"),
                                        ],
                                    )
                                    .into_expr()]
                                )
                                .into_expr()
                                .s()
                            )]),
                        )
                        // `console` isn't captured because it isn't declared
                        // in any parent scope
                        .with_captures(vec!["log", "add", "x", "y"])
                        .into_expr()
                        .return_()
                    ]),
                )
                .with_name("f")
                .with_captures(vec!["log", "add", "x"])
                .declare("f",)
                .into_stmt()
            ])
        );
    }
}
