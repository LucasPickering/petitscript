//! Utilities for building ASTs programatically

use crate::ast::{
    source::Spanned, ArrayElement, ArrayLiteral, BinaryOperation,
    BinaryOperator, Binding, Block, Declaration, ExportDeclaration, Expression,
    FunctionBody, FunctionCall, FunctionDeclaration, FunctionDefinition,
    FunctionParameter, FunctionPointer, Identifier, ImportDeclaration,
    ImportModule, ImportNamed, IntoSpanned, LexicalDeclaration, Literal,
    Module, ObjectLiteral, ObjectProperty, PropertyAccess, PropertyName,
    Statement, TemplateLiteral, Variable,
};

// TODO remove Spanned everywhere
// TODO take impl IntoIter instead of vec

impl Module {
    pub fn new(statements: Vec<Spanned<Statement>>) -> Self {
        Self {
            statements: statements.into_iter().collect(),
        }
    }
}

impl Statement {
    /// Create a native import declaration: `import default, { named } from
    /// 'module'`
    pub fn import_native(
        default: Option<&'static str>,
        // TODO generic on item?
        named: impl IntoIterator<Item = &'static str>,
        module: &str,
    ) -> Self {
        Self::Import(
            ImportDeclaration {
                default: default.map(|name| Identifier::from(name).s()),
                named: named
                    .into_iter()
                    .map(|name| {
                        ImportNamed {
                            identifier: Identifier::from(name).s(),
                            rename: None,
                        }
                        .s()
                    })
                    .collect(),
                module: ImportModule::Native(module.parse().unwrap()).s(),
            }
            .s(),
        )
    }
}

impl From<Declaration> for Statement {
    fn from(declaration: Declaration) -> Self {
        Self::Declaration(declaration.s())
    }
}

impl From<FunctionDeclaration> for Statement {
    fn from(declaration: FunctionDeclaration) -> Self {
        Self::Declaration(Declaration::Function(declaration.s()).s())
    }
}

impl From<Expression> for Statement {
    fn from(expression: Expression) -> Self {
        Self::Expression(expression.s())
    }
}

impl Declaration {
    /// Create a lexical declaration statement: `const x = <expression>`
    pub fn lexical(name: &str, expression: Expression) -> Declaration {
        Self::Lexical(
            LexicalDeclaration {
                variables: [
                    Variable::identifier(name, Some(expression.s())).s()
                ]
                .into(),
            }
            .s(),
        )
    }

    /// Create a statement exporting this declaration: `export const x = ...` or
    /// `export function f() {...}`
    pub fn export(self) -> Statement {
        Statement::Export(ExportDeclaration::Declaration(self.s()).s())
    }
}

impl FunctionDeclaration {
    /// Create a function declaration: `const f = (a, b) => {`
    pub fn new(
        name: impl Into<Identifier>,
        parameters: impl IntoIterator<Item = FunctionParameter>,
        body: FunctionBody,
    ) -> Self {
        let identifier = name.into();
        Self {
            name: identifier.clone().s(),
            pointer: FunctionPointer::Inline(FunctionDefinition {
                name: Some(identifier.s()),
                parameters: parameters
                    .into_iter()
                    .map(IntoSpanned::s)
                    .collect::<Vec<_>>()
                    .into(),
                body,
                captures: Default::default(),
            })
            .s(),
        }
    }
}

impl FunctionDefinition {
    /// Define a new anonymous function
    pub fn new(
        parameters: impl IntoIterator<Item = FunctionParameter>,
        body: FunctionBody,
    ) -> Self {
        Self {
            name: None,
            parameters: parameters
                .into_iter()
                .map(IntoSpanned::s)
                .collect::<Vec<_>>()
                .into(),
            body,
            captures: Default::default(),
        }
    }

    /// Define what identifiers should be captured by this function. This is
    /// only needed in tests, for assertions. Users should never have to worry
    /// about captures themselves
    #[cfg(test)]
    pub(crate) fn with_captures(
        mut self,
        captures: impl IntoIterator<Item = &'static str>,
    ) -> Self {
        self.captures = captures.into_iter().map(Identifier::from).collect();
        self
    }
}

impl FunctionParameter {
    /// A simple single-identifier parameter, with no default expression
    pub fn identifier(name: &str) -> Self {
        Self {
            variable: Variable::identifier(name, None).s(),
            varargs: false,
        }
    }
}

impl FunctionBody {
    /// Create an expression function body, which evaluates a single expression
    /// and returns its value
    pub fn expression(expression: Expression) -> Self {
        Self::Expression(expression.s().into())
    }

    /// Create a block function body
    pub fn block(statements: impl IntoIterator<Item = Statement>) -> Self {
        Self::Block(
            Block {
                statements: statements
                    .into_iter()
                    .map(IntoSpanned::s)
                    .collect::<Vec<_>>()
                    .into(),
            }
            .s(),
        )
    }
}

impl FunctionCall {
    /// Call a function by name
    pub fn new(
        name: impl Into<Identifier>,
        arguments: impl IntoIterator<Item = Expression>,
    ) -> Self {
        Self {
            function: Expression::reference(name.into()).s().into(),
            arguments: arguments
                .into_iter()
                .map(IntoSpanned::s)
                .collect::<Vec<_>>()
                .into(),
        }
    }
}

impl Expression {
    /// Create a simple identifier reference expression: `x`
    pub fn reference(identifier: impl Into<Identifier>) -> Self {
        Self::Identifier(identifier.into().s())
    }

    /// Create an integer literal
    pub fn int(i: i64) -> Self {
        Self::Literal(Literal::Int(i).s())
    }

    pub fn binary(operator: BinaryOperator, lhs: Self, rhs: Self) -> Self {
        Self::Binary(
            BinaryOperation {
                operator,
                lhs: lhs.s().into(),
                rhs: rhs.s().into(),
            }
            .s(),
        )
    }
}

// TODO merge into other impl block
impl Spanned<Expression> {
    /// Get a property of this expression: `self.y`
    pub fn property(self, property: &str) -> Self {
        Expression::Property(
            PropertyAccess {
                expression: self.into(),
                property: PropertyName::new(property).s(),
            }
            .s(),
        )
        .s()
    }
}

impl<T> From<Option<T>> for Expression
where
    Expression: From<T>,
{
    /// Convert from an optional value to an expression. `None` will be
    /// converted to `undefined`
    fn from(value: Option<T>) -> Self {
        match value {
            Some(value) => value.into(),
            None => Self::Literal(Literal::Undefined.s()),
        }
    }
}

impl From<bool> for Expression {
    fn from(b: bool) -> Self {
        Self::Literal(Literal::Boolean(b).s())
    }
}

impl From<&str> for Expression {
    fn from(s: &str) -> Self {
        Self::Literal(Literal::String(s.to_owned()).s())
    }
}

impl From<String> for Expression {
    fn from(s: String) -> Self {
        Self::Literal(Literal::String(s).s())
    }
}

impl From<Literal> for Expression {
    fn from(literal: Literal) -> Self {
        Self::Literal(literal.s())
    }
}

impl From<TemplateLiteral> for Expression {
    fn from(template: TemplateLiteral) -> Self {
        Self::Template(template.s())
    }
}

impl From<ArrayLiteral> for Expression {
    fn from(literal: ArrayLiteral) -> Self {
        Self::Literal(Literal::Array(literal).s())
    }
}

impl From<ObjectLiteral> for Expression {
    fn from(literal: ObjectLiteral) -> Self {
        Self::Literal(Literal::Object(literal).s())
    }
}

impl From<FunctionDefinition> for Expression {
    fn from(function: FunctionDefinition) -> Self {
        Self::ArrowFunction(FunctionPointer::Inline(function).s())
    }
}

impl From<FunctionCall> for Expression {
    fn from(function_call: FunctionCall) -> Self {
        Self::Call(function_call.s())
    }
}

impl ArrayLiteral {
    pub fn new(iter: impl IntoIterator<Item = Expression>) -> Self {
        Self {
            elements: iter
                .into_iter()
                .map(|expression| ArrayElement::Expression(expression.s()).s())
                .collect::<Vec<_>>()
                .into(),
        }
    }
}

impl ObjectLiteral {
    /// Create a new object literal from a sequence of key-value pairs
    pub fn new<P>(iter: impl IntoIterator<Item = (P, Expression)>) -> Self
    where
        P: ToString,
    {
        Self {
            properties: iter
                .into_iter()
                .map(|(property, expression)| {
                    ObjectProperty::Property {
                        property: PropertyName::new(property).s(),
                        expression: expression.s(),
                    }
                    .s()
                })
                .collect::<Vec<_>>()
                .into(),
        }
    }

    /// Create a new object literal from a sequence of key-value pairs, where
    /// the values are optional. Any `None` value will be filtered out.
    pub fn filtered<P>(
        iter: impl IntoIterator<Item = (P, Option<Expression>)>,
    ) -> Self
    where
        P: ToString,
    {
        Self {
            properties: iter
                .into_iter()
                .filter_map(|(property, expression)| {
                    Some(
                        ObjectProperty::Property {
                            property: PropertyName::new(property).s(),
                            expression: expression?.s(),
                        }
                        .s(),
                    )
                })
                .collect::<Vec<_>>()
                .into(),
        }
    }
}

impl PropertyName {
    /// Create a property name from a string. This will use a literal property
    /// name (`x.y`) if possible. If the name isn't a valid identifier, use
    /// the expression syntax instead (`x["not-identifier"]`).
    pub fn new(name: impl ToString) -> Self {
        // TODO if name isn't a valid identifier, use a dynamic property
        let name = name.to_string();
        if name.contains('-') {
            // This is a hack; make it more generic based on the identifier
            // parser
            Self::Expression(Expression::from(name).s().into())
        } else {
            Self::Literal(Identifier::new(name.to_string()).s())
        }
    }
}

impl Variable {
    /// Create a simple identifier variable
    pub fn identifier(name: &str, init: Option<Spanned<Expression>>) -> Self {
        Self {
            binding: Binding::Identifier(Identifier::new(name).s()),
            init: init.map(Box::new),
        }
    }
}

impl From<&'static str> for Identifier {
    /// Generate an identifier from a static string. This panics if the string
    /// is not a valid identifier. The `'static` restriction aims to mitigate
    /// the risk of this panic by restricting it to string literals.
    fn from(s: &'static str) -> Self {
        Self::new(s)
    }
}
