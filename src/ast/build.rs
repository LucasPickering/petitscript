//! Utilities for building ASTs programatically. TODO add more detail. When do
//! we use associated methods vs combinators?

use crate::ast::{
    ArrayElement, ArrayLiteral, BinaryOperation, BinaryOperator, Binding,
    Block, Declaration, ExportDeclaration, Expression, FunctionBody,
    FunctionCall, FunctionDefinition, FunctionParameter, Identifier,
    ImportDeclaration, ImportModule, ImportNamed, Literal, Module, Node,
    NodeId, ObjectLiteral, ObjectProperty, PropertyAccess, PropertyName,
    Statement, TemplateChunk, TemplateLiteral, Variable,
};

impl Module {
    pub fn new(statements: impl IntoIterator<Item = Statement>) -> Self {
        Self {
            statements: statements.into_iter().map(IntoNode::s).collect(),
        }
    }
}

impl From<Declaration> for Statement {
    fn from(declaration: Declaration) -> Self {
        Self::Declaration(declaration.s())
    }
}

impl From<Expression> for Statement {
    fn from(expression: Expression) -> Self {
        Self::Expression(expression.s())
    }
}

impl From<ImportDeclaration> for Statement {
    fn from(import: ImportDeclaration) -> Self {
        Self::Import(import.s())
    }
}

impl ImportDeclaration {
    /// Create a import declaration from a native module:
    /// `import default, { named } from 'module'`
    pub fn native(
        default: Option<impl Into<Identifier>>,
        named: impl IntoIterator<Item = impl Into<Identifier>>,
        module: &str,
    ) -> Self {
        Self {
            default: default.map(|name| name.into().s()),
            named: named
                .into_iter()
                .map(|name| {
                    ImportNamed {
                        identifier: name.into().s(),
                        rename: None,
                    }
                    .s()
                })
                .collect(),
            module: ImportModule::Native(module.parse().unwrap()).s(),
        }
    }
}

impl Declaration {
    /// Create a lexical declaration statement: `const x = <expression>`
    pub fn new(
        name: impl Into<Identifier>,
        expression: impl Into<Expression>,
    ) -> Self {
        Self {
            variables:
                [Variable::identifier(name, Some(expression.into())).s()].into(),
        }
    }

    /// Create a statement exporting this declaration: `export const x = ...` or
    /// `export function f() {...}`
    pub fn export(self) -> Statement {
        Statement::Export(ExportDeclaration::Declaration(self.s()).s())
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
                .map(IntoNode::s)
                .collect::<Vec<_>>()
                .into(),
            body,
            captures: Default::default(),
        }
    }

    /// Declare this function with the given name. The name will *not* be
    /// attached to the function definition, for explicitness/simplicity. There
    /// are some test cases where the attached name causes havok, so it's easier
    /// to leave it out and users can attach it manually with [Self::with_name].
    pub fn declare(self, name: impl Into<Identifier>) -> Declaration {
        let identifier = name.into();
        Declaration::new(identifier, self)
    }

    /// Attach a name to this function definition. This does not affect code
    /// generation at all and is only helpful if the AST is being compiled or
    /// used for test assertions.
    pub fn with_name(mut self, name: impl Into<Identifier>) -> Self {
        self.name = Some(name.into().s());
        self
    }

    /// Define what identifiers should be captured by this function. This does
    /// not affect code generation at all and is only helpful if the AST is
    /// being compiled or used for test assertions.
    pub fn with_captures(
        mut self,
        captures: impl IntoIterator<Item = &'static str>,
    ) -> Self {
        self.captures = captures.into_iter().map(Identifier::from).collect();
        self
    }
}

impl FunctionParameter {
    /// A simple single-identifier parameter, with no default expression
    pub fn identifier(name: impl Into<Identifier>) -> Self {
        Self {
            variable: Variable::identifier(name, None).s(),
            varargs: false,
        }
    }
}

impl FunctionBody {
    /// Create an expression function body, which evaluates a single expression
    /// and returns its value
    pub fn expression(expression: impl Into<Expression>) -> Self {
        Self::Expression(expression.into().s().into())
    }

    /// Create a block function body
    pub fn block(statements: impl IntoIterator<Item = Statement>) -> Self {
        Self::Block(
            Block {
                statements: statements
                    .into_iter()
                    .map(IntoNode::s)
                    .collect::<Vec<_>>()
                    .into(),
            }
            .s(),
        )
    }

    /// Create a block function body with no statements
    pub fn empty() -> Self {
        Self::Block(
            Block {
                statements: Box::new([]),
            }
            .s(),
        )
    }
}

impl FunctionCall {
    /// Call an expression as a function
    pub fn new(
        function: impl Into<Expression>,
        arguments: impl IntoIterator<Item = Expression>,
    ) -> Self {
        Self {
            function: function.into().s().into(),
            arguments: arguments
                .into_iter()
                .map(IntoNode::s)
                .collect::<Vec<_>>()
                .into(),
        }
    }

    /// Call a function by name
    pub fn named(
        name: impl Into<Identifier>,
        arguments: impl IntoIterator<Item = Expression>,
    ) -> Self {
        Self::new(Expression::reference(name.into()), arguments)
    }
}

impl Expression {
    /// Create a simple identifier reference expression: `x`
    pub fn reference(identifier: impl Into<Identifier>) -> Self {
        Self::Identifier(identifier.into().s())
    }

    /// Create a binary operation expression
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

    /// Get a property of this expression: `self.y`
    pub fn property(self, name: &str) -> Self {
        Self::Property(
            PropertyAccess {
                expression: self.s().into(),
                property: PropertyName::new(name).s(),
            }
            .s(),
        )
    }

    /// Create a `return` statement that returns this expression
    pub fn return_(self) -> Statement {
        Statement::Return(Some(self.s()))
    }

    /// Create an expression that will call a method from this expression
    /// resolved value's prototype. `x` -> `x.f(...)`
    pub fn call(
        self,
        name: &str,
        arguments: impl IntoIterator<Item = Expression>,
    ) -> Self {
        FunctionCall::new(self.property(name), arguments).into()
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

/// Create an integer literal
impl From<i64> for Expression {
    fn from(i: i64) -> Self {
        Self::Literal(Literal::Int(i).s())
    }
}

/// Create a float literal
impl From<f64> for Expression {
    fn from(f: f64) -> Self {
        Self::Literal(Literal::Float(f).s())
    }
}

/// Create a string literal
impl From<&str> for Expression {
    fn from(s: &str) -> Self {
        Self::Literal(Literal::String(s.to_owned()).s())
    }
}

/// Create a string literal
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
        Self::Literal(Literal::Array(literal.s()).s())
    }
}

impl From<ObjectLiteral> for Expression {
    fn from(literal: ObjectLiteral) -> Self {
        Self::Literal(Literal::Object(literal.s()).s())
    }
}

impl From<FunctionDefinition> for Expression {
    fn from(definition: FunctionDefinition) -> Self {
        Self::ArrowFunction(definition.s())
    }
}

impl From<FunctionCall> for Expression {
    fn from(function_call: FunctionCall) -> Self {
        Self::Call(function_call.s())
    }
}

impl From<&str> for Literal {
    fn from(value: &str) -> Self {
        Self::String(value.into())
    }
}

impl From<String> for Literal {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl TemplateLiteral {
    /// Create a new template literal from chunks
    pub fn new(chunks: impl IntoIterator<Item = TemplateChunk>) -> Self {
        Self {
            chunks: chunks
                .into_iter()
                .map(TemplateChunk::s)
                .collect::<Vec<_>>()
                .into(),
        }
    }
}

impl TemplateChunk {
    /// Create a template chunk from an expression
    pub fn expression(expression: impl Into<Expression>) -> Self {
        Self::Expression(expression.into().s())
    }
}

impl From<Expression> for TemplateChunk {
    fn from(expression: Expression) -> Self {
        Self::expression(expression)
    }
}

impl From<&str> for TemplateChunk {
    fn from(value: &str) -> Self {
        TemplateChunk::Literal(value.to_owned())
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
        P: Into<PropertyName>,
    {
        Self {
            properties: iter
                .into_iter()
                .map(|(name, expression)| {
                    ObjectProperty::Property {
                        property: name.into().s(),
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
        P: Into<String>,
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
    pub fn new(name: impl Into<String>) -> Self {
        let name = name.into();
        // If name is a valid identifier, then it can be a literal property
        // name. Otherwise, it has to be an expression.
        //
        // parse() would clone, so we use try_from()
        match Identifier::try_from(name) {
            Ok(identifier) => Self::Literal(identifier.s()),
            Err(error) => {
                Self::Expression(Expression::from(error.identifier).s().into())
            }
        }
    }
}

impl From<Expression> for PropertyName {
    /// Create a property string from an arbitrary expression. If the expression
    /// is a string literal containing a valid identifier, create a literal
    /// property name. Otherwise create an expression name.
    fn from(expression: Expression) -> Self {
        if let Expression::Literal(literal) = &expression {
            if let Literal::String(name) = literal.data() {
                // TODO remove implicit clone
                return Self::new(name);
            }
        }
        Self::Expression(expression.s().into())
    }
}

impl From<&str> for PropertyName {
    fn from(name: &str) -> Self {
        PropertyName::new(name)
    }
}

impl From<String> for PropertyName {
    fn from(name: String) -> Self {
        PropertyName::new(name)
    }
}

impl Variable {
    /// Create a simple identifier variable
    pub fn identifier(
        name: impl Into<Identifier>,
        init: Option<Expression>,
    ) -> Self {
        Self {
            binding: Binding::Identifier(name.into().s()).s(),
            init: init.map(|init| init.s().into()),
        }
    }
}

impl From<&'static str> for Identifier {
    /// Generate an identifier from a static string. This panics if the string
    /// is not a valid identifier. The `'static` restriction aims to mitigate
    /// the risk of this panic by restricting it to string literals.
    fn from(s: &'static str) -> Self {
        s.parse().unwrap()
    }
}

/// TODO remove this one we get rid of Spanned
pub trait IntoStatement {
    fn into_stmt(self) -> Statement;
}

impl<T> IntoStatement for T
where
    Statement: From<T>,
{
    fn into_stmt(self) -> Statement {
        self.into()
    }
}

/// TODO remove this one we get rid of Spanned
pub trait IntoExpression {
    fn into_expr(self) -> Expression;
}

impl<T> IntoExpression for T
where
    Expression: From<T>,
{
    fn into_expr(self) -> Expression {
        self.into()
    }
}

/// TODO
pub trait IntoNode: Sized {
    fn s(self) -> Node<Self> {
        Node::new(NodeId::new(0), self)
    }
}

impl<T> IntoNode for T {}
