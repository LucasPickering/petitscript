//! Utilities for building ASTs programatically
//!
//! See example in the parent module.
//!
//! ## Functions vs Methods
//!
//! Anything that creates a new AST node uses an associated function (e.g.
//! [`Declaration::new`]). Combinator methods are only used to modify a node
//! after creation (e.g. [`FunctionParameter::with_init`]).
//!
//! TODO that's not actually true, but it's aspirational ^

use crate::ast::{
    Argument, ArrayElementKind, ArrayLiteral, ArrayPatternElementKind,
    BinaryOperation, BinaryOperator, Binding, BindingKind, Block, Declaration,
    ExportDeclaration, ExportDeclarationKind, Expression, ExpressionKind,
    FunctionBody, FunctionCall, FunctionDefinition, FunctionParameter,
    Identifier, IdentifierNode, ImportDeclaration, ImportModuleKind,
    ImportNamed, KindedNode, Literal, Module, Node, NodeId, ObjectLiteral,
    ObjectPatternElement, ObjectPropertyKind, OptionalPropertyAccess,
    PropertyAccess, PropertyName, PropertyNameKind, Statement, StatementKind,
    TemplateChunk, TemplateChunkKind, TemplateLiteral, Variable,
};
use std::cell::Cell;

/// Generate a new [`NodeId`]
///
/// This uses a thread local to keep track of the next available ID. That means
/// each node ID will be unique not just for the AST, but for any AST generated
/// in that thread. This does mean that identical ASTs generated sequentially on
/// the same thread will *not* have consistent IDs.
///
/// To ignore IDs on equality checks, see [`NodeId::with_equal_ids`].
fn next_id() -> NodeId {
    thread_local! {
        static NEXT_NODE_ID: Cell<u32> = const { Cell::new(0) };
    }

    let id = NEXT_NODE_ID.get();
    NEXT_NODE_ID.set(id + 1);
    NodeId(id)
}

impl Block {
    /// Create a new block of statements
    pub fn new(
        statements: impl IntoIterator<Item = impl IntoStatement>,
    ) -> Self {
        Self {
            id: next_id(),
            statements: statements
                .into_iter()
                .map(IntoStatement::into_stmt)
                .collect(),
        }
    }
}

impl Module {
    /// Construct a new module with a series of statements
    pub fn new(
        statements: impl IntoIterator<Item = impl IntoStatement>,
    ) -> Self {
        Self {
            id: next_id(),
            statements: statements
                .into_iter()
                .map(IntoStatement::into_stmt)
                .collect(),
        }
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
            id: next_id(),
            default: default.map(|name| name.into().into_node()),
            named: named
                .into_iter()
                .map(|name| {
                    let identifier: IdentifierNode = name.into().into_node();
                    ImportNamed {
                        id: next_id(),
                        imported: identifier.clone(),
                        local: identifier,
                    }
                })
                .collect(),
            module: ImportModuleKind::Native(module.parse().unwrap())
                .into_node(),
        }
    }
}

impl Declaration {
    /// Create a lexical declaration statement: `const x = <expression>`
    pub fn new(
        name: impl Into<Identifier>,
        expression: impl IntoExpression,
    ) -> Self {
        Self {
            id: next_id(),
            variables: [Variable::identifier(name, Some(expression))].into(),
        }
    }

    /// Create a statement exporting this declaration: `export const x = ...` or
    /// `export function f() {...}`
    pub fn export(self) -> Statement {
        StatementKind::Export(
            ExportDeclarationKind::Declaration(self).into_node(),
        )
        .into_node()
    }
}

impl FunctionDefinition {
    /// Define a new function with a block body:
    /// `(...parameters) => {...statements}`
    pub fn block(
        parameters: impl IntoIterator<Item = FunctionParameter>,
        statements: impl IntoIterator<Item = impl IntoStatement>,
    ) -> Self {
        Self {
            id: next_id(),
            name: None,
            parameters: parameters.into_iter().collect(),
            body: FunctionBody::Block(Block::new(statements)),
            captures: Default::default(),
        }
    }

    /// Define a new function with an empty body: `(...parameters) => {}`
    pub fn empty(
        parameters: impl IntoIterator<Item = FunctionParameter>,
    ) -> Self {
        Self {
            id: next_id(),
            name: None,
            parameters: parameters.into_iter().collect(),
            body: FunctionBody::Block(Block::new([(); 0])),
            captures: Default::default(),
        }
    }

    /// Define a new function with an expression body:
    /// `(...parameters) => expression`
    ///
    /// The function will evaluate the expression and return its output value.
    pub fn expression(
        parameters: impl IntoIterator<Item = FunctionParameter>,
        expression: impl IntoExpression,
    ) -> Self {
        Self {
            id: next_id(),
            name: None,
            parameters: parameters.into_iter().collect(),
            body: FunctionBody::Expression(Box::new(expression.into_expr())),
            captures: Default::default(),
        }
    }

    /// Attach a name to this function definition.
    ///
    /// This does **not affect code generation at all** and is only helpful if
    /// the AST is being used for test assertions. Normally this is done
    /// automatically during the "label" step of the compiler.
    #[must_use]
    #[cfg(test)]
    pub fn with_name(mut self, name: impl Into<Identifier>) -> Self {
        self.name = Some(name.into());
        self
    }

    /// Define what identifiers should be captured by this function
    ///
    /// This does not affect code generation at all and is only helpful if the
    /// AST is being used for test assertions. Normally this is done
    /// automatically during the "capture" step of the compiler.
    #[must_use]
    #[cfg(test)]
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
            id: next_id(),
            variable: Variable::identifier(name, None::<()>),
            varargs: false,
        }
    }

    /// Add an `= init` expression to this parameter
    #[must_use]
    pub fn with_init(mut self, init: impl IntoExpression) -> Self {
        self.variable.init = Some(Box::new(init.into_expr()));
        self
    }
}

impl FunctionCall {
    /// Call an expression as a function: `(function)(...arguments)`
    pub fn new(
        function: impl IntoExpression,
        arguments: impl IntoIterator<Item = impl IntoExpression>,
    ) -> Self {
        Self {
            id: next_id(),
            function: Box::new(function.into_expr()),
            arguments: arguments
                .into_iter()
                .map(|argument| {
                    Argument::Expression(Box::new(argument.into_expr()))
                })
                .collect(),
            optional: false,
        }
    }

    /// Call a function by name: `name(...arguments)`
    pub fn named(
        name: impl Into<Identifier>,
        arguments: impl IntoIterator<Item = impl IntoExpression>,
    ) -> Self {
        let function = name.into().into_node();
        Self::new(function, arguments)
    }
}

impl Expression {
    /// Create a simple identifier reference expression: `x`
    pub fn reference(identifier: impl Into<Identifier>) -> Self {
        ExpressionKind::Identifier(identifier.into().into_node()).into_node()
    }

    /// Create a binary operation expression
    pub fn binary(
        operator: BinaryOperator,
        lhs: impl IntoExpression,
        rhs: impl IntoExpression,
    ) -> Self {
        ExpressionKind::Binary(BinaryOperation {
            id: next_id(),
            operator,
            lhs: Box::new(lhs.into_expr()),
            rhs: Box::new(rhs.into_expr()),
        })
        .into_node()
    }

    /// Get a property of this expression: `self.y`
    #[must_use]
    pub fn property(self, name: &str) -> Self {
        PropertyAccess {
            id: next_id(),
            object: self.into(),
            property: PropertyName::identifier(name),
        }
        .into_expr()
    }

    /// Create a `return` statement that returns this expression
    pub fn return_(self) -> Statement {
        StatementKind::Return(Some(self)).into_node()
    }

    /// Create an expression that will call a method from this expression
    /// resolved value's prototype. `x` -> `x.f(...)`
    #[must_use]
    pub fn call(
        self,
        name: &str,
        arguments: impl IntoIterator<Item = impl IntoExpression>,
    ) -> Self {
        ExpressionKind::Call(FunctionCall::new(self.property(name), arguments))
            .into_node()
    }

    /// Create an `export default` statement exporting this expression
    pub fn export_default(self) -> Statement {
        ExportDeclarationKind::DefaultExpression(self)
            .into_node()
            .into_stmt()
    }
}

impl<T> From<Option<T>> for ExpressionKind
where
    ExpressionKind: From<T>,
{
    /// Convert from an optional value to an expression. `None` will be
    /// converted to `undefined`
    fn from(value: Option<T>) -> Self {
        match value {
            Some(value) => value.into(),
            None => ExpressionKind::Literal(Literal::Undefined),
        }
    }
}

impl From<()> for ExpressionKind {
    fn from((): ()) -> Self {
        ExpressionKind::Literal(Literal::Undefined)
    }
}

impl From<bool> for ExpressionKind {
    fn from(b: bool) -> Self {
        ExpressionKind::Literal(Literal::Boolean(b))
    }
}

/// Create an integer literal
impl From<i64> for ExpressionKind {
    fn from(i: i64) -> Self {
        ExpressionKind::Literal(Literal::Int(i))
    }
}

/// Create a float literal
impl From<f64> for ExpressionKind {
    fn from(f: f64) -> Self {
        ExpressionKind::Literal(Literal::Float(f))
    }
}

/// Create a string literal
impl From<&str> for ExpressionKind {
    fn from(s: &str) -> Self {
        ExpressionKind::Literal(Literal::String(s.to_owned()))
    }
}

/// Create a string literal
impl From<String> for ExpressionKind {
    fn from(s: String) -> Self {
        ExpressionKind::Literal(Literal::String(s))
    }
}

impl From<Literal> for ExpressionKind {
    fn from(literal: Literal) -> Self {
        ExpressionKind::Literal(literal)
    }
}

impl From<TemplateLiteral> for ExpressionKind {
    fn from(template: TemplateLiteral) -> Self {
        ExpressionKind::Template(template)
    }
}

impl From<ArrayLiteral> for ExpressionKind {
    fn from(literal: ArrayLiteral) -> Self {
        ExpressionKind::Literal(Literal::Array(literal))
    }
}

impl From<ObjectLiteral> for ExpressionKind {
    fn from(literal: ObjectLiteral) -> Self {
        ExpressionKind::Literal(Literal::Object(literal))
    }
}

impl From<FunctionDefinition> for ExpressionKind {
    fn from(definition: FunctionDefinition) -> Self {
        ExpressionKind::Function(definition)
    }
}

impl From<FunctionCall> for ExpressionKind {
    fn from(function_call: FunctionCall) -> Self {
        ExpressionKind::Call(function_call)
    }
}

impl From<IdentifierNode> for ExpressionKind {
    fn from(identifier: IdentifierNode) -> Self {
        ExpressionKind::Identifier(identifier)
    }
}

impl From<PropertyAccess> for ExpressionKind {
    fn from(property: PropertyAccess) -> Self {
        ExpressionKind::Property(property)
    }
}

impl From<OptionalPropertyAccess> for ExpressionKind {
    fn from(property: OptionalPropertyAccess) -> Self {
        ExpressionKind::OptionalProperty(property)
    }
}

impl TemplateLiteral {
    /// Create a new template literal from chunks
    pub fn new(chunks: impl IntoIterator<Item = TemplateChunk>) -> Self {
        Self {
            id: next_id(),
            chunks: chunks.into_iter().collect(),
        }
    }
}

impl TemplateChunk {
    /// Create a template chunk from an expression
    pub fn expression(expression: impl IntoExpression) -> Self {
        TemplateChunkKind::Expression(expression.into_expr()).into_node()
    }
}

impl From<Expression> for TemplateChunk {
    fn from(expression: Expression) -> Self {
        Self::expression(expression)
    }
}

impl From<&str> for TemplateChunk {
    fn from(value: &str) -> Self {
        TemplateChunkKind::Literal(value.to_owned()).into_node()
    }
}

impl ArrayLiteral {
    pub fn new(iter: impl IntoIterator<Item = impl IntoExpression>) -> Self {
        Self {
            id: next_id(),
            elements: iter
                .into_iter()
                .map(|expression| {
                    ArrayElementKind::Expression(expression.into_expr())
                        .into_node()
                })
                .collect(),
        }
    }
}

impl ObjectLiteral {
    /// Create a new object literal from a sequence of key-value pairs
    pub fn new<P>(
        iter: impl IntoIterator<Item = (P, impl IntoExpression)>,
    ) -> Self
    where
        P: Into<PropertyName>,
    {
        Self {
            id: next_id(),
            properties: iter
                .into_iter()
                .map(|(name, expression)| {
                    ObjectPropertyKind::Property {
                        property: name.into(),
                        expression: expression.into_expr(),
                    }
                    .into_node()
                })
                .collect(),
        }
    }

    /// Create a new object literal from a sequence of key-value pairs, where
    /// the values are optional
    ///
    /// Any `None` value will be filtered out.
    pub fn filtered<P>(
        iter: impl IntoIterator<Item = (P, Option<Expression>)>,
    ) -> Self
    where
        P: Into<String>,
    {
        Self {
            id: next_id(),
            properties: iter
                .into_iter()
                .filter_map(|(property, expression)| {
                    Some(
                        ObjectPropertyKind::Property {
                            property: PropertyName::identifier(property),
                            expression: expression?,
                        }
                        .into_node(),
                    )
                })
                .collect(),
        }
    }
}

impl PropertyName {
    /// Create a property string from an arbitrary expression
    ///
    /// If the expression is a string literal containing a valid identifier,
    /// create a literal property name. Otherwise create an expression name.
    pub fn new(expression: impl IntoExpression) -> Self {
        let expression = expression.into_expr();
        match expression.kind {
            ExpressionKind::Literal(Literal::String(name)) => {
                Self::identifier(name)
            }
            _ => PropertyNameKind::Expression(Box::new(expression)).into_node(),
        }
    }

    /// Create a property name from a string
    ///
    /// This will use a literal property name (`x.y`) if possible. If the name
    /// isn't a valid identifier, use the expression syntax instead
    /// (`x["not-identifier"]`).
    pub fn identifier(name: impl Into<String>) -> Self {
        let name = name.into();
        // If name is a valid identifier, then it can be a literal property
        // name. Otherwise, it has to be an expression.
        //
        // parse() would clone, so we use try_from()
        match Identifier::try_from(name) {
            Ok(identifier) => {
                PropertyNameKind::Identifier(identifier.into_node())
            }
            Err(error) => PropertyNameKind::Expression(Box::new(
                error.identifier.into_expr(),
            )),
        }
        .into_node()
    }
}

impl From<&str> for PropertyName {
    fn from(name: &str) -> Self {
        PropertyName::identifier(name)
    }
}

impl From<String> for PropertyName {
    fn from(name: String) -> Self {
        PropertyName::identifier(name)
    }
}

impl Variable {
    /// Create a simple identifier variable
    pub fn identifier(
        name: impl Into<Identifier>,
        init: Option<impl IntoExpression>,
    ) -> Self {
        Self {
            id: next_id(),
            binding: Binding::identifier(name),
            init: init.map(|expression| Box::new(expression.into_expr())),
        }
    }
}

impl Binding {
    /// Create a simple identifier binding
    pub fn identifier(name: impl Into<Identifier>) -> Self {
        BindingKind::Identifier(name.into().into_node()).into_node()
    }

    /// Create an object binding
    ///
    /// `properties` is a list of `(key, binding, init)` tuples. `rest` is the
    /// optional `...` element at the end of the pattern.
    pub fn object(
        properties: impl IntoIterator<
            Item = (
                impl Into<PropertyName>,
                impl Into<Binding>,
                Option<impl IntoExpression>,
            ),
        >,
        rest: Option<impl Into<Binding>>,
    ) -> Self {
        BindingKind::Object {
            properties: properties
                .into_iter()
                .map(|(key, binding, init)| ObjectPatternElement {
                    id: next_id(),
                    key: key.into(),
                    binding: binding.into(),
                    init: init.map(IntoExpression::into_expr),
                })
                .collect(),
            rest: rest.map(|binding| Box::new(binding.into())),
        }
        .into_node()
    }

    /// Create an array binding
    ///
    /// `elements` is a list of `(binding, init)` pairs. Each element can be
    /// `None`, which will map to [`ArrayPatternElementKind::Skip`]. `rest` is
    /// the optional `...` element at the end of the pattern.
    pub fn array(
        elements: impl IntoIterator<
            Item = Option<(impl Into<Binding>, Option<impl IntoExpression>)>,
        >,
        rest: Option<impl Into<Binding>>,
    ) -> Self {
        BindingKind::Array {
            elements: elements
                .into_iter()
                .map(|option| {
                    if let Some((binding, init)) = option {
                        ArrayPatternElementKind::Identifier {
                            binding: binding.into(),
                            init: init.map(IntoExpression::into_expr),
                        }
                    } else {
                        ArrayPatternElementKind::Skip
                    }
                    .into_node()
                })
                .collect(),
            rest: rest.map(|binding| Box::new(binding.into())),
        }
        .into_node()
    }
}

/// Create an identifier binding from a string literal
impl From<&'static str> for Binding {
    fn from(name: &'static str) -> Self {
        Self::identifier(name)
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

/// Convert an AST value into an [`Expression`] node
///
/// This is needed instead of just [`From`] impls because it will generate at
/// least one node ID, meaning it's not a simple 1:1 stable conversion. Having
/// it as a separate trait also makes it easier to make blanket impls.
pub trait IntoExpression {
    /// Convert the value to [`ExpressionKind`], then wrap as an [`Expression`]
    fn into_expr(self) -> Expression;
}

impl IntoExpression for Expression {
    fn into_expr(self) -> Expression {
        self
    }
}

impl<T> IntoExpression for T
where
    ExpressionKind: From<T>,
{
    fn into_expr(self) -> Expression {
        Expression {
            id: next_id(),
            kind: self.into(),
        }
    }
}

impl IntoExpression for Identifier {
    fn into_expr(self) -> Expression {
        self.into_node().into_expr()
    }
}

/// Convert an AST value into a [`Statement`] node
///
/// This is needed instead of just [`From`] impls because it will generate at
/// least one node ID, meaning it's not a simple 1:1 stable conversion. Having
/// it as a separate trait also makes it easier to make blanket impls.
pub trait IntoStatement {
    /// Convert the value to [`StatementKind`], then wrap as an [`Statement`]
    fn into_stmt(self) -> Statement;
}

impl IntoStatement for Statement {
    fn into_stmt(self) -> Statement {
        self
    }
}

impl<T> IntoStatement for T
where
    StatementKind: From<T>,
{
    fn into_stmt(self) -> Statement {
        Statement {
            id: next_id(),
            kind: self.into(),
        }
    }
}

/// Convert an AST enum value into its [`Node`] wrapper
pub trait IntoNode {
    type Node: Node;

    /// Convert an AST enum value into its [`Node`] wrapper
    ///
    /// This will generate an ID for the wrapping node.
    fn into_node(self) -> Self::Node;
}

impl<T: Node> IntoNode for T {
    type Node = T;

    fn into_node(self) -> Self::Node {
        self
    }
}

impl IntoNode for Identifier {
    type Node = IdentifierNode;

    fn into_node(self) -> Self::Node {
        IdentifierNode {
            id: next_id(),
            identifier: self,
        }
    }
}

/// Implement [`IntoNode`] for [`KindedNode`] type aliases
macro_rules! impl_into_node {
    ($($type:ty)*) => {
        $(
            impl IntoNode for $type {
                type Node = KindedNode<$type>;

                fn into_node(self) -> Self::Node {
                    KindedNode {
                        id: next_id(),
                        kind: self,
                    }
                }
            }
        )*
    }
}

impl_into_node! {
    ArrayElementKind
    ArrayPatternElementKind
    BindingKind
    ExportDeclarationKind
    ExpressionKind
    ImportModuleKind
    ObjectPropertyKind
    PropertyNameKind
    StatementKind
    TemplateChunkKind
}

impl From<bool> for Literal {
    fn from(value: bool) -> Self {
        Self::Boolean(value)
    }
}

impl From<i64> for Literal {
    fn from(value: i64) -> Self {
        Self::Int(value)
    }
}

impl From<f64> for Literal {
    fn from(value: f64) -> Self {
        Self::Float(value)
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

impl From<Declaration> for StatementKind {
    fn from(declaration: Declaration) -> Self {
        StatementKind::Declaration(declaration)
    }
}

impl From<ExportDeclaration> for StatementKind {
    fn from(export: ExportDeclaration) -> Self {
        StatementKind::Export(export)
    }
}

impl From<ImportDeclaration> for StatementKind {
    fn from(import: ImportDeclaration) -> Self {
        StatementKind::Import(import)
    }
}

impl<T: IntoExpression> From<T> for StatementKind {
    fn from(expression: T) -> StatementKind {
        StatementKind::Expression(expression.into_expr())
    }
}
