//! TODO
//!
//! Collection types all use `Box<[T]>` instead of `Vec<T>` because we know
//! they're all fixed size and won't need to grow. `Box<[T]>` ensures we don't
//! allocate more memory than needed.

// TODO comments on everything

mod build;
mod display;
pub mod source;
mod walk;

pub use build::{IntoExpression, IntoStatement};
pub use walk::{AstVisitor, Walk};

use crate::{compile::FunctionDefinitionId, error::ModuleNameError};
use std::{
    hash::Hash,
    ops::{Deref, DerefMut},
    str::FromStr,
};

/// An identifier that uniquely identifies an AST node **within the AST**. These
/// IDs are not globally unique, just within a single AST.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct NodeId(u32);

impl NodeId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }
}

/// TODO
#[derive(Clone, Debug)]
pub struct Node<T> {
    id: NodeId,
    node: T,
}

impl<T> Node<T> {
    /// TODO
    pub(crate) fn new(id: NodeId, node: T) -> Self {
        Self { id, node }
    }

    pub(crate) fn id(&self) -> NodeId {
        self.id
    }

    /// Get a reference to the contained node value
    pub fn node(&self) -> &T {
        &self.node
    }

    /// Get a mutable reference to the contained node value
    pub fn node_mut(&mut self) -> &mut T {
        &mut self.node
    }
}

impl<T> Deref for Node<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

impl<T> DerefMut for Node<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.node
    }
}

#[cfg(test)]
impl<T: PartialEq> PartialEq for Node<T> {
    fn eq(&self, other: &Self) -> bool {
        // TODO explain
        self.node == other.node
    }
}

/// The root AST node for a single source file. This is the outcome of parsing a
/// file, and may contain nested modules from local imports.
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Module {
    pub statements: Box<[Node<Statement>]>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Statement {
    Empty,
    Block(Node<Block>),
    Expression(Node<Expression>),
    Declaration(Node<Declaration>),

    If(Node<If>),
    ForOfLoop(Node<ForOfLoop>),
    WhileLoop(Node<WhileLoop>),
    DoWhileLoop(Node<DoWhileLoop>),

    Return(Option<Node<Expression>>),
    Break,
    Continue,

    Import(Node<ImportDeclaration>),
    Export(Node<ExportDeclaration>),
    // TODO: switch, throw, try, catch, finally
}

/// A collection of statements, delineated by {}. This denotes a new
/// lexical scope.
/// TODO kill this and inline into the enum variant?
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Block {
    pub statements: Box<[Node<Statement>]>,
}

/// TODO eliminate this and just use lexical/fn decls directly?
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Declaration {
    Lexical(Node<LexicalDeclaration>),
    Function(Node<FunctionDeclaration>),
}

/// `const x = 3;` or `const x = 3, y = 4, z = 5;`
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct LexicalDeclaration {
    pub variables: Box<[Node<Variable>]>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Variable {
    pub binding: Node<Binding>,
    pub init: Option<Box<Node<Expression>>>,
}

/// A declaration of a function:
/// ```notrust
/// function f() {
///   // Body
/// }
/// ```
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FunctionDeclaration {
    pub name: Node<Identifier>,
    pub pointer: Node<FunctionPointer>,
}

/// A form of indirection for a function definition. Immediately after parsing,
/// this is the actual function definition. During lifting, the definition is
/// interned and replaced with an ID pointing to the definition.
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum FunctionPointer {
    /// Lifting hasn't been performed yet, the function definitions is still
    /// inline. This code isn't executable yet!
    Inline(FunctionDefinition),
    /// Function definition has been lifted to the top of the program, and this
    /// is just a pointer to the definition
    Lifted(FunctionDefinitionId),
}

/// The parameters, body, and captures that constitute a PetitScript (i.e *not*
/// native) function definition. This covers both `function` functions and
/// arrow functions, since the two are semantically equivalent in PS.
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FunctionDefinition {
    /// A label for this function, to be passed onto the function value. This
    /// is **not necessarily** the name the function is bound to; that is
    /// defined in the parent [FunctionDeclaration].
    pub name: Option<Node<Identifier>>,
    pub parameters: Box<[Node<FunctionParameter>]>,
    // `Spanned` not necessary because both variants of the body contain it
    pub body: FunctionBody,
    /// A list of all the identifiers this function captures from its parent
    /// scope. This only includes identifiers that are actually present in the
    /// parent scope. References in the function parameters/body that don't
    /// existing anywhere in scope can't be captured and therefore won't appear
    /// here. They will either be provided by the global scope or trigger a
    /// runtime error.
    pub captures: Box<[Identifier]>,
}

/// The body of a function
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum FunctionBody {
    /// A single-expression body of an arrow function: `(x) => x + 1`
    Expression(Box<Node<Expression>>),
    /// A standard function body. Accessible through either syntax
    Block(
        // TODO make sure we don't create an unnecessary scope for this block
        Node<Block>,
    ),
}

/// One parameter in a function definition
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FunctionParameter {
    pub variable: Node<Variable>,
    pub varargs: bool,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct If {
    pub condition: Node<Expression>,
    pub body: Box<Node<Statement>>,
    /// Optional else block. For `else if` blocks, this will be a nested `if`
    pub else_body: Option<Box<Node<Statement>>>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ForOfLoop {
    pub binding: Binding,
    pub iterable: Node<Expression>,
    pub body: Box<Node<Statement>>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct WhileLoop {
    pub condition: Node<Expression>,
    pub body: Box<Node<Statement>>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct DoWhileLoop {
    pub condition: Node<Expression>,
    pub body: Box<Node<Statement>>,
}

/// `import exportDefault, { export1, export2 as ex2 } from "module-name"`
/// No other import formats are supported
/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/import
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ImportDeclaration {
    /// `exportDefault`
    pub default: Option<Node<Identifier>>,
    /// `{ export1, export2 as ex2 }`
    pub named: Box<[Node<ImportNamed>]>,
    /// `"module-name"`
    pub module: Node<ImportModule>,
}

/// One identifier in a named import clause. In this import:
/// ```
/// import exportDefault, { export1, export2 as ex2 } from "module-name"
/// ```
///
/// the nameds are `export1` and `export2 as ex2`
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ImportNamed {
    /// `export1` or `export2`
    pub identifier: Node<Identifier>,
    /// `ex2`
    pub rename: Option<Node<Identifier>>,
}

/// Source for an imported module
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum ImportModule {
    /// A native module provided by the engine with a static name, like
    /// `import helpers from 'helpers'`
    Native(NativeModuleName),
    /// Another PetitScript file, e.g. `import helpers from './helpers'`. The
    /// imported file will be parsed during the parse of the parent, so we
    /// end up with a separate AST to be executed.
    Local(Module),
}

/// TODO move this somewhere
/// TODO explain naming rules
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct NativeModuleName(String);

impl TryFrom<String> for NativeModuleName {
    type Error = ModuleNameError;

    fn try_from(name: String) -> Result<Self, Self::Error> {
        fn is_valid(c: char) -> bool {
            c.is_alphanumeric() || ['-', '_'].contains(&c)
        }

        if !name.is_empty() && name.chars().all(is_valid) {
            Ok(Self(name))
        } else {
            Err(ModuleNameError { name })
        }
    }
}

impl FromStr for NativeModuleName {
    type Err = ModuleNameError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.to_owned().try_into()
    }
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum ExportDeclaration {
    Reexport {
        // TODO
    },
    Declaration(Node<Declaration>),
    // TODO do we need this variant? Can we merge it into DefaultExpression?
    DefaultFunctionDeclaration(Node<FunctionDeclaration>),
    DefaultExpression(Node<Expression>),
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Expression {
    Parenthesized(Box<Node<Expression>>),
    /// Primitive and complex type literals
    Literal(Node<Literal>),
    Template(Node<TemplateLiteral>),
    Identifier(Node<Identifier>),
    Call(Node<FunctionCall>),
    /// The static or dynamic property accessors: `.` or `[]`
    Property(Node<PropertyAccess>),
    /// Optional chaining operator: `?.`
    OptionalProperty(Node<OptionalPropertyAccess>),
    /// Lambda syntax: `(...) => {...}` or `() => value`. This shares the same
    /// AST node as the `function` syntax; they get combined during parsing
    ArrowFunction(Node<FunctionPointer>),
    Unary(Node<UnaryOperation>),
    Binary(Node<BinaryOperation>),
    Ternary(Node<TernaryConditional>),
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Identifier(String);

impl Identifier {
    /// TODO
    pub fn new(s: impl Into<String>) -> Self {
        Self(s.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// TODO document why no spans
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Literal {
    Null,
    Undefined,
    Boolean(bool),
    Float(f64),
    Int(i64),
    String(String),
    Array(Node<ArrayLiteral>),
    Object(Node<ObjectLiteral>),
}

/// TODO
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TemplateLiteral {
    /// A set of contiguous chunks that comprise the template. These will be
    /// alternating in variant, e.g. `[Lit, Expr, Lit]` or `[Expr, Lit, Expr]`
    pub chunks: Box<[Node<TemplateChunk>]>,
}

/// One piece in a template. Either a static string or an expression
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum TemplateChunk {
    /// This doesn't need a `Node` because we never refer to the literal chunks
    /// of a template on their own
    Literal(String),
    Expression(Node<Expression>),
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ArrayLiteral {
    pub elements: Box<[Node<ArrayElement>]>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum ArrayElement {
    Expression(Node<Expression>),
    Spread(Node<Expression>),
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ObjectLiteral {
    pub properties: Box<[Node<ObjectProperty>]>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum ObjectProperty {
    /// Normal key value: `{ key: value }` or `{ ["key"]: value }`
    Property {
        property: Node<PropertyName>,
        expression: Node<Expression>,
    },
    /// Identifier shorthand: `{ name }`
    Identifier(Node<Identifier>),
    /// Spread: `{ ...other }`
    Spread(Node<Expression>),
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FunctionCall {
    pub function: Box<Node<Expression>>,
    pub arguments: Box<[Node<Expression>]>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct PropertyAccess {
    pub expression: Box<Node<Expression>>,
    pub property: Node<PropertyName>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct OptionalPropertyAccess {
    pub expression: Box<Node<Expression>>,
    pub property: Node<PropertyName>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct UnaryOperation {
    pub operator: UnaryOperator,
    pub expression: Box<Node<Expression>>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum UnaryOperator {
    /// `!`: Boolean negation
    BooleanNot,
    /// `+`: Arithmetic plusification. This is essentially a cast to number
    Plus,
    /// `-`: Arithmetic negation
    Minus,
    /// `typeof`: Get the type of a value, as a string
    Typeof,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct BinaryOperation {
    pub operator: BinaryOperator,
    pub lhs: Box<Node<Expression>>,
    pub rhs: Box<Node<Expression>>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum BinaryOperator {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Mod,

    /// `&&`
    BooleanAnd,
    /// `||`
    BooleanOr,

    /// **Strict** equality: `===`
    Equal,
    /// **Strict** inequality: !==
    NotEqual,

    /// `<`
    LessThan,
    /// `>`
    GreaterThan,
    /// `<=`
    LessThanEqual,
    /// `>=`
    GreaterThanEqual,

    /// `??`
    NullishCoalesce,
    // TODO bitwise operations, exponent
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TernaryConditional {
    pub condition: Box<Node<Expression>>,
    pub true_expression: Box<Node<Expression>>,
    pub false_expression: Box<Node<Expression>>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum PropertyName {
    /// Normal key: `{ key: value }`
    Literal(Node<Identifier>),
    /// Computed key: `{ ["key"]: value }`
    Expression(Box<Node<Expression>>),
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Binding {
    /// `const x = 3`
    Identifier(Node<Identifier>),
    /// An object pattern: `const { a, b, c } = object`
    Object(Box<[Node<ObjectPatternElement>]>),
    /// An array pattern: `const [a, b, c] = array`
    Array(Box<[Node<ArrayPatternElement>]>),
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum ObjectPatternElement {
    /// `const { x } = object` or `const { x = 3 } = object`
    Identifier {
        identifier: Node<Identifier>,
        init: Option<Node<Expression>>,
    },
    /// `const { x: x2 } = object` or `const { x: x2 = 3 } = object`
    Mapped {
        key: Node<PropertyName>,
        value: Node<Binding>,
        init: Option<Node<Expression>>,
    },
    /// `const { ...x } = object` or `const { ...x = {} } = object`
    Rest {
        binding: Node<Binding>,
        init: Option<Node<Expression>>,
    },
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum ArrayPatternElement {
    // TODO
}
