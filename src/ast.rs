//! The [Abstract Syntax Tree](https://w.wiki/6jCi) types of PetitScript
//!
//! These define all the possible components of a valid PetitScript program.
//! Because PetitScript is implemented as a tree-walking interpreter, these
//! types also compose into executable PS programs.
//!
//! ## Collection Types
//!
//! Collection types all use `Box<[T]>` instead of `Vec<T>` because we know
//! they're all fixed size and won't need to grow. `Box<[T]>` ensures we don't
//! allocate more memory than needed.

mod build;
mod display;
mod walk;

pub use build::{IntoExpression, IntoNode, IntoStatement};
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

/// A wrapper type for an AST node. This wrapper holds a unique ID for each
/// node, which uniquely identifies that node within the AST. These IDs are used
/// to track metadata, such as source spans, for each node. The IDs are
/// currently not exposed; the wrapper is intended to be as transparent as
/// possible.
///
/// ## Why?
///
/// There are a lot of ways to do this and they all have tradeoffs. Some
/// considered alternatives:
///
/// - Store the metadata inline in the AST. Replace each ID with the actual
///   metadata. This eliminates the indirection of having a separate metadata
///   table, but restricts you to a single metadata type.
/// - Generic ineline metadata. The same solution as above, but add a generic
///   param to each node type to define what metadata to store. This adds a lot
///   of clutter and requires mapping between metadata types for cases like
///   comparing parsed ASTs to generated ones in tests.
/// - Store the ID directly in each node type instead of using a wrapper type.
///   This declutters a lot of usage cases, but requires the repetition of
///   adding each node field, and also adds complexity around enums. It forces
///   you to either add the ID field to every variant of every enum, or
///   normalize enums so that each variant contains a single unique struct. Both
///   end up adding more clutter than they save.
/// - Use the memory address of each node as its implicit ID. This subverts the
///   protection of the borrow checker and completely breaks as soon as you move
///   an AST node (which is very common during construction and parsing).
/// - Store AST nodes in a bump-allocated arena to ensure addresses are stable.
///   This adds a lot of complexity and is challenging to then store the AST and
///   the encapsulating arena together because they are self-referential.
/// - Use structural keys, such as a traversal path pointing to each node in the
///   tree. This is a recipe for disaster though, because any changes to the
///   tree's structure during compilation will completely destroy the keys.
///
/// Overall, storing metadata separately adds flexibility to cover the two main
/// use cases:
/// - ASTs parsed from source code
/// - ASTs generated programatically
#[derive(Clone, Debug)]
pub struct Node<T> {
    id: NodeId,
    node: T,
}

impl<T> Node<T> {
    pub(crate) fn new(id: NodeId, node: T) -> Self {
        Self { id, node }
    }

    pub(crate) fn id(&self) -> NodeId {
        self.id
    }

    /// Get a reference to the contained node value
    pub fn data(&self) -> &T {
        &self.node
    }

    /// Get a mutable reference to the contained node value
    pub fn data_mut(&mut self) -> &mut T {
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

/// Compare equality of node *contents* only. Comparing AST nodes is typically
/// only useful for tests. Skipping the ID in the comparison makes it much
/// easier to compare parsed ASTs to generated ones because the IDs will likely
/// be allocated in a different order.
impl<T: PartialEq> PartialEq for Node<T> {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node
    }
}

/// The root AST node for a single source file. This is the outcome of parsing a
/// file, and may contain nested modules from local imports.
#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub statements: Box<[Node<Statement>]>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Empty,
    Block(Node<Block>),
    Expression(Node<Expression>),
    /// `const` declaration
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
}

/// A collection of statements, delineated by `{ }`. This declares a new lexical
/// scope.
#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub statements: Box<[Node<Statement>]>,
}

/// A single `const` declaration with one or more declared variables: `const x =
/// 3;` or `const x = 3, y = 4, z = 5;`
#[derive(Clone, Debug, PartialEq)]
pub struct Declaration {
    pub variables: Box<[Node<Variable>]>,
}

/// A single declared variable in a declaration or function parameter
#[derive(Clone, Debug, PartialEq)]
pub struct Variable {
    pub binding: Node<Binding>,
    pub init: Option<Box<Node<Expression>>>,
}

/// A form of indirection for a function definition. Immediately after parsing,
/// this is the actual function definition. During compilation, a step called
/// "lifting" moves each function definition into a table and replaces it with
/// a unique ID. At runtime, the ID is used to look up and invoke the
/// definition.
#[derive(Clone, Debug, PartialEq)]
pub enum FunctionPointer {
    /// Lifting hasn't been performed yet, the function definitions is still
    /// inline. This code isn't executable yet!
    Inline(FunctionDefinition),
    /// Function definition has been lifted to the function table, and this is
    /// just a pointer to the definition
    Lifted(FunctionDefinitionId),
}

/// The parameters, body, and captures that constitute a user (i.e *not* native)
/// function definition. This covers both `function` functions and
/// arrow functions, since the two are semantically equivalent in PS.
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDefinition {
    /// A label for this function, to be passed onto the function value. This
    /// is **not necessarily** the name the function is bound to; that is
    /// defined in the parent [FunctionDeclaration].
    pub name: Option<Node<Identifier>>,
    pub parameters: Box<[Node<FunctionParameter>]>,
    // `Node` not necessary because both variants of the body contain it
    pub body: FunctionBody,
    /// A list of all the identifiers this function captures from its parent
    /// scope. This only includes identifiers that are actually present in the
    /// parent scope. References in the function parameters/body that don't
    /// exist anywhere in scope can't be captured and therefore won't appear
    /// here. They will either be provided by the global scope or trigger a
    /// runtime error.
    pub captures: Box<[Identifier]>,
}

/// The body of a function definition, which can be either a block or a bare
/// expression.
#[derive(Clone, Debug, PartialEq)]
pub enum FunctionBody {
    /// A single-expression body of an arrow function: `(x) => x + 1`
    Expression(Box<Node<Expression>>),
    /// A standard function body. Accessible through either syntax
    Block(Node<Block>),
}

/// One parameter in a function definition
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionParameter {
    pub variable: Node<Variable>,
    pub varargs: bool,
}

/// A conditional that executes executes its body only if the condition is
/// truthy.
///
/// ```notrust
/// if (condition) {
///   console.log("if!");
/// } else if (condition2) {
///   console.log("else if!");
/// } else {
///   console.log("else");
/// }
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct If {
    pub condition: Node<Expression>,
    pub body: Box<Node<Statement>>,
    /// Optional `else` block. For `else if` blocks, this will be a nested `if`
    pub else_body: Option<Box<Node<Statement>>>,
}

/// A loop that iterates over each element of a an array or string.
///
/// ```notrust
/// for (const n of [1, 2, 3]) {
///   console.log(n);
/// }
///
/// for (const c of "hello!") {
///   console.log(c);
/// }
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct ForOfLoop {
    pub binding: Binding,
    pub iterable: Node<Expression>,
    pub body: Box<Node<Statement>>,
}

/// A loop that runs until its condition is falsy.
/// ```notrust
/// while (shouldRun()) {
///   console.log("running");
/// }
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct WhileLoop {
    pub condition: Node<Expression>,
    pub body: Box<Node<Statement>>,
}

/// A loop that runs once, then continues to run until its condition is falsy.
/// ```notrust
/// do {
///   console.log("running");
/// } while (shouldKeepRunning());
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct DoWhileLoop {
    pub condition: Node<Expression>,
    pub body: Box<Node<Statement>>,
}

/// `import exportDefault, { export1, export2 as ex2 } from "module-name"`
/// No other import formats are supported
/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/import>
#[derive(Clone, Debug, PartialEq)]
pub struct ImportDeclaration {
    /// `exportDefault`
    pub default: Option<Node<Identifier>>,
    /// `{ export1, export2 as ex2 }`
    pub named: Box<[Node<ImportNamed>]>,
    /// `"module-name"`
    pub module: Node<ImportModule>,
}

/// One identifier in a named import clause. In this import:
///
/// ```notrust
/// import exportDefault, { export1, export2 as ex2 } from "module-name"
/// ```
///
/// the nameds are `export1` and `export2 as ex2`
#[derive(Clone, Debug, PartialEq)]
pub struct ImportNamed {
    /// `export1` or `export2`
    pub identifier: Node<Identifier>,
    /// `ex2`
    pub rename: Option<Node<Identifier>>,
}

/// Source for an imported module, i.e. the string after `from`
#[derive(Clone, Debug, PartialEq)]
pub enum ImportModule {
    /// A native module provided by the engine with a static name, like
    /// `import helpers from 'helpers'`
    Native(NativeModuleName),
    /// Another PetitScript file, e.g. `import helpers from './helpers'`. The
    /// imported file will be parsed during the parse of the parent, so we
    /// end up with a separate AST to be executed.
    Local(Node<Module>),
}

/// TODO move this somewhere
/// The name of a native module registered with the PS engine. Native module
/// names must adhere to these rules:
/// - Allowed characters are `a-z`, `A-Z`, `0-9`, `_` and `-`
/// - Must start with a letter `a-z` or `A-Z`
/// - Must be at least one character long
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

/// TODO
#[derive(Clone, Debug, PartialEq)]
pub enum ExportDeclaration {
    /// TODO
    Reexport {
        // TODO
    },
    /// `export const x = 3;`
    Declaration(Node<Declaration>),
    /// `export default 3;`
    DefaultExpression(Node<Expression>),
}

/// TODO
#[derive(Clone, Debug, PartialEq)]
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

/// TODO
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

/// TODO
/// TODO document why no spans
#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
pub struct TemplateLiteral {
    /// A set of contiguous chunks that comprise the template. These will be
    /// alternating in variant, e.g. `[Lit, Expr, Lit]` or `[Expr, Lit, Expr]`
    pub chunks: Box<[Node<TemplateChunk>]>,
}

/// One piece in a template. Either a static string or an expression
#[derive(Clone, Debug, PartialEq)]
pub enum TemplateChunk {
    /// This doesn't need a `Node` because we never refer to the literal chunks
    /// of a template on their own
    Literal(String),
    Expression(Node<Expression>),
}

/// TODO
#[derive(Clone, Debug, PartialEq)]
pub struct ArrayLiteral {
    pub elements: Box<[Node<ArrayElement>]>,
}

/// TODO
#[derive(Clone, Debug, PartialEq)]
pub enum ArrayElement {
    Expression(Node<Expression>),
    Spread(Node<Expression>),
}

/// TODO
#[derive(Clone, Debug, PartialEq)]
pub struct ObjectLiteral {
    pub properties: Box<[Node<ObjectProperty>]>,
}

/// TODO
#[derive(Clone, Debug, PartialEq)]
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

/// TODO
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall {
    pub function: Box<Node<Expression>>,
    pub arguments: Box<[Node<Expression>]>,
}

/// TODO
#[derive(Clone, Debug, PartialEq)]
pub struct PropertyAccess {
    pub expression: Box<Node<Expression>>,
    pub property: Node<PropertyName>,
}

/// TODO
#[derive(Clone, Debug, PartialEq)]
pub struct OptionalPropertyAccess {
    pub expression: Box<Node<Expression>>,
    pub property: Node<PropertyName>,
}

/// TODO
#[derive(Clone, Debug, PartialEq)]
pub struct UnaryOperation {
    pub operator: UnaryOperator,
    pub expression: Box<Node<Expression>>,
}

/// TODO
#[derive(Clone, Debug, PartialEq)]
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

/// TODO
#[derive(Clone, Debug, PartialEq)]
pub struct BinaryOperation {
    pub operator: BinaryOperator,
    pub lhs: Box<Node<Expression>>,
    pub rhs: Box<Node<Expression>>,
}

/// TODO
#[derive(Clone, Debug, PartialEq)]
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

/// TODO
#[derive(Clone, Debug, PartialEq)]
pub struct TernaryConditional {
    pub condition: Box<Node<Expression>>,
    pub true_expression: Box<Node<Expression>>,
    pub false_expression: Box<Node<Expression>>,
}

/// TODO
#[derive(Clone, Debug, PartialEq)]
pub enum PropertyName {
    /// Normal key: `{ key: value }`
    Literal(Node<Identifier>),
    /// Computed key: `{ ["key"]: value }`
    Expression(Box<Node<Expression>>),
}

/// TODO
#[derive(Clone, Debug, PartialEq)]
pub enum Binding {
    /// `const x = 3`
    Identifier(Node<Identifier>),
    /// An object pattern: `const { a, b, c } = object`
    Object(Box<[Node<ObjectPatternElement>]>),
    /// An array pattern: `const [a, b, c] = array`
    Array(Box<[Node<ArrayPatternElement>]>),
}

/// TODO
#[derive(Clone, Debug, PartialEq)]
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

/// TODO
#[derive(Clone, Debug, PartialEq)]
pub enum ArrayPatternElement {
    // TODO
}
