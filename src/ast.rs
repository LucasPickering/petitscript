//! [Abstract Syntax Tree](https://w.wiki/6jCi) types of PetitScript
//!
//! These define all the possible components of a valid PetitScript program.
//! Because PetitScript is implemented as a tree-walking interpreter, these
//! types also compose into executable PS programs.
//!
//! This AST is as simple as possible. Any unsupported JS/TS elements are
//! eliminated by the parser, and many other components are simplified from the
//! formal ECMAScript AST definitions.
//!
//! ## Node IDs
//!
//! Each AST node gets a unique ID, which is used to associate it with metadata
//! stored outside the AST (e.g. source spans). These IDs are stored inline in
//! each node struct, in a field called `id`. They can be accessed generically
//! via the [`Node`] trait. There are a lot of ways to do this and they all have
//! tradeoffs. Some considered alternatives:
//!
//! - Store the metadata inline in the AST. Replace each ID with the actual
//!   metadata. This eliminates the indirection of having a separate metadata
//!   table, but restricts you to a single metadata type.
//! - Generic inline metadata. The same solution as above, but add a generic
//!   param to each node type to define what metadata to store. This adds a lot
//!   of clutter and requires mapping between metadata types for cases like
//!   comparing parsed ASTs to generated ones in tests.
//! - Use a wrapper type that stores the ID, instead of putting it directly in
//!   the AST types. This removes repetition in the AST definition, but leads to
//!   lots of cluttered type signatures in code that handles AST types. It also
//!   creates ambiguity around which types should be used where (e.g.
//!   `Expression` vs `Node<Expression>`?).
//! - Use the memory address of each node as its implicit ID. This subverts the
//!   protection of the borrow checker and completely breaks as soon as you move
//!   an AST node (which is very common during construction and parsing).
//! - Store AST nodes in a bump-allocated arena to ensure addresses are stable.
//!   This adds a lot of complexity and is challenging to then store the AST and
//!   the encapsulating arena together because they are self-referential.
//! - Use structural keys, such as a traversal path pointing to each node in the
//!   tree. This is a recipe for disaster though, because any changes to the
//!   tree's structure during compilation will completely destroy the keys.
//!
//! Overall, storing IDs inline and metadata separately adds flexibility to
//! cover the two main use cases:
//! - ASTs parsed from source code
//! - ASTs generated programatically
//!
//! ## Collection Types
//!
//! Collection types all use `Box<[T]>` instead of `Vec<T>` because we know
//! they're all fixed size and won't need to grow. `Box<[T]>` ensures we don't
//! allocate more memory than needed.
//!
//! ## Building ASTs
//!
//! In addition to being constructed via parsing, ASTs can also be constructed
//! manually. This is useful for building assertions in test cases, but can
//! also be used to generate PetitScript code programatically.
//!
//! All AST fields are public so you can construct them fully by hand, but then
//! you have to manually assign node IDs. Instead, you can use the methods
//! defined on the node types to create them more easily. There are also 3
//! traits that make converting between node types easier:
//! - [`IntoNode`]: Convert from a node kind to its parent node (e.g.
//!   [`ExpressionKind`] -> [`Expression`])
//! - [`IntoExpression`]: Convert to [`Expression`] (e.g. [`Identifier`] ->
//!   [`Expression`])
//! - [`IntoStatement`]: Convert to [`Statement`] (e.g. [`Declaration`] ->
//!   [`Statement`])
//!
//! ```rust
//! use petitscript::{
//!     Engine, Exports,
//!     ast::{
//!         BinaryOperator, Declaration, Expression, FunctionBody,
//!         FunctionCall, FunctionDefinition, FunctionParameter, Identifier,
//!         IntoExpression, IntoStatement, Module,
//!     },
//! };
//!
//! let module = Module::new([
//!     // export const x = 3;
//!     Declaration::new("x", 3).export(),
//!     // const f = (n) => n * 2;
//!     Declaration::new(
//!         "f",
//!         FunctionDefinition::expression(
//!             [FunctionParameter::identifier("n")],
//!             Expression::binary(
//!                 BinaryOperator::Mul,
//!                 Expression::reference("n"),
//!                 2,
//!             ),
//!         ),
//!     )
//!     .into_stmt(),
//!     // export default f(x);
//!     FunctionCall::named("f", [Expression::reference("x")])
//!         .into_expr()
//!         .export_default(),
//! ]);
//!
//! // Use the Display impl to convert to source code
//! assert_eq!(
//!     module.to_string(),
//!     "\
//! export const x = 3;
//! const f = (n) => n * 2;
//! export default f(x);
//! "
//! );
//!
//! // You can also run the code directly:
//! let engine = Engine::new();
//! let process = engine.compile_ast(module);
//! let exports = process.execute().unwrap();
//! assert_eq!(exports, Exports::new(Some(6), [("x", 3)]));
//! ```

mod build;
mod display;
#[cfg(test)]
mod pbt;
mod walk;

// Re-export this because it fits conceptually here
pub use crate::name::Identifier;
pub use build::{IntoExpression, IntoNode, IntoStatement};
pub use walk::{AstVisitor, IdentifierPosition, Walk};

use crate::name::NativeModuleName;
use std::{
    cell::Cell,
    fmt::{self, Debug},
    hash::Hash,
    ops::{Deref, DerefMut},
};

/// Root AST node for a single source file
///
/// This is the outcome of parsing a file. It may contain nested modules via
/// [ImportModuleKind::Local].
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct Module {
    pub id: NodeId,
    pub statements: Box<[Statement]>,
}

impl Node for Module {
    fn id(&self) -> NodeId {
        self.id
    }
}

/// An imperative statement
///
/// `Statement` is one of the two primary AST types, the other being
/// [`Expression`]. Expressions resolve to values while statements execute
/// without emitting a value.
pub type Statement = KindedNode<StatementKind>;

/// Variants for [`Statement`]
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum StatementKind {
    /// Do nothing
    Empty,
    /// Collection of statements to be executed sequentially: `{...}`
    Block(Block),
    /// An expression in statement position: `expression;`
    ///
    /// This will *not* emit the expression's value. The value is discarded.
    Expression(Expression),
    /// `const` declaration
    Declaration(Declaration),

    // Control flow
    // Loops aren't supported at all because they aren't very useful without
    // mutation. I may add them at some point, but I'm starting minimal.
    /// `if`/`else` conditional
    If(If),
    /// Exit the current function, optionally with a return value: `return
    /// [expression];`
    Return(Option<Expression>),

    /// Import values from another module: `import ... from '...'`
    Import(ImportDeclaration),
    /// Export a name or value: `export const ...` or `export default ...`
    Export(ExportDeclaration),
}

/// A collection of statements to be executed sequentially
///
/// A block is delineated by `{ }` and creates a new lexical scope.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct Block {
    pub id: NodeId,
    pub statements: Box<[Statement]>,
}

impl Node for Block {
    fn id(&self) -> NodeId {
        self.id
    }
}

/// A single `const` declaration with one or more declared variables: `const x =
/// 3;` or `const x = 3, y = 4, z = 5;`
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct Declaration {
    pub id: NodeId,
    /// One or more variables to declare (cannot be empty)
    #[cfg_attr(test, proptest(strategy = "pbt::declaration_variables()"))]
    pub variables: Box<[Variable]>,
}

impl Node for Declaration {
    fn id(&self) -> NodeId {
        self.id
    }
}

/// A single declared variable in a declaration or function parameter
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
#[cfg_attr(test, proptest(params = "Option<bool>"))] // Forwarded to init
pub struct Variable {
    pub id: NodeId,
    /// Identifier(s) being bound
    pub binding: Binding,
    /// Expression to bind to
    ///
    /// `None` for declarations without initializers, such as (most) function
    /// parameters.
    #[cfg_attr(test, proptest(strategy = "pbt::variable_init(params)"))]
    pub init: Option<Box<Expression>>,
}

impl Node for Variable {
    fn id(&self) -> NodeId {
        self.id
    }
}

/// The parameters, body, and captures that constitute a user (i.e *not* native)
/// function definition. This covers both `function` functions and
/// arrow functions, since the two are semantically equivalent in PS.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct FunctionDefinition {
    pub id: NodeId,
    /// A label for this function, to be passed onto the function value
    ///
    /// This is **not necessarily** the name the function is bound to; that is
    /// defined in the parent [`Declaration`].
    pub name: Option<Identifier>,
    pub parameters: Box<[FunctionParameter]>,
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

impl Node for FunctionDefinition {
    fn id(&self) -> NodeId {
        self.id
    }
}

/// The body of a function definition, which can be either a block or a bare
/// expression.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub enum FunctionBody {
    /// A single-expression body of an arrow function: `(x) => x + 1`
    Expression(Box<Expression>),
    /// A standard function body. Accessible through either syntax
    Block(Block),
}

impl Node for FunctionBody {
    fn id(&self) -> NodeId {
        // Transparent impl is ok because the body has no syntax outside its
        // child
        match self {
            FunctionBody::Expression(expression) => expression.id(),
            FunctionBody::Block(block) => block.id(),
        }
    }
}

/// One parameter in a function definition
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))] // TODO varargs at end
pub struct FunctionParameter {
    pub id: NodeId,
    pub variable: Variable,
    pub varargs: bool,
}

impl Node for FunctionParameter {
    fn id(&self) -> NodeId {
        self.id
    }
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
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct If {
    pub id: NodeId,
    pub condition: Expression,
    pub body: Box<Statement>,
    /// Optional `else` block. For `else if` blocks, this will be a nested `if`
    pub else_body: Option<Box<Statement>>,
}

impl Node for If {
    fn id(&self) -> NodeId {
        self.id
    }
}

/// `import exportDefault, { export1, export2 as ex2 } from "module-name"`
///
/// No other import formats are supported.
///
/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/import>
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct ImportDeclaration {
    pub id: NodeId,
    /// `exportDefault`
    pub default: Option<IdentifierNode>,
    /// `{ export1, export2 as ex2 }`
    pub named: Box<[ImportNamed]>,
    /// `"module-name"`
    pub module: ImportModule,
}

impl Node for ImportDeclaration {
    fn id(&self) -> NodeId {
        self.id
    }
}

/// One identifier in a named import clause. In this import:
///
/// ```notrust
/// import exportDefault, { export1, export2 as ex2 } from "module-name"
/// ```
///
/// the nameds are `export1` and `export2 as ex2`
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct ImportNamed {
    pub id: NodeId,
    /// External name being imported
    pub imported: IdentifierNode,
    /// Local name bound to the imported value
    ///
    /// If the import is not renamed, this will match `imported`.
    pub local: IdentifierNode,
}

impl Node for ImportNamed {
    fn id(&self) -> NodeId {
        self.id
    }
}

/// Source for an imported module, i.e. the string after `from`
pub type ImportModule = KindedNode<ImportModuleKind>;

/// Variants for [`ImportModule`]
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub enum ImportModuleKind {
    /// A native module provided by the engine with a static name, like
    /// `import helpers from 'helpers'`
    Native(NativeModuleName),
    /// Another PetitScript file, e.g. `import helpers from './helpers'`
    ///
    /// The imported file will be parsed during the parse of the parent, so we
    /// end up with a separate AST to be executed.
    Local(Module),
}

/// Export a name or value: `export const ...` or `export default ...`
pub type ExportDeclaration = KindedNode<ExportDeclarationKind>;

/// Variants for [`ExportDeclaration`]
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub enum ExportDeclarationKind {
    /// `export const x = 3;`
    Declaration(Declaration),
    /// `export default 3;`
    DefaultExpression(Expression),
}

/// An expresion that resolves to a value
///
/// `Expression` is one of the two primary AST types, the other being
/// [`Statement`]. Expressions resolve to values while statements execute
/// without emitting a value.
pub type Expression = KindedNode<ExpressionKind>;

/// Variants for [`Expression`]
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ExpressionKind {
    /// Parenthetical grouping
    Parenthesized(Box<Expression>),
    /// Primitive and complex type literals
    Literal(Literal),
    /// Literal defined with backticks (`` ` ``)
    Template(TemplateLiteral),
    /// Reference to an identifier
    ///
    /// `x` in `x + 1`.
    Identifier(IdentifierNode),
    /// Function call
    Call(FunctionCall),
    /// The static or dynamic property accessors: `.` or `[]`
    Property(PropertyAccess),
    /// Optional chaining operator: `?.`
    OptionalProperty(OptionalPropertyAccess),
    /// Function definition
    ///
    /// This is accessible via both function syntaxes:
    /// - Arrow functions `(...) => {...}` or `() => value`
    /// - `function`: `function f() {...}`
    ///
    /// The two different syntaxes are combined during parsing, for simplicity.
    /// PS support the `this` keyword, so the two are equivalent at runtime.
    Function(FunctionDefinition),
    /// Operation with one operand
    Unary(UnaryOperation),
    /// Operation with two operands
    Binary(BinaryOperation),
    /// Operation with three operands
    ///
    /// This is just the `?:` operator
    Ternary(TernaryConditional),
}

/// Fixed value defined literally in source code
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub enum Literal {
    /// `null`: the empty value
    Null,
    /// `undefined`: the *other* empty value
    ///
    /// In ECMAScript, `undefined` is a global identifier. In PetitScript, it's
    /// a keyword bound to a specific literal type.
    Undefined,
    /// `true`/`false`
    Boolean(bool),
    /// Floating-point (decimal) literal: `3.21`
    Float(f64),
    /// Integer literal: `-37`
    Int(i64),
    /// String literal: `"hello"` or `'world'`
    String(String),
    /// Array literal: `[1, "hello"]`
    Array(ArrayLiteral),
    /// Object literal: `{a: 1, b: "hello"}`
    Object(ObjectLiteral),
}

/// Literal defined with backticks (`` ` ``) that enables string interpolation
///
/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals>
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))] // TODO alternating chunks
pub struct TemplateLiteral {
    pub id: NodeId,
    /// A set of contiguous chunks that comprise the template. These will be
    /// alternating in variant, e.g. `[Lit, Expr, Lit]` or `[Expr, Lit, Expr]`
    pub chunks: Box<[TemplateChunk]>,
}

impl Node for TemplateLiteral {
    fn id(&self) -> NodeId {
        self.id
    }
}

/// One piece in a template string: the `hello ` or `${world}` of `` `hello
/// ${world}` ``
///
/// Either a static string or an expression
pub type TemplateChunk = KindedNode<TemplateChunkKind>;

/// Variants for [`TemplateChunk`]
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub enum TemplateChunkKind {
    /// This doesn't need a `Node` because we never refer to the literal chunks
    /// of a template on their own
    Literal(String),
    Expression(Expression),
}

/// [`Array`](crate::value::Array) literal: `[1, "hello"]`
///
/// Array literals do *not* support element elision (`[1, , 3]`). All elements
/// must be defined.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct ArrayLiteral {
    pub id: NodeId,
    pub elements: Box<[ArrayElement]>,
}

impl Node for ArrayLiteral {
    fn id(&self) -> NodeId {
        self.id
    }
}

/// One element in an array literal: `x` in `[x]`
pub type ArrayElement = KindedNode<ArrayElementKind>;

/// Variants for [`ArrayElement`]
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub enum ArrayElementKind {
    /// A single expression to resolve to an array literal value
    Expression(Expression),
    /// A `...` spread expression
    ///
    /// If this expression evaluates into an array, then all the elements of
    /// that array will be included in the array created by this literal. If it
    /// evaluates to any other value type, this will raise an error.
    ///
    /// ```notrust
    /// [1, 2 ...[3, 4, 5]] // [1, 2, 3, 4, 5]
    /// ```
    Spread(Expression),
}

/// [`Object`](crate::value::Object) literal: `{a: 1, b: "hello"}`
///
/// Object literals can contain both constant and expression keys:
/// ```notrust
/// {
///   x: 1,
///   [':::']: "colons!",
///   [f()]: "func_key",
/// }
/// ```
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct ObjectLiteral {
    pub id: NodeId,
    pub properties: Box<[ObjectProperty]>,
}

impl Node for ObjectLiteral {
    fn id(&self) -> NodeId {
        self.id
    }
}

/// One property in an object literal
///
/// Objects don't support methods, getters, or setters
pub type ObjectProperty = KindedNode<ObjectPropertyKind>;

/// Variants for [`ObjectProperty`]
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub enum ObjectPropertyKind {
    /// Normal key:value mapping
    ///
    /// `{ key: value }`, `{ key }`, or `{ ["key"]: value }`
    Property {
        property: PropertyName,
        expression: Expression,
    },
    /// Spread: `{ ...other }`
    Spread(Expression),
}

/// Invoke a function: `f(x)`
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct FunctionCall {
    pub id: NodeId,
    /// Expression evaluating to the function to invoke
    pub function: Box<Expression>,
    /// Values to pass to the function
    pub arguments: Box<[Argument]>,
    /// Skip call on `null`/`undefined`
    ///
    /// This is enabled by the `f?.()` syntax. If `true` and `function`
    /// evaluates to `null` or `undefined`, then the function call will be
    /// skipped and the arguments will *not* be evaluated. If the value is
    /// defined but any type other than a function, the call will still fail.
    pub optional: bool,
}

impl Node for FunctionCall {
    fn id(&self) -> NodeId {
        self.id
    }
}

/// One argument in a [`FunctionCall`]
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub enum Argument {
    /// Regular passed argument: `f(1)`
    Expression(Box<Expression>),
    /// Spread expression: `f(...[1, 2, 3])`
    ///
    /// If the passed expression evaluates to an array, it will be passed
    /// as multiple arguments. Otherwise, error!
    Spread(Box<Expression>),
}

/// Access a property from an object: `o.x` or `o["x"]`
///
/// If `object` is an object and the property is in that object, this will
/// evaluate to the value of the corresponding property. If `object` is `null`
/// or `undefined`, this will raise an error. If the property isn't in the
/// object or if `object` is any other value (e.g. a number or function), it
/// will evaluate to `undefined`.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct PropertyAccess {
    pub id: NodeId,
    /// Object to access into
    pub object: Box<Expression>,
    /// Property to access
    pub property: PropertyName,
}

impl Node for PropertyAccess {
    fn id(&self) -> NodeId {
        self.id
    }
}

/// Access a property from an object,
///
/// This behaves the same as [`PropertyAccess`] **except**. In the case where
/// `object` is `null` or `undefined`. Rather than raise an error, it will
/// return the value of `object` instead.
///
/// ```notrust
/// o?.x // o.x
/// null?.x // null
/// undefined?.x // undefined
/// ```
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct OptionalPropertyAccess {
    pub id: NodeId,
    /// Object to access into
    pub object: Box<Expression>,
    /// Property to access
    pub property: PropertyName,
}

impl Node for OptionalPropertyAccess {
    fn id(&self) -> NodeId {
        self.id
    }
}

/// Operation with one
///
/// All unary operators are *prefix*, meaning the operator appears *before*
/// the operand.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct UnaryOperation {
    pub id: NodeId,
    /// Operation to perform
    pub operator: UnaryOperator,
    /// Operand to transform
    pub expression: Box<Expression>,
}

impl Node for UnaryOperation {
    fn id(&self) -> NodeId {
        self.id
    }
}

/// Operator for [`UnaryOperation`]
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
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

/// Operation with two operands
///
/// All binary operators are *infix*, meaning the operator appears *between*
/// the operands.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct BinaryOperation {
    pub id: NodeId,
    /// Operation to perform
    pub operator: BinaryOperator,
    /// Left-hand side of the operation
    pub lhs: Box<Expression>,
    /// Right-hand side of the operation
    pub rhs: Box<Expression>,
}

impl Node for BinaryOperation {
    fn id(&self) -> NodeId {
        self.id
    }
}

/// Operator for [`BinaryOperation`]
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
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
    LogicalAnd,
    /// `||`
    LogicalOr,

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
}

/// Conditional expression: `condition ? then : else`
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct TernaryConditional {
    pub id: NodeId,
    pub condition: Box<Expression>,
    pub true_expression: Box<Expression>,
    pub false_expression: Box<Expression>,
}

impl Node for TernaryConditional {
    fn id(&self) -> NodeId {
        self.id
    }
}

/// Name of an object property
///
/// This can appear in 3 places:
/// - Object literal: `const o = { a: 1 };`
/// - Object pattern: `const { a } = { a: 1 };`
/// - Property access: `o.a` or `o["a"]`
pub type PropertyName = KindedNode<PropertyNameKind>;

/// Variants for [`PropertyName`]
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub enum PropertyNameKind {
    /// Normal key: `{ key: value }`
    Identifier(IdentifierNode),
    /// Computed key: `{ ["key"]: value }`
    Expression(Box<Expression>),
}

/// Bind identifier(s) to value(s)
///
/// A binding assigns identifiers to values. Each identifier is mapped to a
/// singular value.
///
/// ```notrust
/// // Bindings cannot be declared in any context. In this case, the const
/// // keyword uses bindings to create declarations.
/// const x = 3;
/// const { a, b } = { a: 1, b: 2 };
/// const [d, e, f] = [1, 2, 3];
/// ```
pub type Binding = KindedNode<BindingKind>;

/// Variants for [`Binding`]
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum BindingKind {
    /// Bind a single name to a value: `x = 3`
    Identifier(IdentifierNode),
    /// Bind multiple names to a destructed object: `{ a, b, c } = object`
    ///
    /// This is recursive, so object and array patterns can be nested within.
    Object {
        properties: Box<[ObjectPatternElement]>,
        /// `{ ...x } = object`
        ///
        /// This can only appear once, at the end of the pattern.
        rest: Option<Box<Binding>>,
    },
    /// Bind multiple names to a destructured array: `[a, b, c] = array`
    ///
    /// This is recursive, so object and array patterns can be nested within.
    Array {
        elements: Box<[ArrayPatternElement]>,
        /// `[...x] = array`
        ///
        /// This can only appear once, at the end of the pattern.
        rest: Option<Box<Binding>>,
    },
}

/// A specific bound field in a [`BindingKind::Object`]
///
/// - `const { x } = object`
/// - `const { x: x2 } = object`
/// - `const { x: x2 = 3 } = object`
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct ObjectPatternElement {
    pub id: NodeId,
    /// Key extracted from the object
    pub key: PropertyName,
    /// Name to bind the value to
    pub binding: Binding,
    /// Optional default expression for properties not in the object
    pub init: Option<Expression>,
}

impl Node for ObjectPatternElement {
    fn id(&self) -> NodeId {
        self.id
    }
}

/// One element in a [`BindingKind::Array`]
pub type ArrayPatternElement = KindedNode<ArrayPatternElementKind>;

/// Variants for [`ArrayPatternElement`]
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub enum ArrayPatternElementKind {
    /// A specific bound field
    ///
    /// - `const [x] = array`
    /// - `const [x = 3] = array`
    Identifier {
        /// Name to bind the value to
        binding: Binding,
        /// Optional default expression for elements not in the array
        init: Option<Expression>,
    },
    /// Skip this field while binding
    ///
    /// `const [a,, c] = [1, 2, 3]`
    Skip,
}

/// An [`Identifier`] with a [`NodeId`]
///
/// Use this for identifiers that exist in the source code.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct IdentifierNode {
    pub id: NodeId,
    pub identifier: Identifier,
}

impl Deref for IdentifierNode {
    type Target = Identifier;

    fn deref(&self) -> &Self::Target {
        &self.identifier
    }
}

impl DerefMut for IdentifierNode {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.identifier
    }
}

impl Node for IdentifierNode {
    fn id(&self) -> NodeId {
        self.id
    }
}

/// An identifier that uniquely identifies an AST node **within that AST**
///
/// These IDs are not globally unique, just within a single AST.
///
/// ## Equality
///
/// This type uses simple equality checking by default, which is important for
/// its usage as map keys. However, it can be annoying when doing test
/// assertions on constructed ASTs. In those cases, the IDs can vary widely and
/// you probably only care about checking the structure of the AST, not the
/// exact ID values. You can use [`NodeId::with_all_equal`] to temporarily
/// disable equality checks.
#[expect(
    // Hash is safe because PartialEq is non-standard only in a very limited and
    // intentional context. Deserialize warning is noise because the unsafe comes
    // from the thread-local, which doesn't affect ser/de.
    clippy::derived_hash_with_manual_eq,
    clippy::unsafe_derive_deserialize
)]
#[derive(Copy, Clone, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct NodeId(pub u32);

impl NodeId {
    thread_local! {
        /// Should all node IDs be considered equal?
        ///
        /// This can be enabled temporarily with [`NodeId::with_all_equal`] to
        /// skip ID checks. This is most useful in test code assertions, where
        /// exact ID values aren't important.
        static ALWAYS_EQUAL: Cell<bool> = const { Cell::new(false) };
    }

    /// Execute a function while overriding `NodeId`'s [`Eq`]/[`PartialEq`]
    /// implementations such that they **always return true**
    ///
    /// Use this for test assertions and other comparisons that want to ignore
    /// node ID equality. The override will be set *only* while in `f`.
    ///
    /// ## Examples
    ///
    /// ```
    /// # use petitscript::ast::{Expression, NodeId};
    /// let e1 = Expression {
    ///     id: NodeId(0),
    ///     kind: 3.into(),
    /// };
    /// let e2 = Expression {
    ///     id: NodeId(1),
    ///     kind: 3.into(),
    /// };
    /// assert_ne!(e1, e2); // e1 != e2
    /// NodeId::with_all_equal(|| assert_eq!(e1, e2)); // e1 == e2
    /// ```
    pub fn with_all_equal<T>(f: impl FnOnce() -> T) -> T {
        Self::ALWAYS_EQUAL.set(true);
        let out = f();
        Self::ALWAYS_EQUAL.set(false);
        out
    }
}

impl Debug for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // The wrapper provides no debugging value, just clutter
        self.0.fmt(f)
    }
}

impl PartialEq for NodeId {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 || Self::ALWAYS_EQUAL.get()
    }
}

/// An AST node
///
/// Every AST node has a unique ID
pub trait Node {
    /// Get this node's ID
    fn id(&self) -> NodeId;
}

/// An AST node whose content is defined by an enum
///
/// This is just a pairing of an enum and node ID.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct KindedNode<T> {
    /// Node ID
    pub id: NodeId,
    /// Node contents
    pub kind: T,
}

impl<K> Node for KindedNode<K> {
    fn id(&self) -> NodeId {
        self.id
    }
}
