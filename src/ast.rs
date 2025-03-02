//! TODO
//!
//! Collection types all use `Box<[T]>` instead of `Vec<T>` because we know
//! they're all fixed size and won't need to grow. `Box<[T]>` ensures we don't
//! allocate more memory than needed.

// TODO comments on everything

pub mod source;
pub mod walk;

use crate::{ast::source::Spanned, compile::FunctionDefinitionId};
use std::{
    fmt::{self, Display},
    hash::Hash,
};

/// The root AST node. This is the outcome of parsing a program, but is not yet
/// ready for execution.
#[derive(Clone, Debug, Hash)]
pub struct Ast {
    pub statements: Box<[Spanned<Statement>]>,
}

#[derive(Clone, Debug, Hash)]
pub enum Statement {
    Empty,
    Block(Spanned<Block>),
    Expression(Spanned<Expression>),
    Declaration(Spanned<Declaration>),

    If(Spanned<If>),
    ForLoop(Spanned<ForLoop>),
    ForOfLoop(Spanned<ForOfLoop>),
    WhileLoop(Spanned<WhileLoop>),
    DoWhileLoop(Spanned<DoWhileLoop>),

    Return(Option<Spanned<Expression>>),
    Break,
    Continue,

    Import(Spanned<ImportDeclaration>),
    Export(Spanned<ExportDeclaration>),
    // TODO: switch, throw, try, catch, finally
}

/// A collection of statements, delineated by {}. This denotes a new
/// lexical scope.
/// TODO kill this and inline into the enum variant?
#[derive(Clone, Debug, Hash)]
pub struct Block {
    pub statements: Box<[Spanned<Statement>]>,
}

#[derive(Clone, Debug, Hash)]
pub enum Declaration {
    Lexical(Spanned<LexicalDeclaration>),
    Function(Spanned<FunctionDeclaration>),
}

#[derive(Clone, Debug, Hash)]
pub struct LexicalDeclaration {
    pub variables: Box<[Spanned<Variable>]>,
    pub mutable: bool,
}

#[derive(Clone, Debug, Hash)]
pub struct Variable {
    pub binding: Binding,
    pub init: Option<Box<Spanned<Expression>>>,
}

#[derive(Clone, Debug, Hash)]
pub struct FunctionDeclaration {
    pub pointer: FunctionPointer,
}

/// TODO rename this
#[derive(Clone, Debug, Hash)]
pub enum FunctionPointer {
    /// Lifting hasn't been performed yet, the function definitions is still
    /// inline. This code isn't executable yet!
    Inline(Spanned<FunctionDefinition>),
    /// Function definition has been lifted to the top of the program, and this
    /// is just a pointer to the definition
    Lifted {
        id: FunctionDefinitionId,
        /// Retain the name from the declaration, if there was one, for
        /// convenience
        name: Option<Spanned<Identifier>>,
    },
}

/// TODO
#[derive(Clone, Debug, Hash)]
pub struct FunctionDefinition {
    pub name: Option<Spanned<Identifier>>,
    pub parameters: Box<[Spanned<FunctionParameter>]>,
    /// We don't use [Block] here because we don't need this to create a new
    /// scope when entering. Function calls have special logic to create a new
    /// scope already.
    pub body: Box<[Spanned<Statement>]>,
}

/// One parameter in a function definition
#[derive(Clone, Debug, Hash)]
pub struct FunctionParameter {
    pub variable: Spanned<Variable>,
    pub varargs: bool,
}

#[derive(Clone, Debug, Hash)]
pub struct If {
    pub condition: Spanned<Expression>,
    pub body: Box<Spanned<Statement>>,
    /// Optional else block. For `else if` blocks, this will be a nested `if`
    pub else_body: Option<Box<Spanned<Statement>>>,
}

#[derive(Clone, Debug, Hash)]
pub struct ForLoop {
    pub initializer: Box<Spanned<Statement>>,
    pub condition: Spanned<Expression>,
    pub update: Box<Spanned<Statement>>,
    pub body: Box<Spanned<Statement>>,
}

#[derive(Clone, Debug, Hash)]
pub struct ForOfLoop {
    pub binding: Binding,
    pub iterable: Spanned<Expression>,
    pub body: Box<Spanned<Statement>>,
}

#[derive(Clone, Debug, Hash)]
pub struct WhileLoop {
    pub condition: Spanned<Expression>,
    pub body: Box<Spanned<Statement>>,
}

#[derive(Clone, Debug, Hash)]
pub struct DoWhileLoop {
    pub condition: Spanned<Expression>,
    pub body: Box<Spanned<Statement>>,
}

#[derive(Clone, Debug, Hash)]
pub struct ImportDeclaration {
    // TODO
}

#[derive(Clone, Debug, Hash)]
pub enum ExportDeclaration {
    Reexport {
        // TODO
    },
    Declaration(Spanned<Declaration>),
    DefaultFunctionDeclaration(Spanned<FunctionDeclaration>),
    DefaultExpression(Spanned<Expression>),
}

#[derive(Clone, Debug, Hash)]
pub enum Expression {
    Parenthesized(Box<Spanned<Expression>>),
    /// Primitive and complex type literals
    Literal(Spanned<Literal>),
    Template(Spanned<TemplateLiteral>),
    Identifier(Spanned<Identifier>),
    Call(Spanned<FunctionCall>),
    /// The static or dynamic property accessors: `.` or `[]`
    Property(Spanned<PropertyAccess>),
    /// Optional chaining operator: `?.`
    OptionalProperty(Spanned<OptionalPropertyAccess>),
    /// Lambda syntax: `(...) => {...}` or `() => value`. This shares the same
    /// AST node as the `function` syntax; they get combined during parsing
    ArrowFunction(Spanned<FunctionPointer>),
    Unary(Spanned<UnaryOperation>),
    Binary(Spanned<BinaryOperation>),
    Ternary(Spanned<TernaryConditional>),
    Assign(Spanned<AssignOperation>),
    // TODO update operations (++, --)
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Identifier(String);

impl Identifier {
    pub fn new(s: String) -> Self {
        Self(s)
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// TODO document why no spans
#[derive(Clone, Debug, Hash)]
pub enum Literal {
    Null,
    Undefined,
    Boolean(bool),
    Float(HashableF64),
    Int(i64),
    String(String),
    Array(ArrayLiteral),
    Object(ObjectLiteral),
}

/// TODO remove this
#[derive(Clone, Debug)]
pub struct HashableF64(pub f64);

impl Hash for HashableF64 {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {}
}

#[derive(Clone, Debug, Hash)]
pub struct TemplateLiteral {
    // TODO
}

#[derive(Clone, Debug, Hash)]
pub struct ArrayLiteral {
    pub elements: Box<[Spanned<ArrayElement>]>,
}

#[derive(Clone, Debug, Hash)]
pub enum ArrayElement {
    Expression(Spanned<Expression>),
    Spread(Spanned<Expression>),
}

#[derive(Clone, Debug, Hash)]
pub struct ObjectLiteral {
    pub properties: Box<[Spanned<ObjectProperty>]>,
}

#[derive(Clone, Debug, Hash)]
pub enum ObjectProperty {
    /// Normal key value: `{ key: value }` or `{ ["key"]: value }`
    Property {
        property: Spanned<PropertyName>,
        expression: Spanned<Expression>,
    },
    /// Identifier shorthand: `{ name }`
    Identifier(Spanned<Identifier>),
    /// Spread: `{ ...other }`
    Spread(Spanned<Expression>),
}

#[derive(Clone, Debug, Hash)]
pub struct FunctionCall {
    pub function: Box<Spanned<Expression>>,
    pub arguments: Box<[Spanned<Expression>]>,
}

#[derive(Clone, Debug, Hash)]
pub struct PropertyAccess {
    pub expression: Box<Spanned<Expression>>,
    pub property: Spanned<PropertyName>,
}

#[derive(Clone, Debug, Hash)]
pub struct OptionalPropertyAccess {
    pub expression: Box<Spanned<Expression>>,
    pub property: Spanned<PropertyName>,
}

#[derive(Clone, Debug, Hash)]
pub struct UnaryOperation {
    pub operator: UnaryOperator,
    pub expression: Box<Spanned<Expression>>,
}

#[derive(Clone, Debug, Hash)]
pub enum UnaryOperator {
    /// `!`
    BooleanNot,
    /// `-`
    Negate,
    // TODO bitwise operations
}

#[derive(Clone, Debug, Hash)]
pub struct BinaryOperation {
    pub operator: BinaryOperator,
    pub lhs: Box<Spanned<Expression>>,
    pub rhs: Box<Spanned<Expression>>,
}

#[derive(Clone, Debug, Hash)]
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

#[derive(Clone, Debug, Hash)]
pub struct TernaryConditional {
    pub condition: Box<Spanned<Expression>>,
    pub true_expression: Box<Spanned<Expression>>,
    pub false_expression: Box<Spanned<Expression>>,
}

#[derive(Clone, Debug, Hash)]
pub struct AssignOperation {
    pub operator: AssignOperator,
    pub lhs: Spanned<Binding>,
    pub rhs: Box<Spanned<Expression>>,
}

#[derive(Clone, Debug, Hash)]
pub enum AssignOperator {
    /// `x = y`
    Assign,
    /// `x += y`
    Add,
    /// `x -= y`
    Sub,
    /// `x *= y`
    Mul,
    /// `x /= y`
    Div,
    /// `x %= y`
    Mod,
    /// `x &&= y`
    BooleanAnd,
    /// `x ||= y`
    BooleanOr,
    /// `x ??= y`
    NullishCoalesce,
    // TODO bitwise operations, exponent
}

#[derive(Clone, Debug, Hash)]
pub enum PropertyName {
    /// Normal key: `{ key: value }`
    Literal(Spanned<Identifier>),
    /// Computed key: `{ ["key"]: value }`
    Expression(Box<Spanned<Expression>>),
}

#[derive(Clone, Debug, Hash)]
pub enum Binding {
    /// `const x = 3`
    Identifier(Spanned<Identifier>),
    /// An object pattern: `const { a, b, c } = object`
    Object(Box<[Spanned<ObjectPatternElement>]>),
    /// An array pattern: `const [a, b, c] = array`
    Array(Box<[Spanned<ArrayPatternElement>]>),
}

#[derive(Clone, Debug, Hash)]
pub enum ObjectPatternElement {
    /// `const { x } = object` or `const { x = 3 } = object`
    Identifier {
        identifier: Spanned<Identifier>,
        init: Option<Spanned<Expression>>,
    },
    /// `const { x: x2 } = object` or `const { x: x2 = 3 } = object`
    Mapped {
        key: Spanned<PropertyName>,
        value: Spanned<Binding>,
        init: Option<Spanned<Expression>>,
    },
    /// `const { ...x } = object` or `const { ...x = {} } = object`
    Rest {
        binding: Spanned<Binding>,
        init: Option<Spanned<Expression>>,
    },
}

#[derive(Clone, Debug, Hash)]
pub enum ArrayPatternElement {
    // TODO
}
