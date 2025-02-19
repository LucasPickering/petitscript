//! TODO
//!
//! Collection types all use `Box<[T]>` instead of `Vec<T>` because we know
//! they're all fixed size and won't need to grow. `Box<[T]>` ensures we don't
//! allocate more memory than needed.

// TODO comments on everything

use std::{
    fmt::{self, Display},
    ops::Deref,
};

/// Root AST node. This is a parsed program, ready to be executed
#[derive(Clone, Debug)]
pub struct Program {
    pub statements: Box<[Spanned<Statement>]>,
}

#[derive(Clone, Debug)]
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
#[derive(Clone, Debug)]
pub struct Block {
    pub statements: Box<[Spanned<Statement>]>,
}

#[derive(Clone, Debug)]
pub enum Declaration {
    Lexical(Spanned<LexicalDeclaration>),
    Function(Spanned<FunctionDeclaration>),
}

#[derive(Clone, Debug)]
pub struct LexicalDeclaration {
    pub variables: Box<[Spanned<Variable>]>,
    pub mutable: bool,
}

#[derive(Clone, Debug)]
pub struct Variable {
    pub binding: Binding,
    pub init: Option<Box<Spanned<Expression>>>,
}

#[derive(Clone, Debug)]
pub struct FunctionDeclaration {
    pub name: Option<Spanned<Identifier>>,
    pub parameters: Box<[Spanned<FunctionParameter>]>,
    /// We don't use [Block] here because we don't need this to create a new
    /// scope when entering. Function calls have special logic to create a new
    /// scope already.
    pub body: Box<[Spanned<Statement>]>,
}

#[derive(Clone, Debug)]
pub struct FunctionParameter {
    pub variable: Spanned<Variable>,
    pub varargs: bool,
}

#[derive(Clone, Debug)]
pub struct If {
    pub condition: Spanned<Expression>,
    pub body: Box<Spanned<Statement>>,
    /// Optional else block. For `else if` blocks, this will be a nested `if`
    pub else_body: Option<Box<Spanned<Statement>>>,
}

#[derive(Clone, Debug)]
pub struct ForLoop {
    pub initializer: Box<Spanned<Statement>>,
    pub condition: Spanned<Expression>,
    pub update: Box<Spanned<Statement>>,
    pub body: Box<Spanned<Statement>>,
}

#[derive(Clone, Debug)]
pub struct ForOfLoop {
    pub binding: Binding,
    pub iterable: Spanned<Expression>,
    pub body: Box<Spanned<Statement>>,
}

#[derive(Clone, Debug)]
pub struct WhileLoop {
    pub condition: Spanned<Expression>,
    pub body: Box<Spanned<Statement>>,
}

#[derive(Clone, Debug)]
pub struct DoWhileLoop {
    pub condition: Spanned<Expression>,
    pub body: Box<Spanned<Statement>>,
}

#[derive(Clone, Debug)]
pub struct ImportDeclaration {
    // TODO
}

#[derive(Clone, Debug)]
pub enum ExportDeclaration {
    Reexport {
        // TODO
    },
    Declaration(Spanned<Declaration>),
    DefaultFunctionDeclaration(Spanned<FunctionDeclaration>),
    DefaultExpression(Spanned<Expression>),
}

#[derive(Clone, Debug)]
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
    /// Lambda syntax: `(...) => {...}` or `() => value`
    ArrowFunction(Spanned<ArrowFunction>),
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

    pub fn into_string(self) -> String {
        self.0
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", 0)
    }
}

/// TODO document why no spans
#[derive(Clone, Debug)]
pub enum Literal {
    Null,
    Undefined,
    Boolean(bool),
    Float(f64),
    Int(i64),
    String(String),
    Array(ArrayLiteral),
    Object(ObjectLiteral),
}

#[derive(Clone, Debug)]
pub struct TemplateLiteral {
    // TODO
}

#[derive(Clone, Debug)]
pub struct ArrayLiteral {
    pub elements: Box<[Spanned<ArrayElement>]>,
}

#[derive(Clone, Debug)]
pub enum ArrayElement {
    Expression(Spanned<Expression>),
    Spread(Spanned<Expression>),
}

#[derive(Clone, Debug)]
pub struct ObjectLiteral {
    pub properties: Box<[Spanned<ObjectProperty>]>,
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct FunctionCall {
    pub function: Box<Spanned<Expression>>,
    pub arguments: Box<[Spanned<Expression>]>,
}

#[derive(Clone, Debug)]
pub struct PropertyAccess {
    pub expression: Box<Spanned<Expression>>,
    pub property: Spanned<PropertyName>,
}

#[derive(Clone, Debug)]
pub struct OptionalPropertyAccess {
    pub expression: Box<Spanned<Expression>>,
    pub property: Spanned<PropertyName>,
}

#[derive(Clone, Debug)]
pub struct ArrowFunction {
    pub parameters: Box<[Spanned<FunctionParameter>]>,
    pub body: Spanned<ArrowFunctionBody>,
}

#[derive(Clone, Debug)]
pub enum ArrowFunctionBody {
    Block(Box<[Spanned<Statement>]>),
    Expression(Box<Spanned<Expression>>),
}

#[derive(Clone, Debug)]
pub struct UnaryOperation {
    pub operator: UnaryOperator,
    pub expression: Box<Spanned<Expression>>,
}

#[derive(Clone, Debug)]
pub enum UnaryOperator {
    /// `!`
    BooleanNot,
    /// `-`
    Negate,
    // TODO bitwise operations
}

#[derive(Clone, Debug)]
pub struct BinaryOperation {
    pub operator: BinaryOperator,
    pub lhs: Box<Spanned<Expression>>,
    pub rhs: Box<Spanned<Expression>>,
}

#[derive(Clone, Debug)]
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
pub struct TernaryConditional {
    pub condition: Box<Spanned<Expression>>,
    pub true_expression: Box<Spanned<Expression>>,
    pub false_expression: Box<Spanned<Expression>>,
}

#[derive(Clone, Debug)]
pub struct AssignOperation {
    pub operator: AssignOperator,
    pub lhs: Spanned<Binding>,
    pub rhs: Box<Spanned<Expression>>,
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum PropertyName {
    /// Normal key: `{ key: value }`
    Literal(Spanned<Identifier>),
    /// Computed key: `{ ["key"]: value }`
    Expression(Box<Spanned<Expression>>),
}

#[derive(Clone, Debug)]
pub enum Binding {
    /// `const x = 3`
    Identifier(Spanned<Identifier>),
    /// An object pattern: `const { a, b, c } = object`
    Object(Box<[Spanned<ObjectPatternElement>]>),
    /// An array pattern: `const [a, b, c] = array`
    Array(Box<[Spanned<ArrayPatternElement>]>),
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum ArrayPatternElement {
    // TODO
}

// TODO move source span stuff?

/// TODO
#[derive(Copy, Clone, Debug)]
pub struct Span {
    /// Byte offset for the beginning of this span. Always <= end_span.
    start_offset: usize,
    /// Byte offset for the end of this span. Always >= start_span.
    end_offset: usize,
}

impl Span {
    pub fn new(start_offset: usize, end_offset: usize) -> Self {
        Self {
            start_offset,
            end_offset,
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO get lines/columns somehow
        write!(f, "{}-{}", self.start_offset, self.end_offset)
    }
}

/// TODO
#[derive(Copy, Clone, Debug)]
pub struct Spanned<T> {
    pub data: T,
    pub span: Span,
}

// TODO remove this?
impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

/// TODO
pub trait IntoSpanned: Sized {
    fn into_spanned(self, span: Span) -> Spanned<Self>;
}

impl<T> IntoSpanned for T {
    fn into_spanned(self, span: Span) -> Spanned<Self> {
        Spanned { data: self, span }
    }
}
