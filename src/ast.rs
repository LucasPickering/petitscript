//! TODO
//! TODO explain why we use Box<[T]> instead of Vec<T>

// TODO comments on everything

/// TODO
#[derive(Debug)]
pub struct Script {
    pub statements: Box<[Statement]>,
}

#[derive(Debug)]
pub enum Statement {
    Block(Block),
    Lexical(LexicalDeclaration),
    Function(FunctionDeclaration),

    If(If),
    ForLoop(ForLoop),
    ForOfLoop(ForOfLoop),
    WhileLoop(WhileLoop),
    DoWhileLoop(DoWhileLoop),

    Return(Option<Expression>),
    Break(Option<Label>),
    Continue(Option<Label>),

    Import {},
    Export {},
    ExportDefault(Expression),
}

/// A collection of statements, delineated by {}. This denotes a new
/// lexical scope.
#[derive(Debug)]
pub struct Block {
    pub statements: Box<[Statement]>,
}

#[derive(Debug)]
pub struct LexicalDeclaration {
    pub binding: Binding,
    pub expression: Expression,
    pub mutable: bool,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: Option<Identifier>,
    pub parameters: Box<[FunctionParameter]>,
    pub body: Block,
}

#[derive(Debug)]
pub struct FunctionParameter {
    pub binding: Binding,
    pub varargs: bool,
}

#[derive(Debug)]
pub struct If {
    pub condition: Expression,
    pub body: Box<Statement>,
    /// Optional else block. For `else if` blocks, this will be a nested `if`
    pub else_body: Option<Box<Statement>>,
}

#[derive(Debug)]
pub struct ForLoop {
    pub initializer: Box<Statement>,
    pub condition: Expression,
    pub update: Box<Statement>,
    pub body: Box<Statement>,
}

#[derive(Debug)]
pub struct ForOfLoop {
    pub binding: Binding,
    pub iterable: Expression,
    pub body: Box<Statement>,
}

#[derive(Debug)]
pub struct WhileLoop {
    pub condition: Expression,
    pub body: Box<Statement>,
}

#[derive(Debug)]
pub struct DoWhileLoop {
    // TODO
}

#[derive(Debug)]
pub enum Expression {
    Parenthesized(Box<Expression>),
    /// Primitive and complex type literals
    Literal(Literal),
    /// The static or dynamic property accessors: `.` or `[]`
    Property(PropertyAccess),
    /// Optional chaining operator: `?.`
    OptionalProperty(PropertyAccess),
    /// Lambda syntax: `(...) => {...}` or `() => value`
    ArrowFunction(ArrowFunction),
    Unary(UnaryOperation),
    Binary(BinaryOperation),
    Ternary(TernaryConditional),
    Assign(AssignOperation),
    // TODO update operations (++, --)
}

#[derive(Debug)]
pub struct Identifier(String);

#[derive(Debug)]
pub struct Label(String);

#[derive(Debug)]
pub enum Literal {
    Null,
    Undefined,
    Boolean(bool),
    Float(f64),
    Int(i64),
    String(String),
    Template(TemplateLiteral),
    Array(Box<[ArrayEntry]>),
    Object(Box<[ObjectEntry]>),
}

#[derive(Debug)]
pub struct TemplateLiteral {
    // TODO
}

#[derive(Debug)]
pub struct PropertyAccess {
    expression: Box<Expression>,
    property: Property,
}

#[derive(Debug)]
pub struct ArrowFunction {
    pub parameters: Box<[FunctionParameter]>,
    pub body: ArrowFunctionBody,
}

#[derive(Debug)]
pub enum ArrowFunctionBody {
    Block(Block),
    Expression(Box<Expression>),
}

#[derive(Debug)]
pub struct UnaryOperation {
    operator: UnaryOperator,
    expression: Box<Expression>,
}

#[derive(Debug)]
pub enum UnaryOperator {
    /// `!`
    BooleanNot,
    /// `-`
    Negate,
    // TODO bitwise operations
}

#[derive(Debug)]
pub struct BinaryOperation {
    operator: BinaryOperator,
    lhs: Box<Expression>,
    rhs: Box<Expression>,
}

#[derive(Debug)]
pub enum BinaryOperator {
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Multiply,
    /// `/`
    Divide,
    /// `%`
    Remainder,

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

#[derive(Debug)]
pub struct TernaryConditional {
    pub condition: Box<Expression>,
    pub true_expression: Box<Expression>,
    pub false_expression: Box<Expression>,
}

#[derive(Debug)]
pub struct AssignOperation {
    operator: AssignOperator,
    lhs: Binding,
    rhs: Box<Expression>,
}

#[derive(Debug)]
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
    Coalesce,
    // TODO bitwise operations, exponent
}

#[derive(Debug)]
pub enum ArrayEntry {
    Value(Expression),
    Spread(Expression),
}

#[derive(Debug)]
pub enum ObjectEntry {
    /// Normal key value: `{ key: value }` or `{ ["key"]: value }`
    Value {
        property: Property,
        value: Expression,
    },
    /// Identifier shorthand: `{ name }`
    Identifier(Identifier),
    /// Spread: `{ ...other }`
    Spread(Expression),
}

#[derive(Debug)]
pub enum Property {
    /// Normal key: `{ key: value }`
    Literal(Identifier),
    /// Computed key: `{ ["key"]: value }`
    Expression(Box<Expression>),
}

#[derive(Debug)]
pub enum Binding {
    Identifier(Identifier),
    Pattern(Pattern),
}

#[derive(Debug)]
pub enum Pattern {
    /// An object pattern: `const { a, b, c } = object`
    Object(Box<[ObjectPatternElement]>),
    /// An array pattern: `const [a, b, c] = array`
    Array(Box<[ArrayPatternElement]>),
}

#[derive(Debug)]
pub enum ObjectPatternElement {
    // TODO
}

#[derive(Debug)]
pub enum ArrayPatternElement {
    // TODO
}
