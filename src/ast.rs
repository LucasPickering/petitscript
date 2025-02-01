//! TODO
//! TODO explain why we use Box<[T]> instead of Vec<T>

// TODO comments on everything

/// TODO
#[derive(Clone, Debug)]
pub struct Script {
    pub statements: Box<[Statement]>,
}

#[derive(Clone, Debug)]
pub enum Statement {
    Block(Block),
    Expression(Expression),
    Declaration(Declaration),

    If(If),
    ForLoop(ForLoop),
    ForOfLoop(ForOfLoop),
    WhileLoop(WhileLoop),
    DoWhileLoop(DoWhileLoop),

    Return(Option<Expression>),
    Break(Option<Label>),
    Continue(Option<Label>),

    Import(ImportDeclaration),
    Export(ExportDeclaration),
    // TODO: switch, labels, throw, try, catch, finally
}

/// A collection of statements, delineated by {}. This denotes a new
/// lexical scope.
/// TODO kill this and inline into the enum variant?
#[derive(Clone, Debug)]
pub struct Block {
    pub statements: Box<[Statement]>,
}

#[derive(Clone, Debug)]
pub enum Declaration {
    Lexical(LexicalDeclaration),
    Function(FunctionDeclaration),
}

#[derive(Clone, Debug)]
pub struct LexicalDeclaration {
    pub variables: Box<[Variable]>,
    pub mutable: bool,
}

#[derive(Clone, Debug)]
pub struct Variable {
    pub binding: Binding,
    pub init: Option<Box<Expression>>,
}

#[derive(Clone, Debug)]
pub struct FunctionDeclaration {
    pub name: Option<Identifier>,
    pub parameters: Box<[FunctionParameter]>,
    pub body: Box<[Statement]>,
}

#[derive(Clone, Debug)]
pub struct FunctionParameter {
    pub variable: Variable,
    pub varargs: bool,
}

#[derive(Clone, Debug)]
pub struct If {
    pub condition: Expression,
    pub body: Box<Statement>,
    /// Optional else block. For `else if` blocks, this will be a nested `if`
    pub else_body: Option<Box<Statement>>,
}

#[derive(Clone, Debug)]
pub struct ForLoop {
    pub initializer: Box<Statement>,
    pub condition: Expression,
    pub update: Box<Statement>,
    pub body: Box<Statement>,
}

#[derive(Clone, Debug)]
pub struct ForOfLoop {
    pub binding: Binding,
    pub iterable: Expression,
    pub body: Box<Statement>,
}

#[derive(Clone, Debug)]
pub struct WhileLoop {
    pub condition: Expression,
    pub body: Box<Statement>,
}

#[derive(Clone, Debug)]
pub struct DoWhileLoop {
    pub condition: Expression,
    pub body: Box<Statement>,
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
    Declaration(Declaration),
    DefaultFunctionDeclaration(FunctionDeclaration),
    DefaultExpression(Expression),
}

#[derive(Clone, Debug)]
pub enum Expression {
    Parenthesized(Box<Expression>),
    /// Primitive and complex type literals
    Literal(Literal),
    Template(TemplateLiteral),
    Identifier(Identifier),
    Call(FunctionCall),
    /// The static or dynamic property accessors: `.` or `[]`
    Property(PropertyAccess),
    /// Optional chaining operator: `?.`
    OptionalProperty(OptionalPropertyAccess),
    /// Lambda syntax: `(...) => {...}` or `() => value`
    ArrowFunction(ArrowFunction),
    Unary(UnaryOperation),
    Binary(BinaryOperation),
    Ternary(TernaryConditional),
    Assign(AssignOperation),
    // TODO update operations (++, --)
}

#[derive(Clone, Debug)]
pub struct Identifier(pub String);

impl Identifier {
    pub fn to_str(&self) -> &str {
        &self.0
    }
}

#[derive(Clone, Debug)]
pub struct Label(String);

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
    pub elements: Box<[ArrayElement]>,
}

#[derive(Clone, Debug)]
pub struct ObjectLiteral {
    pub properties: Box<[ObjectProperty]>,
}

#[derive(Clone, Debug)]
pub struct FunctionCall {
    pub function: Box<Expression>,
    pub arguments: Box<[Expression]>,
}

#[derive(Clone, Debug)]
pub struct PropertyAccess {
    pub expression: Box<Expression>,
    pub property: PropertyName,
}

#[derive(Clone, Debug)]
pub struct OptionalPropertyAccess {
    pub expression: Box<Expression>,
    pub property: PropertyName,
}

#[derive(Clone, Debug)]
pub struct ArrowFunction {
    pub parameters: Box<[FunctionParameter]>,
    pub body: ArrowFunctionBody,
}

#[derive(Clone, Debug)]
pub enum ArrowFunctionBody {
    Block(Box<[Statement]>),
    Expression(Box<Expression>),
}

#[derive(Clone, Debug)]
pub struct UnaryOperation {
    pub operator: UnaryOperator,
    pub expression: Box<Expression>,
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
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
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
    pub condition: Box<Expression>,
    pub true_expression: Box<Expression>,
    pub false_expression: Box<Expression>,
}

#[derive(Clone, Debug)]
pub struct AssignOperation {
    pub operator: AssignOperator,
    pub lhs: Binding,
    pub rhs: Box<Expression>,
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
    Coalesce,
    // TODO bitwise operations, exponent
}

#[derive(Clone, Debug)]
pub enum ArrayElement {
    Expression(Expression),
    Spread(Expression),
}

#[derive(Clone, Debug)]
pub enum ObjectProperty {
    /// Normal key value: `{ key: value }` or `{ ["key"]: value }`
    Property {
        property: PropertyName,
        expression: Expression,
    },
    /// Identifier shorthand: `{ name }`
    Identifier(Identifier),
    /// Spread: `{ ...other }`
    Spread(Expression),
}

#[derive(Clone, Debug)]
pub enum PropertyName {
    /// Normal key: `{ key: value }`
    Literal(Identifier),
    /// Computed key: `{ ["key"]: value }`
    Expression(Box<Expression>),
}

#[derive(Clone, Debug)]
pub enum Binding {
    Identifier(Identifier),
    Pattern(Pattern),
}

#[derive(Clone, Debug)]
pub enum Pattern {
    /// An object pattern: `const { a, b, c } = object`
    Object(Box<[ObjectPatternElement]>),
    /// An array pattern: `const [a, b, c] = array`
    Array(Box<[ArrayPatternElement]>),
}

#[derive(Clone, Debug)]
pub enum ObjectPatternElement {
    // TODO
}

#[derive(Clone, Debug)]
pub enum ArrayPatternElement {
    // TODO
}
