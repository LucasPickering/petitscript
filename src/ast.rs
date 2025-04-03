//! TODO
//!
//! Collection types all use `Box<[T]>` instead of `Vec<T>` because we know
//! they're all fixed size and won't need to grow. `Box<[T]>` ensures we don't
//! allocate more memory than needed.

#![allow(unused)] // TODO remove

// TODO comments on everything

pub mod source;
pub mod walk;

use crate::{
    ast::source::Spanned, compile::FunctionDefinitionId, error::ModuleNameError,
};
use std::{
    fmt::{self, Display},
    hash::Hash,
    ops::Deref,
    path::PathBuf,
    str::FromStr,
};

/// The root AST node for a single source file. This is the outcome of parsing a
/// file, and may contain nested modules from local imports.
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Module {
    pub statements: Box<[Spanned<Statement>]>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Statement {
    Empty,
    Block(Spanned<Block>),
    Expression(Spanned<Expression>),
    Declaration(Spanned<Declaration>),

    If(Spanned<If>),
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
#[cfg_attr(test, derive(PartialEq))]
pub struct Block {
    pub statements: Box<[Spanned<Statement>]>,
}

/// TODO eliminate this and just use lexical/fn decls directly?
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Declaration {
    Lexical(Spanned<LexicalDeclaration>),
    Function(Spanned<FunctionDeclaration>),
}

/// `const x = 3;` or `const x = 3, y = 4, z = 5;`
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct LexicalDeclaration {
    pub variables: Box<[Spanned<Variable>]>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Variable {
    pub binding: Binding,
    pub init: Option<Box<Spanned<Expression>>>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FunctionDeclaration {
    pub name: Spanned<Identifier>,
    pub pointer: FunctionPointer,
}

/// TODO rename this
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum FunctionPointer {
    /// Lifting hasn't been performed yet, the function definitions is still
    /// inline. This code isn't executable yet!
    Inline(Spanned<FunctionDefinition>),
    /// Function definition has been lifted to the top of the program, and this
    /// is just a pointer to the definition
    Lifted(FunctionDefinitionId),
}

/// TODO
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FunctionDefinition {
    /// A label for this function, to be passed onto the function value. This
    /// is **not necessarily** the name the function is bound to; that is
    /// defined in the parent [FunctionDeclaration].
    pub name: Option<Spanned<Identifier>>,
    pub parameters: Box<[Spanned<FunctionParameter>]>,
    /// We don't use [Block] here because a function's body shares a scope with
    /// its parameters.
    pub body: Box<[Spanned<Statement>]>,
    /// A list of all the identifiers this function captures from its parent
    /// scope. This only includes identifiers that are actually present in the
    /// parent scope. References in the function parameters/body that don't
    /// existing anywhere in scope can't be captured and therefore won't appear
    /// here. They will either be provided by the global scope or trigger a
    /// runtime error.
    pub captures: Box<[Identifier]>,
}

/// One parameter in a function definition
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FunctionParameter {
    pub variable: Spanned<Variable>,
    pub varargs: bool,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct If {
    pub condition: Spanned<Expression>,
    pub body: Box<Spanned<Statement>>,
    /// Optional else block. For `else if` blocks, this will be a nested `if`
    pub else_body: Option<Box<Spanned<Statement>>>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ForOfLoop {
    pub binding: Binding,
    pub iterable: Spanned<Expression>,
    pub body: Box<Spanned<Statement>>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct WhileLoop {
    pub condition: Spanned<Expression>,
    pub body: Box<Spanned<Statement>>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct DoWhileLoop {
    pub condition: Spanned<Expression>,
    pub body: Box<Spanned<Statement>>,
}

/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/import
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum ImportDeclaration {
    /// `import exportDefault, { export1, export2 as ex2 } from "module-name"`
    Named {
        /// `exportDefault`
        default: Option<Spanned<Identifier>>,
        /// `{ export1, export2 as ex2 }`
        named: Box<[Spanned<ImportNamed>]>,
        /// `"module-name"`
        module: Spanned<ImportModule>,
    },
    /// `import * as name from "module-name"`
    Namespace {
        identifier: Spanned<Identifier>,
        module: Spanned<ImportModule>,
    },
    // Side-effect imports not supported - we're a functional lang!!
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
    pub identifier: Spanned<Identifier>,
    /// `ex2`
    pub rename: Option<Spanned<Identifier>>,
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

impl Display for NativeModuleName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

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
    Declaration(Spanned<Declaration>),
    DefaultFunctionDeclaration(Spanned<FunctionDeclaration>),
    DefaultExpression(Spanned<Expression>),
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
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
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Identifier(String);

impl Identifier {
    pub fn new(s: impl Into<String>) -> Self {
        Self(s.into())
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
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
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

/// TODO
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TemplateLiteral {
    /// A set of contiguous chunks that comprise the template. These will be
    /// alternating in variant, e.g. `[Lit, Expr, Lit]` or `[Expr, Lit, Expr]`
    pub chunks: Box<[Spanned<TemplateChunk>]>,
}

/// One piece in a template. Either a static string or an expression
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum TemplateChunk {
    Literal(Spanned<String>),
    Expression(Spanned<Expression>),
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ArrayLiteral {
    pub elements: Box<[Spanned<ArrayElement>]>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum ArrayElement {
    Expression(Spanned<Expression>),
    Spread(Spanned<Expression>),
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ObjectLiteral {
    pub properties: Box<[Spanned<ObjectProperty>]>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
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
#[cfg_attr(test, derive(PartialEq))]
pub struct FunctionCall {
    pub function: Box<Spanned<Expression>>,
    pub arguments: Box<[Spanned<Expression>]>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct PropertyAccess {
    pub expression: Box<Spanned<Expression>>,
    pub property: Spanned<PropertyName>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct OptionalPropertyAccess {
    pub expression: Box<Spanned<Expression>>,
    pub property: Spanned<PropertyName>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct UnaryOperation {
    pub operator: UnaryOperator,
    pub expression: Box<Spanned<Expression>>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum UnaryOperator {
    /// `!`
    BooleanNot,
    /// `-`
    Negate,
    // TODO bitwise operations
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct BinaryOperation {
    pub operator: BinaryOperator,
    pub lhs: Box<Spanned<Expression>>,
    pub rhs: Box<Spanned<Expression>>,
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
    pub condition: Box<Spanned<Expression>>,
    pub true_expression: Box<Spanned<Expression>>,
    pub false_expression: Box<Spanned<Expression>>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum PropertyName {
    /// Normal key: `{ key: value }`
    Literal(Spanned<Identifier>),
    /// Computed key: `{ ["key"]: value }`
    Expression(Box<Spanned<Expression>>),
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Binding {
    /// `const x = 3`
    Identifier(Spanned<Identifier>),
    /// An object pattern: `const { a, b, c } = object`
    Object(Box<[Spanned<ObjectPatternElement>]>),
    /// An array pattern: `const [a, b, c] = array`
    Array(Box<[Spanned<ArrayPatternElement>]>),
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
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
#[cfg_attr(test, derive(PartialEq))]
pub enum ArrayPatternElement {
    // TODO
}

/// Helpers for constructing AST nodes in tests
#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::source::IntoSpanned;

    // These helpers all take `Vec<T>` instead of `impl IntoIterator<Item = T>`
    // to cut down on compile time/code dupe

    impl Module {
        pub fn new(statements: Vec<Spanned<Statement>>) -> Self {
            Self {
                statements: statements.into_iter().collect(),
            }
        }
    }

    impl Statement {
        /// Create a lexical declaration statement: `const x = <expression>`
        pub fn const_assign(
            name: &str,
            expression: Spanned<Expression>,
        ) -> Spanned<Self> {
            Statement::Declaration(
                Declaration::Lexical(
                    LexicalDeclaration {
                        variables: [Variable::identifier(
                            name,
                            Some(expression),
                        )]
                        .into(),
                    }
                    .s(),
                )
                .s(),
            )
            .s()
        }

        /// Create a function declaration: `function f() {}`
        pub fn function(
            name: &str,
            parameters: Vec<Spanned<FunctionParameter>>,
            body: Vec<Spanned<Statement>>,
            captures: Vec<&str>,
        ) -> Spanned<Self> {
            Statement::Declaration(
                Declaration::Function(
                    FunctionDeclaration {
                        name: Identifier::new(name).s(),
                        pointer: FunctionPointer::Inline(
                            FunctionDefinition {
                                name: Some(Identifier::new(name).s()),
                                parameters: parameters.into(),
                                body: body.into(),
                                captures: captures
                                    .into_iter()
                                    .map(Identifier::new)
                                    .collect(),
                            }
                            .s(),
                        ),
                    }
                    .s(),
                )
                .s(),
            )
            .s()
        }
    }

    impl Expression {
        /// Create a simple identifier expression: `x`
        pub fn identifier(name: &str) -> Spanned<Self> {
            Self::Identifier(Identifier::new(name).s()).s()
        }

        /// Create an integer literal
        pub fn int(i: i64) -> Spanned<Self> {
            Self::Literal(Literal::Int(i).s()).s()
        }

        /// Create a lambda expression
        pub fn function(
            name: Option<&str>,
            parameters: Vec<Spanned<FunctionParameter>>,
            body: Vec<Spanned<Statement>>,
            captures: Vec<&str>,
        ) -> Spanned<Self> {
            Self::ArrowFunction(
                FunctionPointer::Inline(
                    FunctionDefinition {
                        name: name.map(|name| Identifier::new(name).s()),
                        parameters: parameters.into(),
                        body: body.into(),
                        captures: captures
                            .into_iter()
                            .map(Identifier::new)
                            .collect(),
                    }
                    .s(),
                )
                .s(),
            )
            .s()
        }

        /// Create a function call expression
        pub fn call(
            function: Spanned<Self>,
            arguments: Vec<Spanned<Self>>,
        ) -> Spanned<Self> {
            Self::Call(
                FunctionCall {
                    function: function.into(),
                    arguments: arguments.into(),
                }
                .s(),
            )
            .s()
        }

        pub fn binary(
            operator: BinaryOperator,
            lhs: Spanned<Self>,
            rhs: Spanned<Self>,
        ) -> Spanned<Self> {
            Self::Binary(
                BinaryOperation {
                    operator,
                    lhs: lhs.into(),
                    rhs: rhs.into(),
                }
                .s(),
            )
            .s()
        }
    }

    impl Spanned<Expression> {
        /// Get a property of this expression: `self.y`
        pub fn property(self, property: &str) -> Self {
            Expression::Property(
                PropertyAccess {
                    expression: self.into(),
                    property: PropertyName::literal(property),
                }
                .s(),
            )
            .s()
        }
    }

    impl Variable {
        /// Create a simple identifier variable
        pub fn identifier(
            name: &str,
            init: Option<Spanned<Expression>>,
        ) -> Spanned<Self> {
            Self {
                binding: Binding::Identifier(Identifier::new(name).s()),
                init: init.map(Box::new),
            }
            .s()
        }
    }

    impl PropertyName {
        pub fn literal(name: &str) -> Spanned<Self> {
            Self::Literal(Identifier::new(name).s()).s()
        }
    }

    impl FunctionParameter {
        /// A simple single-identifier parameter, with no default expression
        pub fn identifier(name: &str) -> Spanned<Self> {
            Self {
                variable: Variable::identifier(name, None),
                varargs: false,
            }
            .s()
        }
    }
}
