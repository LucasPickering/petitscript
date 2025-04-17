//! Utilities for building ASTs programatically

// These helpers all take `Vec<T>` instead of `impl IntoIterator<Item = T>`
// to cut down on compile time/code dupe

use crate::ast::{
    source::Spanned, BinaryOperation, BinaryOperator, Binding, Declaration,
    Expression, FunctionCall, FunctionDeclaration, FunctionDefinition,
    FunctionParameter, FunctionPointer, Identifier, ImportDeclaration,
    ImportModule, ImportNamed, IntoSpanned, LexicalDeclaration, Literal,
    Module, PropertyAccess, PropertyName, Statement, Variable,
};

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
                    variables: [Variable::identifier(name, Some(expression))]
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
        name: impl ToString,
        parameters: Vec<Spanned<FunctionParameter>>,
        body: Vec<Spanned<Statement>>,
        captures: Vec<&str>,
    ) -> Spanned<Self> {
        let name = name.to_string();
        Statement::Declaration(
            Declaration::Function(
                FunctionDeclaration {
                    name: Identifier::new(name.clone()).s(),
                    pointer: FunctionPointer::Inline(FunctionDefinition {
                        name: Some(Identifier::new(name).s()),
                        parameters: parameters.into(),
                        body: body.into(),
                        captures: captures
                            .into_iter()
                            .map(Identifier::new)
                            .collect(),
                    })
                    .s(),
                }
                .s(),
            )
            .s(),
        )
        .s()
    }

    /// Create a native import declaration: `import default, { named } from
    /// 'module'`
    pub fn import_native(
        default: Option<&str>,
        named: Vec<&str>,
        module: &str,
    ) -> Spanned<Self> {
        Self::Import(
            ImportDeclaration {
                default: default.map(|name| Identifier::new(name).s()),
                named: named
                    .into_iter()
                    .map(|name| {
                        ImportNamed {
                            identifier: Identifier::new(name).s(),
                            rename: None,
                        }
                        .s()
                    })
                    .collect(),
                module: ImportModule::Native(module.parse().unwrap()).s(),
            }
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
            FunctionPointer::Inline(FunctionDefinition {
                name: name.map(|name| Identifier::new(name).s()),
                parameters: parameters.into(),
                body: body.into(),
                captures: captures.into_iter().map(Identifier::new).collect(),
            })
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
