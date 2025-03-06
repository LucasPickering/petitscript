use crate::{
    ast::{
        source::IntoSpanned, Ast, Binding, Declaration, Expression,
        FunctionDeclaration, FunctionDefinition, FunctionPointer, Identifier,
        LexicalDeclaration, Literal, ObjectLiteral, ObjectProperty,
        PropertyName, Statement, Variable,
    },
    compile::{Compiler, LabelledAst, Lifted},
};

/// Test that functions declared with an obvious associated name get
/// labelled with that name.
#[test]
fn test_function_label() {
    let LabelledAst(ast) = Compiler::new(
        "
        const f = () => {};
        const o = {g: () => {}};
        // TODO make this work (param doesn't parse)
        //function param(h = () => {}) {}
        ",
    )
    .parse()
    .unwrap()
    .label()
    .program;
    let expected_statement_1 = Statement::Declaration(
        Declaration::Lexical(
            LexicalDeclaration {
                variables: [Variable {
                    binding: Binding::Identifier(Identifier::new("f").s()),
                    init: Some(
                        Expression::ArrowFunction(
                            FunctionPointer::Inline(
                                FunctionDefinition {
                                    name: Some(Identifier::new("f").s()),
                                    parameters: [].into(),
                                    body: [].into(),
                                    captures: [].into(),
                                }
                                .s(),
                            )
                            .s(),
                        )
                        .s()
                        .into(),
                    ),
                }
                .s()]
                .into(),
            }
            .s(),
        )
        .s(),
    );
    let expected_statement_2 = Statement::Declaration(
        Declaration::Lexical(
            LexicalDeclaration {
                variables: [Variable {
                    binding: Binding::Identifier(Identifier::new("o").s()),
                    init: Some(
                        Expression::Literal(
                            Literal::Object(ObjectLiteral {
                                properties: [ObjectProperty::Property {
                                    property: PropertyName::Literal(
                                        Identifier::new("g").s(),
                                    )
                                    .s(),

                                    expression: Expression::ArrowFunction(
                                        FunctionPointer::Inline(
                                            FunctionDefinition {
                                                name: Some(
                                                    Identifier::new("g").s(),
                                                ),
                                                parameters: [].into(),
                                                body: [].into(),
                                                captures: [].into(),
                                            }
                                            .s(),
                                        )
                                        .s(),
                                    )
                                    .s(),
                                }
                                .s()]
                                .into(),
                            })
                            .s(),
                        )
                        .s()
                        .into(),
                    ),
                }
                .s()]
                .into(),
            }
            .s(),
        )
        .s(),
    );

    assert_eq!(
        ast,
        Ast {
            statements: [expected_statement_1.s(), expected_statement_2.s()]
                .into(),
        }
    );
}

/// Test that functions get lifted from the AST to the function table
#[test]
fn test_function_lift() {
    let Lifted {
        ast,
        function_table,
    } = Compiler::new(
        // TODO include a function in param position here: (i = () => {}) => {}
        "
        function f() {
            function g() {
                return () => {};
            }
        }
        ",
    )
    .parse()
    .unwrap()
    .label()
    .capture()
    .lift()
    .program;

    assert_eq!(
        ast,
        Ast {
            statements: [Statement::Declaration(
                Declaration::Function(
                    FunctionDeclaration {
                        name: Identifier::new("f").s(),
                        // TODO ID should be 2
                        pointer: FunctionPointer::Lifted(2.into())
                    }
                    .s()
                )
                .s()
            )
            .s()]
            .into(),
        }
    );

    assert_eq!(
        &function_table.functions,
        &[
            // These are defined inside-out
            FunctionDefinition {
                name: None,
                parameters: [].into(),
                body: [].into(),
                captures: [].into(),
            }
            .into(),
            FunctionDefinition {
                name: Some(Identifier::new("g").s()),
                parameters: [].into(),
                body: [Statement::Return(Some(
                    Expression::ArrowFunction(
                        FunctionPointer::Lifted(0.into()).s()
                    )
                    .s()
                ))
                .s()]
                .into(),
                captures: [].into(),
            }
            .into(),
            FunctionDefinition {
                name: Some(Identifier::new("f").s()),
                parameters: [].into(),
                body: [Statement::Declaration(
                    Declaration::Function(
                        FunctionDeclaration {
                            name: Identifier::new("g").s(),
                            pointer: FunctionPointer::Lifted(1.into())
                        }
                        .s()
                    )
                    .s()
                )
                .s()]
                .into(),
                captures: [].into(),
            }
            .into(),
        ]
    );
}
