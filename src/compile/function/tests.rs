use crate::{
    ast::{
        source::IntoSpanned, Binding, Declaration, Expression,
        FunctionDeclaration, FunctionDefinition, FunctionParameter,
        FunctionPointer, Identifier, LexicalDeclaration, Literal, Module,
        ObjectLiteral, ObjectProperty, PropertyName, Statement, Variable,
    },
    compile::{CapturedAst, Compiler, LabelledAst, Lifted},
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
    let expected_statement_1 = Statement::const_assign(
        "f",
        Expression::function(Some("f"), vec![], vec![], vec![]),
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

                                    expression: Expression::function(
                                        Some("g"),
                                        vec![],
                                        vec![],
                                        vec![],
                                    ),
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
    )
    .s();

    assert_eq!(
        ast,
        Module {
            statements: [expected_statement_1, expected_statement_2].into(),
        }
    );
}

/// Test closure capturing
#[test]
fn test_function_capture() {
    let CapturedAst(ast) = Compiler::new(
        "
        import { add } from 'math';

        function log(e) {
            console.log(e);
        }

        const x = 2;
        function f() {
            const y = 3;
            return (z) => {
                log(add(x, y, z))
            };
        }
        ",
    )
    .parse()
    .unwrap()
    .label()
    .capture()
    .program;

    assert_eq!(
        ast,
        Module::new(vec![
            Statement::import_native(None, vec!["add"], "math"),
            Statement::function(
                "log",
                vec![FunctionParameter::identifier("e")],
                vec![Statement::Expression(Expression::call(
                    Expression::identifier("console").property("log"),
                    vec![Expression::identifier("e")]
                ))
                .s()],
                vec![]
            ),
            Statement::const_assign("x", Expression::int(2)),
            Statement::function(
                "f",
                vec![],
                vec![
                    Statement::const_assign("y", Expression::int(3)),
                    Statement::Return(Some(Expression::function(
                        None,
                        vec![FunctionParameter::identifier("z")],
                        vec![Statement::Expression(Expression::call(
                            Expression::identifier("log"),
                            vec![Expression::call(
                                Expression::identifier("add"),
                                vec![
                                    Expression::identifier("x"),
                                    Expression::identifier("y"),
                                    Expression::identifier("z"),
                                ],
                            )]
                        ))
                        .s()],
                        // `console` isn't captured because it isn't declared
                        // in any parent scope
                        vec!["log", "add", "x", "y"]
                    )))
                    .s()
                ],
                vec!["log", "add", "x"]
            )
        ])
    );
}

/// Test that functions get lifted from the AST to the function table
#[test]
fn test_function_lift() {
    let Lifted {
        module: ast,
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
        Module {
            statements: [Statement::Declaration(
                Declaration::Function(
                    FunctionDeclaration {
                        name: Identifier::new("f").s(),
                        pointer: FunctionPointer::Lifted(2.into()).s()
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
                            pointer: FunctionPointer::Lifted(1.into()).s()
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
