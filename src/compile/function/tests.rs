use crate::{
    ast::{
        source::IntoSpanned, Binding, Declaration, Expression, FunctionBody,
        FunctionCall, FunctionDeclaration, FunctionDefinition,
        FunctionParameter, FunctionPointer, Identifier, ImportDeclaration,
        IntoExpression, IntoStatement, LexicalDeclaration, Literal, Module,
        ObjectLiteral, Statement, Variable,
    },
    compile::{CapturedAst, Compiler, LabelledAst, Lifted},
};
use std::sync::Arc;

// We use vecs in a lot of args here in an attempt to cut down on
// monomorphization copies for the builder methods

/// Test that functions declared with an obvious associated name get
/// labelled with that name.
#[test]
fn function_label() {
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
    let expected_statement_1 = Declaration::lexical(
        "f",
        FunctionDefinition::new(vec![], FunctionBody::empty())
            .with_name("f")
            .into(),
    )
    .into_stmt()
    .s();
    let expected_statement_2 = Statement::Declaration(
        Declaration::Lexical(
            LexicalDeclaration {
                variables: [Variable {
                    binding: Binding::Identifier(Identifier::new("o").s()),
                    init: Some(
                        Expression::Literal(
                            Literal::Object(ObjectLiteral::new(vec![(
                                "g",
                                FunctionDefinition::new(
                                    [],
                                    FunctionBody::empty(),
                                )
                                .with_name("g")
                                .into_expr(),
                            )]))
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
fn function_capture() {
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
            ImportDeclaration::native(None::<Identifier>, vec!["add"], "math")
                .into_stmt()
                .s(),
            FunctionDefinition::new(
                vec![FunctionParameter::identifier("e")],
                FunctionBody::block(vec![Statement::Expression(
                    FunctionCall::new(
                        Expression::reference("console").property("log"),
                        vec![Expression::reference("e")]
                    )
                    .into_expr()
                    .s()
                )])
            )
            .declare("log",)
            .into_stmt()
            .s(),
            Declaration::lexical("x", 2.into()).into_stmt().s(),
            FunctionDefinition::new(
                vec![],
                FunctionBody::block(vec![
                    Declaration::lexical("y", 3.into()).into_stmt(),
                    FunctionDefinition::new(
                        vec![FunctionParameter::identifier("z")],
                        FunctionBody::block(vec![Statement::Expression(
                            FunctionCall::named(
                                "log",
                                vec![FunctionCall::named(
                                    "add",
                                    vec![
                                        Expression::reference("x"),
                                        Expression::reference("y"),
                                        Expression::reference("z"),
                                    ],
                                )
                                .into_expr()]
                            )
                            .into_expr()
                            .s()
                        )]),
                    )
                    // `console` isn't captured because it isn't declared
                    // in any parent scope
                    .with_captures(vec!["log", "add", "x", "y"])
                    .into_expr()
                    .return_()
                ]),
            )
            .with_captures(vec!["log", "add", "x"])
            .declare("f",)
            .into_stmt()
            .s()
        ])
    );
}

/// Test that functions get lifted from the AST to the function table
#[test]
fn function_lift() {
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
        function_table.functions,
        [
            // These are defined inside-out
            FunctionDefinition::new(vec![], FunctionBody::empty()),
            FunctionDefinition::new(
                vec![],
                FunctionBody::block([Statement::Return(Some(
                    Expression::ArrowFunction(
                        FunctionPointer::Lifted(0.into()).s()
                    )
                    .s()
                ))])
            )
            .with_name("g"),
            FunctionDefinition::new(
                vec![],
                FunctionBody::block(vec![FunctionDeclaration {
                    name: Identifier::new("g").s(),
                    pointer: FunctionPointer::Lifted(1.into()).s()
                }
                .into()])
            )
            .with_name("f"),
        ]
        .into_iter()
        .map(Arc::new)
        .collect::<Vec<_>>()
    );
}
