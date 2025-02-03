//! Parse some source code via the rslint parser, than convert from their lazy
//! AST to our own types

use crate::{
    ast::{
        ArrayElement, ArrayLiteral, ArrowFunction, ArrowFunctionBody,
        AssignOperation, AssignOperator, BinaryOperation, BinaryOperator,
        Binding, Block, Declaration, DoWhileLoop, ExportDeclaration,
        Expression, ForLoop, FunctionCall, FunctionDeclaration,
        FunctionParameter, Identifier, If, ImportDeclaration,
        LexicalDeclaration, Literal, ObjectLiteral, ObjectPatternElement,
        ObjectProperty, Program, PropertyAccess, PropertyName, Statement,
        TemplateLiteral, Variable, WhileLoop,
    },
    error::{Error, ParseError, TransformError},
    Source,
};
use rslint_parser::{
    ast::{self as ext},
    AstNode,
};

/// Parse source code into an Abstract Syntax Tree
pub fn parse(source: impl Source) -> Result<Program, Error> {
    let code = source.text()?;
    let ast = rslint_parser::parse_module(&code, 0)
        .ok()
        .map_err(|errors| ParseError {
            source_name: source.name().map(String::from),
            errors,
        })?;
    Ok(ast.transform()?)
}

/// Transform an optional value to an optional output
trait Transform {
    type Output;

    fn transform(self) -> Result<Self::Output, TransformError>;
}

/// Transform a series of AST nodes of the same type
impl<T: ext::AstNode + Transform> Transform for ext::AstChildren<T> {
    type Output = Box<[T::Output]>;

    fn transform(self) -> Result<Self::Output, TransformError> {
        self.map(T::transform).collect::<Result<_, _>>()
    }
}

/// TODO
impl<T: Transform> Transform for Option<T> {
    type Output = Option<T::Output>;

    fn transform(self) -> Result<Self::Output, TransformError> {
        self.map(T::transform).transpose()
    }
}

impl Transform for ext::Module {
    type Output = Program;

    fn transform(self) -> Result<Self::Output, TransformError> {
        let statements = self.items().transform()?;
        Ok(Program { statements })
    }
}

impl Transform for ext::ModuleItem {
    type Output = Statement;

    fn transform(self) -> Result<Self::Output, TransformError> {
        match self {
            Self::ImportDecl(import_decl) => {
                import_decl.transform().map(Statement::Import)
            }
            Self::ExportDefaultDecl(_) => todo!(),
            Self::ExportDefaultExpr(export_default_expr) => {
                let expression =
                    export_default_expr.expr().present()?.transform()?;
                Ok(Statement::Export(ExportDeclaration::DefaultExpression(
                    expression,
                )))
            }
            Self::ExportDecl(export_decl) => {
                let declaration = export_decl.decl().present()?.transform()?;
                Ok(Statement::Export(ExportDeclaration::Declaration(
                    declaration,
                )))
            }
            Self::ExportNamed(_) => todo!(),
            Self::ExportWildcard(_) => todo!(),

            Self::Stmt(stmt) => stmt.transform(),

            Self::TsImportEqualsDecl(_)
            | Self::TsExportAssignment(_)
            | Self::TsNamespaceExportDecl(_) => unsupported_typescript(),
        }
    }
}

impl Transform for ext::ImportDecl {
    type Output = ImportDeclaration;

    fn transform(self) -> Result<Self::Output, TransformError> {
        todo!()
    }
}

impl Transform for ext::Stmt {
    type Output = Statement;

    fn transform(self) -> Result<Self::Output, TransformError> {
        match self {
            // Things that can return None
            Self::EmptyStmt(_) => Ok(Statement::Empty),
            Self::ExprStmt(expr_stmt) => expr_stmt
                .expr()
                .present()?
                .transform()
                .map(Statement::Expression),

            // Things that definitely can't return None
            Self::BlockStmt(block_stmt) => {
                block_stmt.transform().map(Statement::Block)
            }
            Self::IfStmt(if_stmt) => if_stmt.transform().map(Statement::If),
            Self::DoWhileStmt(do_while_stmt) => {
                do_while_stmt.transform().map(Statement::DoWhileLoop)
            }
            Self::WhileStmt(while_stmt) => {
                while_stmt.transform().map(Statement::WhileLoop)
            }
            Self::ForStmt(for_stmt) => {
                for_stmt.transform().map(Statement::ForLoop)
            }
            // TODO for..of loop?
            Self::ForInStmt(_) => todo!(),
            // TODO labels on these
            Self::ContinueStmt(_) => Ok(Statement::Continue(None)),
            Self::BreakStmt(_) => Ok(Statement::Break(None)),
            Self::ReturnStmt(return_stmt) => {
                let expression = return_stmt.value().transform()?;
                Ok(Statement::Return(expression))
            }
            Self::LabelledStmt(_) => todo!(),
            Self::SwitchStmt(_) => todo!(),
            Self::ThrowStmt(_) => todo!(),
            Self::TryStmt(_) => todo!(),
            Self::Decl(decl) => decl.transform().map(Statement::Declaration),
            Self::DebuggerStmt(_) => unsupported("TODO", "TODO"),
            Self::WithStmt(_) => unsupported("TODO", "TODO"),
        }
    }
}

impl Transform for ext::BlockStmt {
    type Output = Block;

    fn transform(self) -> Result<Self::Output, TransformError> {
        let statements = self.stmts().transform()?;
        Ok(Block { statements })
    }
}

impl Transform for ext::IfStmt {
    type Output = If;

    fn transform(self) -> Result<Self::Output, TransformError> {
        let condition = self
            .condition()
            .present()?
            .condition()
            .present()?
            .transform()?;
        // THis body is not optional
        let body = self.cons().present()?.transform()?;
        // This body IS optional
        let else_body = self.alt().transform()?;
        Ok(If {
            condition,
            body: body.into(),
            else_body: else_body.map(Box::new),
        })
    }
}

impl Transform for ext::DoWhileStmt {
    type Output = DoWhileLoop;

    fn transform(self) -> Result<Self::Output, TransformError> {
        todo!()
    }
}

impl Transform for ext::WhileStmt {
    type Output = WhileLoop;

    fn transform(self) -> Result<Self::Output, TransformError> {
        todo!()
    }
}

impl Transform for ext::ForStmt {
    type Output = ForLoop;

    fn transform(self) -> Result<Self::Output, TransformError> {
        todo!()
    }
}

impl Transform for ext::Decl {
    type Output = Declaration;

    fn transform(self) -> Result<Self::Output, TransformError> {
        match self {
            Self::VarDecl(var_decl) => {
                var_decl.transform().map(Declaration::Lexical)
            }
            Self::FnDecl(fn_decl) => {
                fn_decl.transform().map(Declaration::Function)
            }
            Self::ClassDecl(_) => {
                unsupported("`class`", "Classes are not supported")
            }
            Self::TsEnum(_)
            | Self::TsTypeAliasDecl(_)
            | Self::TsNamespaceDecl(_)
            | Self::TsModuleDecl(_)
            | Self::TsInterfaceDecl(_) => unsupported_typescript(),
        }
    }
}

impl Transform for ext::VarDecl {
    type Output = LexicalDeclaration;

    fn transform(self) -> Result<Self::Output, TransformError> {
        if self.is_var() {
            return unsupported("`var`", "Use `const` or `let` instead");
        }
        let mutable = self.is_let();
        let variables = self.declared().transform()?;
        Ok(LexicalDeclaration { variables, mutable })
    }
}

impl Transform for ext::Declarator {
    type Output = Variable;

    fn transform(self) -> Result<Self::Output, TransformError> {
        let binding = self.pattern().present()?.transform()?;
        let init = self.value().transform()?.map(Box::new);
        Ok(Variable { binding, init })
    }
}

impl Transform for ext::PatternOrExpr {
    type Output = Binding;

    fn transform(self) -> Result<Self::Output, TransformError> {
        match self {
            ext::PatternOrExpr::Pattern(pattern) => pattern.transform(),
            ext::PatternOrExpr::Expr(ext::Expr::NameRef(name_ref)) => {
                name_ref.transform().map(Binding::Identifier)
            }
            ext::PatternOrExpr::Expr(_) => todo!("how this happen?"),
        }
    }
}

impl Transform for ext::Pattern {
    type Output = Binding;

    fn transform(self) -> Result<Self::Output, TransformError> {
        match self {
            Self::SinglePattern(single_pattern) => {
                single_pattern.transform().map(Binding::Identifier)
            }
            Self::ArrayPattern(_) => todo!(),
            Self::ObjectPattern(object_pattern) => {
                object_pattern.elements().transform().map(Binding::Object)
            }
            Self::ExprPattern(_) => todo!("what it this?"),
            Self::RestPattern(_) => todo!("not allowed here"),
            Self::AssignPattern(_) => todo!("not allowed here"),
        }
    }
}

impl Transform for ext::SinglePattern {
    type Output = Identifier;

    fn transform(self) -> Result<Self::Output, TransformError> {
        self.name().present()?.transform()
    }
}

impl Transform for ext::AssignPattern {
    type Output = (Binding, Expression);

    fn transform(self) -> Result<Self::Output, TransformError> {
        let binding = self.key().present()?.transform()?;
        let expression = self.value().present()?.transform()?;
        Ok((binding, expression))
    }
}

impl Transform for ext::ObjectPatternProp {
    type Output = ObjectPatternElement;

    fn transform(self) -> Result<Self::Output, TransformError> {
        match self {
            Self::AssignPattern(_) => todo!(),
            Self::KeyValuePattern(key_value_pattern) => {
                let key = key_value_pattern.key().present()?.transform()?;
                let value = key_value_pattern.value().present()?.transform()?;
                Ok(ObjectPatternElement::Mapped {
                    key,
                    value,
                    init: None,
                })
            }
            Self::RestPattern(rest_pattern) => {
                let binding = rest_pattern.pat().present()?.transform()?;
                Ok(ObjectPatternElement::Rest {
                    binding,
                    init: None,
                })
            }
            Self::SinglePattern(single_pattern) => {
                let identifier = single_pattern.transform()?;
                Ok(ObjectPatternElement::Identifier {
                    identifier,
                    init: None,
                })
            }
        }
    }
}

impl Transform for ext::NameRef {
    type Output = Identifier;

    fn transform(self) -> Result<Self::Output, TransformError> {
        Ok(Identifier(self.ident_token().present()?.to_string()))
    }
}

impl Transform for ext::Name {
    type Output = Identifier;

    fn transform(self) -> Result<Self::Output, TransformError> {
        Ok(Identifier(self.text()))
    }
}

impl Transform for ext::FnDecl {
    type Output = FunctionDeclaration;

    fn transform(self) -> Result<Self::Output, TransformError> {
        let name = self.name().transform()?;
        let parameters = self.parameters().present()?.transform()?;
        let body = self.body().present()?.transform()?.statements;
        Ok(FunctionDeclaration {
            name,
            parameters,
            body,
        })
    }
}

impl Transform for ext::ArrowExpr {
    type Output = ArrowFunction;

    fn transform(self) -> Result<Self::Output, TransformError> {
        let parameters = match self.params().present()? {
            ext::ArrowExprParams::Name(name) => {
                let identifier = name.transform()?;
                let parameter = FunctionParameter {
                    variable: Variable {
                        binding: Binding::Identifier(identifier),
                        init: None,
                    },
                    varargs: false,
                };
                Box::new([parameter])
            }
            ext::ArrowExprParams::ParameterList(parameter_list) => {
                parameter_list.transform()?
            }
        };
        let body = match self.body().present()? {
            ext::ExprOrBlock::Expr(expr) => {
                ArrowFunctionBody::Expression(expr.transform()?.into())
            }
            ext::ExprOrBlock::Block(block_stmt) => {
                ArrowFunctionBody::Block(block_stmt.transform()?.statements)
            }
        };
        Ok(ArrowFunction { parameters, body })
    }
}

impl Transform for ext::ParameterList {
    type Output = Box<[FunctionParameter]>;

    fn transform(self) -> Result<Self::Output, TransformError> {
        /// Recursive helper for converting a pattern to a function parameter.
        /// We can't just rely on Pattern::transform because it's used in a few
        /// different places in the AST with different semantics
        fn transform(
            pattern: ext::Pattern,
        ) -> Result<FunctionParameter, TransformError> {
            match pattern {
                ext::Pattern::RestPattern(rest_pattern) => {
                    let pattern = rest_pattern.pat().present()?;
                    let mut parameter = transform(pattern)?;
                    parameter.varargs = true;
                    Ok(parameter)
                }
                ext::Pattern::AssignPattern(assign_pattern) => {
                    let pattern = assign_pattern.key().present()?;
                    let mut parameter = transform(pattern)?;
                    let expression =
                        assign_pattern.value().present()?.transform()?;
                    parameter.variable.init = Some(expression.into());
                    Ok(parameter)
                }
                pattern => {
                    let binding = pattern.transform()?;
                    Ok(FunctionParameter {
                        variable: Variable {
                            binding,
                            init: None,
                        },
                        varargs: false,
                    })
                }
            }
        }

        self.parameters().map(transform).collect::<Result<_, _>>()
    }
}

impl Transform for ext::Expr {
    type Output = Expression;

    fn transform(self) -> Result<Self::Output, TransformError> {
        match self {
            Self::Literal(literal) => {
                literal.transform().map(Expression::Literal)
            }
            Self::Template(template) => {
                template.transform().map(Expression::Template)
            }
            Self::NameRef(name_ref) => {
                name_ref.transform().map(Expression::Identifier)
            }
            Self::ArrayExpr(array_expr) => array_expr
                .transform()
                .map(|array| Expression::Literal(Literal::Array(array))),
            Self::ObjectExpr(object_expr) => object_expr
                .transform()
                .map(|object| Expression::Literal(Literal::Object(object))),
            Self::GroupingExpr(grouping_expr) => {
                grouping_expr.inner().present()?.transform()
            }
            Self::BracketExpr(bracket_expr) => {
                bracket_expr.transform().map(Expression::Property)
            }
            Self::DotExpr(dot_expr) => {
                dot_expr.transform().map(Expression::Property)
            }
            Self::CallExpr(call_expr) => {
                call_expr.transform().map(Expression::Call)
            }
            Self::UnaryExpr(_) => todo!(),
            Self::BinExpr(bin_expr) => {
                bin_expr.transform().map(Expression::Binary)
            }
            Self::CondExpr(_) => todo!(),
            Self::AssignExpr(assign_expr) => {
                assign_expr.transform().map(Expression::Assign)
            }
            Self::SequenceExpr(_) => todo!(),
            Self::ArrowExpr(arrow_expr) => {
                arrow_expr.transform().map(Expression::ArrowFunction)
            }
            Self::FnExpr(_) => todo!("wtf is this?"),

            // Unsupported
            Self::ThisExpr(_) => {
                unsupported("`this`", "`this` is not supported")
            }
            Self::NewExpr(_) => {
                unsupported("`new`", "Classes are not supported")
            }
            Self::SuperCall(_) => {
                unsupported("`super`", "Classes are not supported")
            }
            Self::ClassExpr(_) => todo!(),
            Self::NewTarget(_) => todo!(),
            Self::PrivatePropAccess(_) => todo!(),
            Self::ImportMeta(_) => todo!(),
            Self::ImportCall(_) => unsupported(
                "`import()`",
                "Use the ES6-style `import` keyword instead",
            ),
            Self::AwaitExpr(_) => {
                unsupported("`await`", "Async-await is not supported")
            }
            Self::YieldExpr(_) => {
                unsupported("`yield`", "Generators are not supported")
            }
            Self::TsNonNull(_)
            | Self::TsAssertion(_)
            | Self::TsConstAssertion(_) => unsupported_typescript(),
        }
    }
}

impl Transform for ext::Literal {
    type Output = Literal;

    fn transform(self) -> Result<Self::Output, TransformError> {
        let literal = match self.kind() {
            // TODO undefined literal?
            ext::LiteralKind::Null => Literal::Null,
            ext::LiteralKind::Bool(b) => Literal::Boolean(b),
            ext::LiteralKind::Number(f) => Literal::Float(f),
            ext::LiteralKind::BigInt(int) => {
                Literal::Int(int.try_into().or(unsupported("TODO", "TODO"))?)
            }
            ext::LiteralKind::String => Literal::String(
                self.inner_string_text()
                    .ok_or(TransformError::Missing)?
                    .to_string(),
            ),
            ext::LiteralKind::Regex => unsupported("TODO", "TODO")?,
        };
        Ok(literal)
    }
}

impl Transform for ext::Template {
    type Output = TemplateLiteral;

    fn transform(self) -> Result<Self::Output, TransformError> {
        Ok(TemplateLiteral {})
    }
}

impl Transform for ext::ArrayExpr {
    type Output = ArrayLiteral;

    fn transform(self) -> Result<Self::Output, TransformError> {
        let elements = self
            .elements()
            .map(|element| match element {
                ext::ExprOrSpread::Expr(expr) => {
                    Ok(ArrayElement::Expression(expr.transform()?))
                }
                ext::ExprOrSpread::Spread(spread_element) => {
                    let expression =
                        spread_element.element().present()?.transform()?;
                    Ok(ArrayElement::Spread(expression))
                }
            })
            .collect::<Result<_, _>>()?;
        Ok(ArrayLiteral { elements })
    }
}

impl Transform for ext::ObjectExpr {
    type Output = ObjectLiteral;

    fn transform(self) -> Result<Self::Output, TransformError> {
        let properties = self.props().transform()?;
        Ok(ObjectLiteral { properties })
    }
}

impl Transform for ext::ObjectProp {
    type Output = ObjectProperty;

    fn transform(self) -> Result<Self::Output, TransformError> {
        match self {
            Self::LiteralProp(literal_prop) => {
                let property = literal_prop.key().present()?.transform()?;
                let expression = literal_prop.value().present()?.transform()?;
                Ok(ObjectProperty::Property {
                    property,
                    expression,
                })
            }
            Self::IdentProp(ident_prop) => {
                let identifier = ident_prop.name().present()?.transform()?;
                Ok(ObjectProperty::Identifier(identifier))
            }
            Self::SpreadProp(spread_prop) => {
                let expression = spread_prop.value().present()?.transform()?;
                Ok(ObjectProperty::Spread(expression))
            }
            Self::InitializedProp(_) => todo!("what is this?"),
            Self::Getter(_) | Self::Setter(_) | Self::Method(_) => {
                todo!("not allowed")
            }
        }
    }
}

impl Transform for ext::PropName {
    type Output = PropertyName;

    fn transform(self) -> Result<Self::Output, TransformError> {
        match self {
            ext::PropName::Ident(name) => {
                let identifier = name.transform()?;
                Ok(PropertyName::Literal(identifier))
            }
            ext::PropName::Computed(computed_property_name) => {
                let expression =
                    computed_property_name.prop().present()?.transform()?;
                Ok(PropertyName::Expression(expression.into()))
            }
            ext::PropName::Literal(literal) => {
                if let ext::LiteralKind::String = literal.kind() {
                    let s = literal.inner_string_text().present()?.to_string();
                    Ok(PropertyName::Expression(
                        Expression::Literal(Literal::String(s)).into(),
                    ))
                } else {
                    todo!("not allowed?")
                }
            }
        }
    }
}

impl Transform for ext::BracketExpr {
    type Output = PropertyAccess;

    fn transform(self) -> Result<Self::Output, TransformError> {
        let expression = self.object().present()?.transform()?;
        let property = self.prop().present()?.transform()?;
        Ok(PropertyAccess {
            expression: expression.into(),
            property: PropertyName::Expression(property.into()),
        })
    }
}

impl Transform for ext::DotExpr {
    type Output = PropertyAccess;

    fn transform(self) -> Result<Self::Output, TransformError> {
        let expression = self.object().present()?.transform()?;
        let identifier = self.prop().present()?.transform()?;
        Ok(PropertyAccess {
            expression: expression.into(),
            property: PropertyName::Literal(identifier),
        })
    }
}

impl Transform for ext::BinExpr {
    type Output = BinaryOperation;

    fn transform(self) -> Result<Self::Output, TransformError> {
        let operator = self.op().present()?.transform()?;
        let lhs = self.lhs().present()?.transform()?;
        let rhs = self.rhs().present()?.transform()?;
        Ok(BinaryOperation {
            operator,
            lhs: lhs.into(),
            rhs: rhs.into(),
        })
    }
}

impl Transform for ext::AssignExpr {
    type Output = AssignOperation;

    fn transform(self) -> Result<Self::Output, TransformError> {
        let operator = self.op().present()?.transform()?;
        let lhs = self.lhs().present()?.transform()?;
        let rhs = self.rhs().present()?.transform()?;
        Ok(AssignOperation {
            operator,
            lhs,
            rhs: rhs.into(),
        })
    }
}

impl Transform for ext::AssignOp {
    type Output = AssignOperator;

    fn transform(self) -> Result<Self::Output, TransformError> {
        let operator = match self {
            Self::Assign => AssignOperator::Assign,
            Self::AddAssign => AssignOperator::Add,
            Self::SubtractAssign => AssignOperator::Sub,
            Self::TimesAssign => AssignOperator::Mul,
            // TODO: there's no /= ??
            Self::RemainderAssign => AssignOperator::Mod,
            Self::LogicalAndAssign => AssignOperator::BooleanAnd,
            Self::LogicalOrAssign => AssignOperator::BooleanOr,
            Self::NullishCoalescingAssign => AssignOperator::NullishCoalesce,

            Self::ExponentAssign => todo!(),
            Self::LeftShiftAssign => todo!(),
            Self::RightShiftAssign => todo!(),
            Self::UnsignedRightShiftAssign => todo!(),
            Self::BitwiseAndAssign => todo!(),
            Self::BitwiseOrAssign => todo!(),
            Self::BitwiseXorAssign => todo!(),
        };
        Ok(operator)
    }
}

impl Transform for ext::BinOp {
    type Output = BinaryOperator;

    fn transform(self) -> Result<Self::Output, TransformError> {
        let operator = match self {
            Self::LessThan => BinaryOperator::LessThan,
            Self::LessThanOrEqual => BinaryOperator::LessThanEqual,
            Self::GreaterThan => BinaryOperator::GreaterThan,
            Self::GreaterThanOrEqual => BinaryOperator::GreaterThanEqual,
            Self::StrictEquality => BinaryOperator::Equal,
            Self::StrictInequality => BinaryOperator::NotEqual,
            Self::Plus => BinaryOperator::Add,
            Self::Minus => BinaryOperator::Sub,
            Self::Times => BinaryOperator::Mul,
            Self::Divide => BinaryOperator::Div,
            Self::Remainder => BinaryOperator::Mod,
            Self::LogicalOr => BinaryOperator::BooleanOr,
            Self::LogicalAnd => BinaryOperator::BooleanAnd,
            Self::NullishCoalescing => BinaryOperator::NullishCoalesce,

            Self::Exponent => todo!(),
            Self::LeftShift => todo!(),
            Self::RightShift => todo!(),
            Self::Equality => todo!(),
            Self::Inequality => todo!(),
            Self::UnsignedRightShift => todo!(),
            Self::BitwiseAnd => todo!(),
            Self::BitwiseOr => todo!(),
            Self::BitwiseXor => todo!(),
            Self::In => todo!(),
            Self::Instanceof => todo!(),
        };
        Ok(operator)
    }
}

impl Transform for ext::CallExpr {
    type Output = FunctionCall;

    fn transform(self) -> Result<Self::Output, TransformError> {
        let arguments = self.arguments().present()?.args().transform()?;
        let function = self.callee().present()?.transform()?;
        Ok(FunctionCall {
            function: function.into(),
            arguments,
        })
    }
}

trait OptionExt<T> {
    fn present(self) -> Result<T, TransformError>;
}

impl<T> OptionExt<T> for Option<T> {
    fn present(self) -> Result<T, TransformError> {
        self.ok_or(TransformError::Missing)
    }
}

/// Helper for generating an [Unsupported](Error::Unsupported) error, wrapped in
/// a `Result` for Added Convenience
fn unsupported<T>(
    name: &'static str,
    help: &'static str,
) -> Result<T, TransformError> {
    Err(TransformError::Unsupported { name, help })
}

/// TODO
fn unsupported_typescript<T>() -> Result<T, TransformError> {
    unsupported(
        "TypeScript",
        "Type annotations and other TypeScript constructs \
            are not supported",
    )
}
