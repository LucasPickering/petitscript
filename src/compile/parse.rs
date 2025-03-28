//! Parse some source code via the rslint parser, than convert from their lazy
//! AST to our own types

use crate::{
    ast::{
        source::{IntoSpanned, Span, Spanned},
        ArrayElement, ArrayLiteral, Ast, BinaryOperation, BinaryOperator,
        Binding, Block, Declaration, ExportDeclaration, Expression,
        FunctionCall, FunctionDeclaration, FunctionDefinition,
        FunctionParameter, FunctionPointer, Identifier, ImportDeclaration,
        LexicalDeclaration, Literal, ObjectLiteral, ObjectPatternElement,
        ObjectProperty, PropertyAccess, PropertyName, Statement, TemplateChunk,
        TemplateLiteral, Variable,
    },
    error::{Error, ParseError, TransformError},
    Source,
};
use rslint_parser::{
    ast::{self as ext},
    AstNode, TextRange,
};
use std::any;

/// Parse source code into an Abstract Syntax Tree
pub fn parse(source: &dyn Source) -> Result<Ast, Error> {
    let code = source.text()?;
    let ast = rslint_parser::parse_module(&code, 0)
        .ok()
        .map_err(|errors| ParseError {
            source_name: source.name().map(String::from),
            errors,
        })?;
    ast.transform().map_err(|error| Error::Transform {
        error: error.data,
        span: error.span.qualify(source),
    })
}

/// Transform an rslint AST node to a local AST node
trait Transform {
    type Output;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>>;
}

impl Transform for ext::Module {
    type Output = Ast;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        let statements = self.items().transform_all()?;
        Ok(Ast { statements })
    }
}

impl Transform for ext::ModuleItem {
    type Output = Statement;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            Self::ImportDecl(import_decl) => {
                import_decl.transform_spanned().map(Statement::Import)
            }
            Self::ExportDefaultDecl(_) => todo!(),
            Self::ExportDefaultExpr(export_default_expr) => export_default_expr
                .transform_spanned()
                .map(Statement::Export),
            Self::ExportDecl(export_decl) => {
                export_decl.transform_spanned().map(Statement::Export)
            }
            Self::ExportNamed(_) => todo!(),
            Self::ExportWildcard(_) => todo!(),

            Self::Stmt(stmt) => stmt.transform(),

            Self::TsImportEqualsDecl(node) => unsupported_ts(node.range()),
            Self::TsExportAssignment(node) => unsupported_ts(node.range()),
            Self::TsNamespaceExportDecl(node) => unsupported_ts(node.range()),
        }
    }
}

impl Transform for ext::ImportDecl {
    type Output = ImportDeclaration;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        todo!()
    }
}

impl Transform for ext::ExportDecl {
    type Output = ExportDeclaration;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        self.decl()
            .required(self.range())?
            .transform_spanned()
            .map(ExportDeclaration::Declaration)
    }
}

impl Transform for ext::ExportDefaultExpr {
    type Output = ExportDeclaration;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        self.expr()
            .required(self.range())?
            .transform_spanned()
            .map(ExportDeclaration::DefaultExpression)
    }
}

impl Transform for ext::Stmt {
    type Output = Statement;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            // Things that can return None
            Self::EmptyStmt(_) => Ok(Statement::Empty),
            Self::ExprStmt(expr_stmt) => expr_stmt
                .expr()
                .required(expr_stmt.range())?
                .transform_spanned()
                .map(Statement::Expression),

            // Things that definitely can't return None
            Self::BlockStmt(block_stmt) => {
                block_stmt.transform_spanned().map(Statement::Block)
            }
            Self::IfStmt(node) => unsupported(node.range(), "TODO", "TODO"),
            Self::DoWhileStmt(node) => {
                unsupported(node.range(), "TODO", "TODO")
            }
            Self::WhileStmt(node) => unsupported(node.range(), "TODO", "TODO"),
            Self::ForStmt(node) => unsupported(node.range(), "TODO", "TODO"),
            // TODO for..of loop?
            Self::ForInStmt(_) => todo!(),
            // TODO error if labels are present
            Self::ContinueStmt(node) => {
                unsupported(node.range(), "TODO", "TODO")
            }
            Self::BreakStmt(node) => unsupported(node.range(), "TODO", "TODO"),
            Self::ReturnStmt(return_stmt) => {
                let expression = return_stmt.value().transform_spanned_opt()?;
                Ok(Statement::Return(expression))
            }
            Self::LabelledStmt(node) => {
                unsupported(node.range(), "TODO", "TODO")
            }
            Self::SwitchStmt(_) => todo!(),
            Self::ThrowStmt(_) => todo!(),
            Self::TryStmt(_) => todo!(),
            Self::Decl(decl) => {
                decl.transform_spanned().map(Statement::Declaration)
            }
            Self::DebuggerStmt(node) => {
                unsupported(node.range(), "TODO", "TODO")
            }
            Self::WithStmt(node) => unsupported(node.range(), "TODO", "TODO"),
        }
    }
}

impl Transform for ext::BlockStmt {
    type Output = Block;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        let statements = self.stmts().transform_all()?;
        Ok(Block { statements })
    }
}

impl Transform for ext::Decl {
    type Output = Declaration;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            Self::VarDecl(var_decl) => {
                var_decl.transform_spanned().map(Declaration::Lexical)
            }
            Self::FnDecl(fn_decl) => {
                fn_decl.transform_spanned().map(Declaration::Function)
            }
            Self::ClassDecl(node) => unsupported(
                node.range(),
                "`class`",
                "Classes are not supported",
            ),
            Self::TsEnum(node) => unsupported_ts(node.range()),
            Self::TsTypeAliasDecl(node) => unsupported_ts(node.range()),
            Self::TsNamespaceDecl(node) => unsupported_ts(node.range()),
            Self::TsModuleDecl(node) => unsupported_ts(node.range()),
            Self::TsInterfaceDecl(node) => unsupported_ts(node.range()),
        }
    }
}

impl Transform for ext::VarDecl {
    type Output = LexicalDeclaration;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        if self.is_var() {
            return unsupported(self.range(), "`var`", "Use `const` instead");
        }
        if self.is_let() {
            // TODO add link to docs explaining immutability
            return unsupported(
                self.range(),
                "`let`",
                "Mutable bindings are not supported; use `const` instead",
            );
        }
        let variables = self.declared().transform_all()?;
        Ok(LexicalDeclaration { variables })
    }
}

impl Transform for ext::Declarator {
    type Output = Variable;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        let binding = self.pattern().required(self.range())?.transform()?;
        let init = self.value().transform_spanned_opt()?.map(Box::new);
        Ok(Variable { binding, init })
    }
}

impl Transform for ext::PatternOrExpr {
    type Output = Binding;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            ext::PatternOrExpr::Pattern(pattern) => pattern.transform(),
            ext::PatternOrExpr::Expr(ext::Expr::NameRef(name_ref)) => {
                name_ref.transform_spanned().map(Binding::Identifier)
            }
            ext::PatternOrExpr::Expr(_) => todo!("how this happen?"),
        }
    }
}

impl Transform for ext::Pattern {
    type Output = Binding;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            Self::SinglePattern(single_pattern) => {
                single_pattern.transform_spanned().map(Binding::Identifier)
            }
            Self::ArrayPattern(_) => todo!(),
            Self::ObjectPattern(object_pattern) => object_pattern
                .elements()
                .transform_all()
                .map(Binding::Object),
            Self::ExprPattern(_) => todo!("what it this?"),
            Self::RestPattern(_) => todo!("not allowed here"),
            Self::AssignPattern(_) => todo!("not allowed here"),
        }
    }
}

impl Transform for ext::SinglePattern {
    type Output = Identifier;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        self.name().required(self.range())?.transform()
    }
}

impl Transform for ext::AssignPattern {
    type Output = (Binding, Expression);

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        let binding = self.key().required(self.range())?.transform()?;
        let expression = self.value().required(self.range())?.transform()?;
        Ok((binding, expression))
    }
}

impl Transform for ext::ObjectPatternProp {
    type Output = ObjectPatternElement;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            Self::AssignPattern(_) => todo!(),
            Self::KeyValuePattern(key_value_pattern) => {
                let key = key_value_pattern
                    .key()
                    .required(key_value_pattern.range())?
                    .transform_spanned()?;
                let value = key_value_pattern
                    .value()
                    .required(key_value_pattern.range())?
                    .transform_spanned()?;
                Ok(ObjectPatternElement::Mapped {
                    key,
                    value,
                    init: None,
                })
            }
            Self::RestPattern(rest_pattern) => {
                let binding = rest_pattern
                    .pat()
                    .required(rest_pattern.range())?
                    .transform_spanned()?;
                Ok(ObjectPatternElement::Rest {
                    binding,
                    init: None,
                })
            }
            Self::SinglePattern(single_pattern) => {
                let identifier = single_pattern.transform_spanned()?;
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

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        Ok(Identifier::new(
            self.ident_token().required(self.range())?.to_string(),
        ))
    }
}

impl Transform for ext::Name {
    type Output = Identifier;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        Ok(Identifier::new(self.text()))
    }
}

impl Transform for ext::FnDecl {
    type Output = FunctionDeclaration;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        // `function f()` is the only supported form; `function()` in invalid
        let name = self.name().required(self.range())?.transform_spanned()?;
        let parameters =
            self.parameters().required(self.range())?.transform()?;
        let body = self.body().required(self.range())?.transform()?.statements;
        let pointer = FunctionPointer::Inline(
            FunctionDefinition {
                name: Some(name.clone()),
                parameters,
                body,
                captures: [].into(), // Will be filled out later
            }
            .into_spanned(self.range().into()),
        );
        Ok(FunctionDeclaration { name, pointer })
    }
}

impl Transform for ext::ArrowExpr {
    type Output = FunctionPointer;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        let parameters = match self.params().required(self.range())? {
            ext::ArrowExprParams::Name(name) => {
                let identifier = name.transform_spanned()?;
                let span = identifier.span;
                let parameter = FunctionParameter {
                    variable: Variable {
                        binding: Binding::Identifier(identifier),
                        init: None,
                    }
                    .into_spanned(span),
                    varargs: false,
                }
                .into_spanned(span);
                Box::new([parameter])
            }
            ext::ArrowExprParams::ParameterList(parameter_list) => {
                parameter_list.transform()?
            }
        };
        let body = match self.body().required(self.range())? {
            ext::ExprOrBlock::Expr(expr) => {
                let expression = expr.transform_spanned()?;
                let span = expression.span;
                Box::new([
                    Statement::Return(Some(expression)).into_spanned(span)
                ])
            }
            ext::ExprOrBlock::Block(block_stmt) => {
                block_stmt.transform()?.statements
            }
        };
        Ok(FunctionPointer::Inline(
            FunctionDefinition {
                name: None,
                parameters,
                body,
                captures: [].into(), // Will be filled out later
            }
            .into_spanned(self.range().into()),
        ))
    }
}

impl Transform for ext::ParameterList {
    type Output = Box<[Spanned<FunctionParameter>]>;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        /// Recursive helper for converting a pattern to a function parameter.
        /// We can't just rely on Pattern::transform because it's used in a few
        /// different places in the AST with different semantics
        fn transform(
            pattern: ext::Pattern,
        ) -> Result<Spanned<FunctionParameter>, Spanned<TransformError>>
        {
            match pattern {
                ext::Pattern::RestPattern(rest_pattern) => {
                    let pattern =
                        rest_pattern.pat().required(rest_pattern.range())?;
                    let mut parameter = transform(pattern)?;
                    parameter.data.varargs = true;
                    Ok(parameter)
                }
                ext::Pattern::AssignPattern(assign_pattern) => {
                    // Param has a default expression. Everything left of the =
                    // is the key, which could be a sub-pattern
                    // TODO why doesn't this work?
                    let pattern = assign_pattern
                        .key()
                        .required(assign_pattern.range())?;
                    let mut parameter = transform(pattern)?;

                    let expression = assign_pattern
                        .value()
                        .required(assign_pattern.range())?
                        .transform_spanned()?;
                    parameter.data.variable.data.init = Some(expression.into());
                    Ok(parameter)
                }
                pattern => {
                    let binding = pattern.transform_spanned()?;
                    Ok(FunctionParameter {
                        variable: Variable {
                            binding: binding.data,
                            init: None,
                        }
                        .into_spanned(binding.span),
                        varargs: false,
                    }
                    .into_spanned(binding.span))
                }
            }
        }

        self.parameters().map(transform).collect::<Result<_, _>>()
    }
}

impl Transform for ext::Expr {
    type Output = Expression;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            Self::Literal(literal) => {
                literal.transform_spanned().map(Expression::Literal)
            }
            Self::Template(template) => {
                template.transform_spanned().map(Expression::Template)
            }
            Self::NameRef(name_ref) => {
                name_ref.transform_spanned().map(Expression::Identifier)
            }
            Self::ArrayExpr(array_expr) => {
                array_expr.transform_spanned().map(|array| {
                    Expression::Literal(
                        Literal::Array(array.data).into_spanned(array.span),
                    )
                })
            }
            Self::ObjectExpr(object_expr) => {
                object_expr.transform_spanned().map(|object| {
                    Expression::Literal(
                        Literal::Object(object.data).into_spanned(object.span),
                    )
                })
            }
            Self::GroupingExpr(grouping_expr) => grouping_expr
                .inner()
                .required(grouping_expr.range())?
                .transform(),
            Self::BracketExpr(bracket_expr) => {
                bracket_expr.transform_spanned().map(Expression::Property)
            }
            Self::DotExpr(dot_expr) => {
                dot_expr.transform_spanned().map(Expression::Property)
            }
            Self::CallExpr(call_expr) => {
                call_expr.transform_spanned().map(Expression::Call)
            }
            Self::UnaryExpr(_) => todo!(),
            Self::BinExpr(bin_expr) => {
                bin_expr.transform_spanned().map(Expression::Binary)
            }
            Self::CondExpr(_) => todo!(),
            Self::AssignExpr(node) => unsupported(node.range(), "TODO", "TODO"),
            Self::SequenceExpr(_) => todo!(),
            Self::ArrowExpr(arrow_expr) => arrow_expr
                .transform_spanned()
                .map(Expression::ArrowFunction),
            Self::FnExpr(_) => todo!("wtf is this?"),

            // Unsupported
            Self::ThisExpr(expr) => {
                unsupported(expr.range(), "`this`", "`this` is not supported")
            }
            Self::NewExpr(node) => {
                unsupported(node.range(), "`new`", "Classes are not supported")
            }
            Self::SuperCall(node) => unsupported(
                node.range(),
                "`super`",
                "Classes are not supported",
            ),
            Self::ClassExpr(_) => todo!(),
            Self::NewTarget(_) => todo!(),
            Self::PrivatePropAccess(_) => todo!(),
            Self::ImportMeta(_) => todo!(),
            Self::ImportCall(node) => unsupported(
                node.range(),
                "`import()`",
                "Use the ES6-style `import` keyword instead",
            ),
            Self::AwaitExpr(node) => unsupported(
                node.range(),
                "`await`",
                "Async-await is not supported",
            ),
            Self::YieldExpr(node) => unsupported(
                node.range(),
                "`yield`",
                "Generators are not supported",
            ),
            Self::TsNonNull(node) => unsupported_ts(node.range()),
            Self::TsAssertion(node) => unsupported_ts(node.range()),
            Self::TsConstAssertion(node) => unsupported_ts(node.range()),
        }
    }
}

impl Transform for ext::Literal {
    type Output = Literal;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        let literal = match self.kind() {
            // TODO undefined literal?
            ext::LiteralKind::Null => Literal::Null,
            ext::LiteralKind::Bool(b) => Literal::Boolean(b),
            ext::LiteralKind::Number(f) => {
                // rslint reports all numbers as floats. We want to convert
                // anything without a '.' back to ints
                if self.syntax().text().contains_char('.') {
                    Literal::Float(f)
                } else {
                    // TODO handle floats that are too big
                    Literal::Int(f as i64)
                }
            }
            ext::LiteralKind::BigInt(int) => Literal::Int(
                int.try_into()
                    .or(unsupported(self.range(), "TODO", "TODO"))?,
            ),
            ext::LiteralKind::String => Literal::String(
                self.inner_string_text().required(self.range())?.to_string(),
            ),
            ext::LiteralKind::Regex => {
                unsupported(self.range(), "TODO", "TODO")?
            }
        };
        Ok(literal)
    }
}

impl Transform for ext::Template {
    type Output = TemplateLiteral;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        // rslint provides the quasis (literal chunks) and elements (expression
        // chunks) separately. We're responsible for piecing them back together
        // in order. Collect them all into one sequence, then sort by their
        // source position
        let mut chunks: Vec<Spanned<TemplateChunk>> = self
            .quasis()
            .map(|quasi| {
                let span = quasi.text_range().into();
                Spanned {
                    data: TemplateChunk::Literal(Spanned {
                        data: quasi.text().to_string(),
                        span,
                    }),
                    span,
                }
            })
            .chain(self.elements().transform_all()?)
            .collect();
        chunks.sort_by_key(|chunk| chunk.span.start_offset);
        Ok(TemplateLiteral {
            chunks: chunks.into(),
        })
    }
}

impl Transform for ext::TemplateElement {
    type Output = TemplateChunk;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        let expression =
            self.expr().required(self.range())?.transform_spanned()?;
        Ok(TemplateChunk::Expression(expression))
    }
}

impl Transform for ext::ArrayExpr {
    type Output = ArrayLiteral;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        let elements = self
            .elements()
            .map(|element| match element {
                ext::ExprOrSpread::Expr(expr) => {
                    let expression = expr.transform_spanned()?;
                    let span = expression.span;
                    Ok(ArrayElement::Expression(expression).into_spanned(span))
                }
                ext::ExprOrSpread::Spread(spread_element) => {
                    let expression = spread_element
                        .element()
                        .required(spread_element.range())?
                        .transform_spanned()?;
                    let span = expression.span;
                    Ok(ArrayElement::Spread(expression).into_spanned(span))
                }
            })
            .collect::<Result<_, _>>()?;
        Ok(ArrayLiteral { elements })
    }
}

impl Transform for ext::ObjectExpr {
    type Output = ObjectLiteral;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        let properties = self.props().transform_all()?;
        Ok(ObjectLiteral { properties })
    }
}

impl Transform for ext::ObjectProp {
    type Output = ObjectProperty;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            Self::LiteralProp(literal_prop) => {
                let property = literal_prop
                    .key()
                    .required(literal_prop.range())?
                    .transform_spanned()?;
                let expression = literal_prop
                    .value()
                    .required(literal_prop.range())?
                    .transform_spanned()?;
                Ok(ObjectProperty::Property {
                    property,
                    expression,
                })
            }
            Self::IdentProp(ident_prop) => {
                let identifier = ident_prop
                    .name()
                    .required(ident_prop.range())?
                    .transform_spanned()?;
                Ok(ObjectProperty::Identifier(identifier))
            }
            Self::SpreadProp(spread_prop) => {
                let expression = spread_prop
                    .value()
                    .required(spread_prop.range())?
                    .transform_spanned()?;
                Ok(ObjectProperty::Spread(expression))
            }
            Self::Method(_) => todo!(),
            Self::InitializedProp(_) => todo!("what is this?"),
            Self::Getter(_) | Self::Setter(_) => {
                todo!("not allowed")
            }
        }
    }
}

impl Transform for ext::PropName {
    type Output = PropertyName;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            ext::PropName::Ident(name) => {
                // {x: ...}
                let identifier = name.transform_spanned()?;
                Ok(PropertyName::Literal(identifier))
            }
            ext::PropName::Computed(computed_property_name) => {
                // {[f()]: ...}
                let expression = computed_property_name
                    .prop()
                    .required(computed_property_name.range())?
                    .transform_spanned()?;
                Ok(PropertyName::Expression(expression.into()))
            }
            ext::PropName::Literal(literal) => {
                // {"123": ...}
                if let ext::LiteralKind::String = literal.kind() {
                    let s = literal
                        .inner_string_text()
                        .required(literal.range())?
                        .to_string();
                    let span = literal.range().into();
                    Ok(PropertyName::Expression(
                        Expression::Literal(
                            Literal::String(s).into_spanned(span),
                        )
                        .into_spanned(span)
                        .into(),
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

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        let expression =
            self.object().required(self.range())?.transform_spanned()?;
        let prop = self.prop().required(self.range())?;
        let property_span = prop.range().into();
        let property = prop.transform_spanned()?;
        Ok(PropertyAccess {
            expression: expression.into(),
            property: PropertyName::Expression(property.into())
                .into_spanned(property_span),
        })
    }
}

impl Transform for ext::DotExpr {
    type Output = PropertyAccess;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        let expression =
            self.object().required(self.range())?.transform_spanned()?;
        let identifier =
            self.prop().required(self.range())?.transform_spanned()?;
        let span = self.range().into();
        Ok(PropertyAccess {
            expression: expression.into(),
            property: PropertyName::Literal(identifier).into_spanned(span),
        })
    }
}

impl Transform for ext::BinExpr {
    type Output = BinaryOperation;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        let operator = self.op().required(self.range())?.transform()?;
        let lhs = self.lhs().required(self.range())?.transform_spanned()?;
        let rhs = self.rhs().required(self.range())?.transform_spanned()?;
        Ok(BinaryOperation {
            operator,
            lhs: lhs.into(),
            rhs: rhs.into(),
        })
    }
}

impl Transform for ext::BinOp {
    type Output = BinaryOperator;

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
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

    fn transform(self) -> Result<Self::Output, Spanned<TransformError>> {
        let arguments = self
            .arguments()
            .required(self.range())?
            .args()
            .transform_all()?;
        let function =
            self.callee().required(self.range())?.transform_spanned()?;
        Ok(FunctionCall {
            function: function.into(),
            arguments,
        })
    }
}

/// An extension of [Transform], for AST items that implement [ext::AstNode]
/// and therefore have source spans attached
/// TODO can we combine this with the base trait?
trait TransformSpanned: Transform {
    fn transform_spanned(
        self,
    ) -> Result<Spanned<Self::Output>, Spanned<TransformError>>;
}

impl<T: ext::AstNode + Transform> TransformSpanned for T {
    fn transform_spanned(
        self,
    ) -> Result<Spanned<Self::Output>, Spanned<TransformError>> {
        let span: Span = self.syntax().text_range().into();
        let data = self.transform()?;
        Ok(Spanned { data, span })
    }
}

/// TODO
trait TransformAll<T: Transform> {
    #[allow(clippy::type_complexity)]
    fn transform_all(
        self,
    ) -> Result<Box<[Spanned<T::Output>]>, Spanned<TransformError>>;
}

impl<T: ext::AstNode + Transform> TransformAll<T> for ext::AstChildren<T> {
    fn transform_all(
        self,
    ) -> Result<Box<[Spanned<T::Output>]>, Spanned<TransformError>> {
        self.map(T::transform_spanned).collect::<Result<_, _>>()
    }
}

trait OptionExt<T> {
    /// Ensure an AST node is present. Rslint has a very forgiving parser that
    /// allows most AST nodes to be missing, because it's designed to work on
    /// incomplete/incorrect code. We need correct code to execute though, so
    /// this method makes it easy to enforce AST nodes are present.
    fn required(
        self,
        parent_text_range: TextRange,
    ) -> Result<T, Spanned<TransformError>>;

    /// Call [TransformSpanned] on an optional AST node
    fn transform_spanned_opt(
        self,
    ) -> Result<Option<Spanned<T::Output>>, Spanned<TransformError>>
    where
        T: TransformSpanned;
}

impl<T> OptionExt<T> for Option<T> {
    fn required(
        self,
        parent_text_range: TextRange,
    ) -> Result<T, Spanned<TransformError>> {
        self.ok_or_else(|| {
            TransformError::Missing {
                expected_type: any::type_name::<T>(),
            }
            .into_spanned(parent_text_range.into())
        })
    }

    fn transform_spanned_opt(
        self,
    ) -> Result<Option<Spanned<T::Output>>, Spanned<TransformError>>
    where
        T: TransformSpanned,
    {
        self.map(T::transform_spanned).transpose()
    }
}

impl From<TextRange> for Span {
    fn from(range: TextRange) -> Self {
        Span::new(range.start().into(), range.end().into())
    }
}

/// Helper for generating an [Unsupported](TransformError::Unsupported) error,
/// wrapped in a `Result` for Added Convenience
fn unsupported<T>(
    text_range: TextRange,
    name: &'static str,
    help: &'static str,
) -> Result<T, Spanned<TransformError>> {
    Err(TransformError::Unsupported { name, help }
        .into_spanned(text_range.into()))
}

/// TODO
fn unsupported_ts<T>(
    text_range: TextRange,
) -> Result<T, Spanned<TransformError>> {
    unsupported(
        text_range,
        "TypeScript",
        "Type annotations and other TypeScript constructs \
            are not supported",
    )
}
