//! Parse some source code via the rslint parser, than convert from their lazy
//! AST to our own types

use crate::{
    ast::{
        ArrayElement, ArrayLiteral, AssignOperation, AssignOperator, Ast,
        BinaryOperation, BinaryOperator, Binding, Block, Declaration,
        DoWhileLoop, ExportDeclaration, Expression, ForLoop, FunctionCall,
        FunctionDeclaration, FunctionDefinition, FunctionParameter,
        FunctionPointer, HashableF64, Identifier, If, ImportDeclaration,
        IntoSpanned, LexicalDeclaration, Literal, ObjectLiteral,
        ObjectPatternElement, ObjectProperty, PropertyAccess, PropertyName,
        Span, Spanned, Statement, TemplateLiteral, Variable, WhileLoop,
    },
    error::{Error, ParseError, TransformError},
    Source,
};
use rslint_parser::{
    ast::{self as ext},
    AstNode, TextRange,
};

/// Parse source code into an Abstract Syntax Tree
pub fn parse(source: impl Source) -> Result<Ast, Error> {
    let code = source.text()?;
    let ast = rslint_parser::parse_module(&code, 0)
        .ok()
        .map_err(|errors| ParseError {
            source_name: source.name().map(String::from),
            errors,
        })?;
    Ok(ast.transform()?)
}

/// Transform an rslint AST node to a local AST node
trait Transform {
    type Output;

    fn transform(self) -> Result<Self::Output, TransformError>;
}

impl Transform for ext::Module {
    type Output = Ast;

    fn transform(self) -> Result<Self::Output, TransformError> {
        let statements = self.items().transform_all()?;
        Ok(Ast { statements })
    }
}

impl Transform for ext::ModuleItem {
    type Output = Statement;

    fn transform(self) -> Result<Self::Output, TransformError> {
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

impl Transform for ext::ExportDecl {
    type Output = ExportDeclaration;

    fn transform(self) -> Result<Self::Output, TransformError> {
        self.decl()
            .present()?
            .transform_spanned()
            .map(ExportDeclaration::Declaration)
    }
}

impl Transform for ext::ExportDefaultExpr {
    type Output = ExportDeclaration;

    fn transform(self) -> Result<Self::Output, TransformError> {
        self.expr()
            .present()?
            .transform_spanned()
            .map(ExportDeclaration::DefaultExpression)
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
                .transform_spanned()
                .map(Statement::Expression),

            // Things that definitely can't return None
            Self::BlockStmt(block_stmt) => {
                block_stmt.transform_spanned().map(Statement::Block)
            }
            Self::IfStmt(if_stmt) => {
                if_stmt.transform_spanned().map(Statement::If)
            }
            Self::DoWhileStmt(do_while_stmt) => do_while_stmt
                .transform_spanned()
                .map(Statement::DoWhileLoop),
            Self::WhileStmt(while_stmt) => {
                while_stmt.transform_spanned().map(Statement::WhileLoop)
            }
            Self::ForStmt(for_stmt) => {
                for_stmt.transform_spanned().map(Statement::ForLoop)
            }
            // TODO for..of loop?
            Self::ForInStmt(_) => todo!(),
            // TODO error if labels are present
            Self::ContinueStmt(_) => Ok(Statement::Continue),
            Self::BreakStmt(_) => Ok(Statement::Break),
            Self::ReturnStmt(return_stmt) => {
                let expression = return_stmt.value().transform_spanned_opt()?;
                Ok(Statement::Return(expression))
            }
            Self::LabelledStmt(_) => unsupported("TODO", "TODO"),
            Self::SwitchStmt(_) => todo!(),
            Self::ThrowStmt(_) => todo!(),
            Self::TryStmt(_) => todo!(),
            Self::Decl(decl) => {
                decl.transform_spanned().map(Statement::Declaration)
            }
            Self::DebuggerStmt(_) => unsupported("TODO", "TODO"),
            Self::WithStmt(_) => unsupported("TODO", "TODO"),
        }
    }
}

impl Transform for ext::BlockStmt {
    type Output = Block;

    fn transform(self) -> Result<Self::Output, TransformError> {
        let statements = self.stmts().transform_all()?;
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
            .transform_spanned()?;
        // This body is NOT optional
        let body = self.cons().present()?.transform_spanned()?;
        // This body IS optional
        let else_body = self.alt().transform_spanned_opt()?;
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
                var_decl.transform_spanned().map(Declaration::Lexical)
            }
            Self::FnDecl(fn_decl) => {
                fn_decl.transform_spanned().map(Declaration::Function)
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
        let variables = self.declared().transform_all()?;
        Ok(LexicalDeclaration { variables, mutable })
    }
}

impl Transform for ext::Declarator {
    type Output = Variable;

    fn transform(self) -> Result<Self::Output, TransformError> {
        let binding = self.pattern().present()?.transform()?;
        let init = self.value().transform_spanned_opt()?.map(Box::new);
        Ok(Variable { binding, init })
    }
}

impl Transform for ext::PatternOrExpr {
    type Output = Binding;

    fn transform(self) -> Result<Self::Output, TransformError> {
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

    fn transform(self) -> Result<Self::Output, TransformError> {
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
                let key =
                    key_value_pattern.key().present()?.transform_spanned()?;
                let value =
                    key_value_pattern.value().present()?.transform_spanned()?;
                Ok(ObjectPatternElement::Mapped {
                    key,
                    value,
                    init: None,
                })
            }
            Self::RestPattern(rest_pattern) => {
                let binding =
                    rest_pattern.pat().present()?.transform_spanned()?;
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

    fn transform(self) -> Result<Self::Output, TransformError> {
        Ok(Identifier::new(self.ident_token().present()?.to_string()))
    }
}

impl Transform for ext::Name {
    type Output = Identifier;

    fn transform(self) -> Result<Self::Output, TransformError> {
        Ok(Identifier::new(self.text()))
    }
}

impl Transform for ext::FnDecl {
    type Output = FunctionDeclaration;

    fn transform(self) -> Result<Self::Output, TransformError> {
        // Name is optional: both `function f()` and `function()` are valid
        let name = self.name().transform_spanned_opt()?;
        let parameters = self.parameters().present()?.transform()?;
        let body = self.body().present()?.transform()?.statements;
        let pointer = FunctionPointer::Inline(
            FunctionDefinition {
                name,
                parameters,
                body,
            }
            .into_spanned(self.range().into()),
        );
        Ok(FunctionDeclaration { pointer })
    }
}

impl Transform for ext::ArrowExpr {
    type Output = FunctionPointer;

    fn transform(self) -> Result<Self::Output, TransformError> {
        let parameters = match self.params().present()? {
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
        let body = match self.body().present()? {
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
            }
            .into_spanned(self.range().into()),
        ))
    }
}

impl Transform for ext::ParameterList {
    type Output = Box<[Spanned<FunctionParameter>]>;

    fn transform(self) -> Result<Self::Output, TransformError> {
        /// Recursive helper for converting a pattern to a function parameter.
        /// We can't just rely on Pattern::transform because it's used in a few
        /// different places in the AST with different semantics
        fn transform(
            pattern: ext::Pattern,
        ) -> Result<Spanned<FunctionParameter>, TransformError> {
            match pattern {
                ext::Pattern::RestPattern(rest_pattern) => {
                    let pattern = rest_pattern.pat().present()?;
                    let mut parameter = transform(pattern)?;
                    parameter.data.varargs = true;
                    Ok(parameter)
                }
                ext::Pattern::AssignPattern(assign_pattern) => {
                    let pattern = assign_pattern.key().present()?;
                    let mut parameter = transform(pattern)?;
                    let expression = assign_pattern
                        .value()
                        .present()?
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

    fn transform(self) -> Result<Self::Output, TransformError> {
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
            Self::GroupingExpr(grouping_expr) => {
                grouping_expr.inner().present()?.transform()
            }
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
            Self::AssignExpr(assign_expr) => {
                assign_expr.transform_spanned().map(Expression::Assign)
            }
            Self::SequenceExpr(_) => todo!(),
            Self::ArrowExpr(arrow_expr) => arrow_expr
                .transform_spanned()
                .map(Expression::ArrowFunction),
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
            ext::LiteralKind::Number(f) => Literal::Float(HashableF64(f)),
            ext::LiteralKind::BigInt(int) => {
                Literal::Int(int.try_into().or(unsupported("TODO", "TODO"))?)
            }
            ext::LiteralKind::String => {
                Literal::String(self.inner_string_text().present()?.to_string())
            }
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
                    let expression = expr.transform_spanned()?;
                    let span = expression.span;
                    Ok(ArrayElement::Expression(expression).into_spanned(span))
                }
                ext::ExprOrSpread::Spread(spread_element) => {
                    let expression = spread_element
                        .element()
                        .present()?
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

    fn transform(self) -> Result<Self::Output, TransformError> {
        let properties = self.props().transform_all()?;
        Ok(ObjectLiteral { properties })
    }
}

impl Transform for ext::ObjectProp {
    type Output = ObjectProperty;

    fn transform(self) -> Result<Self::Output, TransformError> {
        match self {
            Self::LiteralProp(literal_prop) => {
                let property =
                    literal_prop.key().present()?.transform_spanned()?;
                let expression =
                    literal_prop.value().present()?.transform_spanned()?;
                Ok(ObjectProperty::Property {
                    property,
                    expression,
                })
            }
            Self::IdentProp(ident_prop) => {
                let identifier =
                    ident_prop.name().present()?.transform_spanned()?;
                Ok(ObjectProperty::Identifier(identifier))
            }
            Self::SpreadProp(spread_prop) => {
                let expression =
                    spread_prop.value().present()?.transform_spanned()?;
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
                // {x: ...}
                let identifier = name.transform_spanned()?;
                Ok(PropertyName::Literal(identifier))
            }
            ext::PropName::Computed(computed_property_name) => {
                // {[f()]: ...}
                let expression = computed_property_name
                    .prop()
                    .present()?
                    .transform_spanned()?;
                Ok(PropertyName::Expression(expression.into()))
            }
            ext::PropName::Literal(literal) => {
                // {"123": ...}
                if let ext::LiteralKind::String = literal.kind() {
                    let s = literal.inner_string_text().present()?.to_string();
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

    fn transform(self) -> Result<Self::Output, TransformError> {
        let expression = self.object().present()?.transform_spanned()?;
        let prop = self.prop().present()?;
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

    fn transform(self) -> Result<Self::Output, TransformError> {
        let expression = self.object().present()?.transform_spanned()?;
        let identifier = self.prop().present()?.transform_spanned()?;
        let span = self.range().into();
        Ok(PropertyAccess {
            expression: expression.into(),
            property: PropertyName::Literal(identifier).into_spanned(span),
        })
    }
}

impl Transform for ext::BinExpr {
    type Output = BinaryOperation;

    fn transform(self) -> Result<Self::Output, TransformError> {
        let operator = self.op().present()?.transform()?;
        let lhs = self.lhs().present()?.transform_spanned()?;
        let rhs = self.rhs().present()?.transform_spanned()?;
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
        let lhs = self.lhs().present()?.transform_spanned()?;
        let rhs = self.rhs().present()?.transform_spanned()?;
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
        let arguments = self.arguments().present()?.args().transform_all()?;
        let function = self.callee().present()?.transform_spanned()?;
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
    fn transform_spanned(self)
        -> Result<Spanned<Self::Output>, TransformError>;
}

impl<T: ext::AstNode + Transform> TransformSpanned for T {
    fn transform_spanned(
        self,
    ) -> Result<Spanned<Self::Output>, TransformError> {
        let span: Span = self.syntax().text_range().into();
        let data = self.transform()?;
        Ok(Spanned { data, span })
    }
}

/// TODO
trait TransformAll<T: Transform> {
    fn transform_all(self)
        -> Result<Box<[Spanned<T::Output>]>, TransformError>;
}

impl<T: ext::AstNode + Transform> TransformAll<T> for ext::AstChildren<T> {
    fn transform_all(
        self,
    ) -> Result<Box<[Spanned<T::Output>]>, TransformError> {
        self.map(T::transform_spanned).collect::<Result<_, _>>()
    }
}

trait OptionExt<T> {
    /// Ensure an AST node is present. Rslint has a very forgiving parser that
    /// allows most AST nodes to be missing, because it's designed to work on
    /// incomplete/incorrect code. We need correct code to execute though, so
    /// this method makes it easy to enforce AST nodes are present.
    fn present(self) -> Result<T, TransformError>;

    /// Call [TransformSpanned] on an optional AST node
    fn transform_spanned_opt(
        self,
    ) -> Result<Option<Spanned<T::Output>>, TransformError>
    where
        T: TransformSpanned;
}

impl<T> OptionExt<T> for Option<T> {
    fn present(self) -> Result<T, TransformError> {
        self.ok_or(TransformError::Missing)
    }

    fn transform_spanned_opt(
        self,
    ) -> Result<Option<Spanned<T::Output>>, TransformError>
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
