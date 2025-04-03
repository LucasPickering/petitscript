//! Parse some source code via the rslint parser, than convert from their lazy
//! AST to our own types

use crate::{
    ast::{
        source::{IntoSpanned, Span, Spanned},
        ArrayElement, ArrayLiteral, BinaryOperation, BinaryOperator, Binding,
        Block, Declaration, DoWhileLoop, ExportDeclaration, Expression,
        FunctionCall, FunctionDeclaration, FunctionDefinition,
        FunctionParameter, FunctionPointer, Identifier, If, ImportDeclaration,
        ImportModule, ImportNamed, LexicalDeclaration, Literal, Module,
        ObjectLiteral, ObjectPatternElement, ObjectProperty, PropertyAccess,
        PropertyName, Statement, TemplateChunk, TemplateLiteral, Variable,
        WhileLoop,
    },
    error::{Error, ParseError, TransformError},
    Source,
};
use rslint_parser::{
    ast::{self as ext},
    AstNode, TextRange,
};
use std::{
    any, env, fs,
    path::{Path, PathBuf},
};

/// File extensions that can be imported as a local module
const SUPPORTED_EXTENSIONS: &[&str] = &["js", "ts"];

/// Parse source code into an Abstract Syntax Tree
pub fn parse(source: &dyn Source) -> Result<Module, Error> {
    let code = source.text()?;
    let ast = rslint_parser::parse_module(&code, 0)
        .ok()
        .map_err(|errors| ParseError {
            source_name: source.name().map(String::from),
            errors,
        })?;
    ast.transform(&ParseContext { source })
        .map_err(|error| Error::Transform {
            error: error.data,
            span: error.span.qualify(source),
        })
}

/// Context object to be passed around to every transformation
struct ParseContext<'a> {
    /// The source of the module currently being parsed
    source: &'a dyn Source,
}

/// Transform an rslint AST node to a local AST node
trait Transform {
    type Output;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>>;
}

impl Transform for ext::Module {
    type Output = Module;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let statements = self.items().transform_all(context)?;
        Ok(Module { statements })
    }
}

impl Transform for ext::ModuleItem {
    type Output = Statement;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            Self::ImportDecl(import_decl) => import_decl
                .transform_spanned(context)
                .map(Statement::Import),
            Self::ExportDefaultDecl(_) => todo!(),
            Self::ExportDefaultExpr(export_default_expr) => export_default_expr
                .transform_spanned(context)
                .map(Statement::Export),
            Self::ExportDecl(export_decl) => export_decl
                .transform_spanned(context)
                .map(Statement::Export),
            Self::ExportNamed(_) => todo!(),
            Self::ExportWildcard(_) => todo!(),

            Self::Stmt(stmt) => stmt.transform(context),

            Self::TsImportEqualsDecl(node) => unsupported_ts(node.range()),
            Self::TsExportAssignment(node) => unsupported_ts(node.range()),
            Self::TsNamespaceExportDecl(node) => unsupported_ts(node.range()),
        }
    }
}

impl Transform for ext::ImportDecl {
    type Output = ImportDeclaration;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        // TODO only allow imports at top level

        if let Some(node) = self.assert_token() {
            // https://v8.dev/features/import-assertions
            unsupported(node.text_range(), "import assertion", "TODO")?;
        }

        // TODO explain
        let mut wildcard: Option<Spanned<Identifier>> = None;
        let mut default: Option<Spanned<Identifier>> = None;
        let mut named: Vec<Spanned<ImportNamed>> = vec![];
        for import in self.imports() {
            match import {
                ext::ImportClause::WildcardImport(wildcard_import) => {
                    let range = wildcard_import.range();
                    let identifier = Identifier::new(
                        wildcard_import
                            .ident_token()
                            .required(range)?
                            .to_string(),
                    );
                    // TODO handle error for duplicate
                    wildcard.replace(identifier.into_spanned(range));
                }
                ext::ImportClause::NamedImports(named_imports) => {
                    named.extend(
                        named_imports.specifiers().transform_all(context)?,
                    );
                }
                ext::ImportClause::Name(name) => {
                    // TODO handle error for duplicate
                    default.replace(name.transform_spanned(context)?);
                }
                ext::ImportClause::ImportStringSpecifier(_) => {
                    todo!("what is this?")
                }
            }
        }

        let source = self.source().required(self.range())?;
        let source_text = source
            .inner_string_text()
            .required(source.range())?
            .to_string();

        // If it's a valid native module name, treat it as such. Otherwise treat
        // it as a path. A path is always going to have a character like . or /
        // that will make it an invalid module name
        let module = match source_text.try_into() {
            Ok(name) => ImportModule::Native(name),
            Err(error) => {
                let ast = resolve_module(error.name, context.source)
                    .and_then(|path| parse(&path))
                    .map_err(|error| {
                        TransformError::Import(error.into())
                            .into_spanned(source.range())
                    })?;
                ImportModule::Local(ast)
            }
        }
        .into_spanned(source.range());

        match (wildcard, default, named.as_slice()) {
            (Some(wildcard), None, &[]) => Ok(ImportDeclaration::Namespace {
                identifier: wildcard,
                module,
            }),
            (Some(_), _, _) => todo!("error"),
            (None, None, &[]) => todo!("error no imports specified"),
            (None, default, _) => Ok(ImportDeclaration::Named {
                default,
                named: named.into(),
                module,
            }),
        }
    }
}

/// Resolve a local module specifier into a path. This will:
/// - Add an extension if it lacks one
/// - Resolve relative paths to absolute, relative to the parent location
/// - Resolve links
fn resolve_module(
    specifier: String,
    parent_source: &dyn Source,
) -> Result<PathBuf, Error> {
    let path: PathBuf = specifier.into();

    if path.is_absolute() {
        todo!("error");
    }

    let cwd = env::current_dir().expect("TODO");
    let root_path = parent_source
        .import_root()
        .and_then(Path::parent)
        .unwrap_or(&cwd);
    let mut path = root_path.join(path);

    if path.extension().is_none() {
        // Path doesn't have an extension - try all supported extensions
        // TODO better error if none match
        for extension in SUPPORTED_EXTENSIONS {
            path.set_extension(extension);
            if fs::exists(&path).unwrap_or(false) {
                break;
            }
        }
    }
    Ok(path)
}

impl Transform for ext::Specifier {
    type Output = ImportNamed;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let identifier =
            Identifier::new(self.name().required(self.range())?.to_string())
                .into_spanned(self.range());
        // Optional `as x` rename
        let rename = self.alias().transform_spanned_opt(context)?;
        Ok(ImportNamed { identifier, rename })
    }
}

impl Transform for ext::ExportDecl {
    type Output = ExportDeclaration;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        // TODO only allow exports at top level

        self.decl()
            .required(self.range())?
            .transform_spanned(context)
            .map(ExportDeclaration::Declaration)
    }
}

impl Transform for ext::ExportDefaultExpr {
    type Output = ExportDeclaration;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        self.expr()
            .required(self.range())?
            .transform_spanned(context)
            .map(ExportDeclaration::DefaultExpression)
    }
}

impl Transform for ext::Stmt {
    type Output = Statement;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            // Things that can return None
            Self::EmptyStmt(_) => Ok(Statement::Empty),
            Self::ExprStmt(expr_stmt) => expr_stmt
                .expr()
                .required(expr_stmt.range())?
                .transform_spanned(context)
                .map(Statement::Expression),

            // Things that definitely can't return None
            Self::BlockStmt(block_stmt) => {
                block_stmt.transform_spanned(context).map(Statement::Block)
            }
            Self::IfStmt(if_stmt) => {
                if_stmt.transform_spanned(context).map(Statement::If)
            }
            Self::DoWhileStmt(do_while_stmt) => do_while_stmt
                .transform_spanned(context)
                .map(Statement::DoWhileLoop),
            Self::WhileStmt(while_stmt) => while_stmt
                .transform_spanned(context)
                .map(Statement::WhileLoop),
            Self::ForStmt(node) => unsupported(node.range(), "TODO", "TODO"),
            // TODO for..of loop?
            Self::ForInStmt(_) => todo!(),
            // TODO error if labels are present
            Self::ContinueStmt(_) => Ok(Statement::Continue),
            Self::BreakStmt(_) => Ok(Statement::Break),
            Self::ReturnStmt(return_stmt) => {
                let expression =
                    return_stmt.value().transform_spanned_opt(context)?;
                Ok(Statement::Return(expression))
            }
            Self::LabelledStmt(node) => {
                unsupported(node.range(), "TODO", "TODO")
            }
            Self::SwitchStmt(_) => todo!(),
            Self::ThrowStmt(_) => todo!(),
            Self::TryStmt(_) => todo!(),
            Self::Decl(decl) => {
                decl.transform_spanned(context).map(Statement::Declaration)
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

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let statements = self.stmts().transform_all(context)?;
        Ok(Block { statements })
    }
}

impl Transform for ext::IfStmt {
    type Output = If;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let condition = self
            .condition()
            .required(self.range())?
            .condition()
            .required(self.range())?
            .transform_spanned(context)?;
        // This body is NOT optional
        let body = self
            .cons()
            .required(self.range())?
            .transform_spanned(context)?;
        // This body IS optional
        let else_body = self.alt().transform_spanned_opt(context)?;
        Ok(If {
            condition,
            body: body.into(),
            else_body: else_body.map(Box::new),
        })
    }
}

impl Transform for ext::DoWhileStmt {
    type Output = DoWhileLoop;

    fn transform(
        self,
        _: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        todo!()
    }
}

impl Transform for ext::WhileStmt {
    type Output = WhileLoop;

    fn transform(
        self,
        _: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        todo!()
    }
}

impl Transform for ext::Decl {
    type Output = Declaration;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            Self::VarDecl(var_decl) => var_decl
                .transform_spanned(context)
                .map(Declaration::Lexical),
            Self::FnDecl(fn_decl) => fn_decl
                .transform_spanned(context)
                .map(Declaration::Function),
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

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
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
        let variables = self.declared().transform_all(context)?;
        Ok(LexicalDeclaration { variables })
    }
}

impl Transform for ext::Declarator {
    type Output = Variable;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let binding =
            self.pattern().required(self.range())?.transform(context)?;
        let init = self.value().transform_spanned_opt(context)?.map(Box::new);
        Ok(Variable { binding, init })
    }
}

impl Transform for ext::PatternOrExpr {
    type Output = Binding;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            ext::PatternOrExpr::Pattern(pattern) => pattern.transform(context),
            ext::PatternOrExpr::Expr(ext::Expr::NameRef(name_ref)) => {
                name_ref.transform_spanned(context).map(Binding::Identifier)
            }
            ext::PatternOrExpr::Expr(_) => todo!("how this happen?"),
        }
    }
}

impl Transform for ext::Pattern {
    type Output = Binding;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            Self::SinglePattern(single_pattern) => single_pattern
                .transform_spanned(context)
                .map(Binding::Identifier),
            Self::ArrayPattern(_) => todo!(),
            Self::ObjectPattern(object_pattern) => object_pattern
                .elements()
                .transform_all(context)
                .map(Binding::Object),
            Self::ExprPattern(_) => todo!("what it this?"),
            Self::RestPattern(_) => todo!("not allowed here"),
            Self::AssignPattern(_) => todo!("not allowed here"),
        }
    }
}

impl Transform for ext::SinglePattern {
    type Output = Identifier;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        self.name().required(self.range())?.transform(context)
    }
}

impl Transform for ext::AssignPattern {
    type Output = (Binding, Expression);

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let binding = self.key().required(self.range())?.transform(context)?;
        let expression =
            self.value().required(self.range())?.transform(context)?;
        Ok((binding, expression))
    }
}

impl Transform for ext::ObjectPatternProp {
    type Output = ObjectPatternElement;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            Self::AssignPattern(_) => todo!(),
            Self::KeyValuePattern(key_value_pattern) => {
                let key = key_value_pattern
                    .key()
                    .required(key_value_pattern.range())?
                    .transform_spanned(context)?;
                let value = key_value_pattern
                    .value()
                    .required(key_value_pattern.range())?
                    .transform_spanned(context)?;
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
                    .transform_spanned(context)?;
                Ok(ObjectPatternElement::Rest {
                    binding,
                    init: None,
                })
            }
            Self::SinglePattern(single_pattern) => {
                let identifier = single_pattern.transform_spanned(context)?;
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

    fn transform(
        self,
        _: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        Ok(Identifier::new(
            self.ident_token().required(self.range())?.to_string(),
        ))
    }
}

impl Transform for ext::Name {
    type Output = Identifier;

    fn transform(
        self,
        _: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        Ok(Identifier::new(self.text()))
    }
}

impl Transform for ext::FnDecl {
    type Output = FunctionDeclaration;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        // `function f()` is the only supported form; `function()` in invalid
        let name = self
            .name()
            .required(self.range())?
            .transform_spanned(context)?;
        let parameters = self
            .parameters()
            .required(self.range())?
            .transform(context)?;
        let body = self
            .body()
            .required(self.range())?
            .transform(context)?
            .statements;
        let pointer = FunctionPointer::Inline(
            FunctionDefinition {
                name: Some(name.clone()),
                parameters,
                body,
                captures: [].into(), // Will be filled out later
            }
            .into_spanned(self.range()),
        );
        Ok(FunctionDeclaration { name, pointer })
    }
}

impl Transform for ext::ArrowExpr {
    type Output = FunctionPointer;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let parameters = match self.params().required(self.range())? {
            ext::ArrowExprParams::Name(name) => {
                let identifier = name.transform_spanned(context)?;
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
                parameter_list.transform(context)?
            }
        };
        let body = match self.body().required(self.range())? {
            ext::ExprOrBlock::Expr(expr) => {
                let expression = expr.transform_spanned(context)?;
                let span = expression.span;
                Box::new([
                    Statement::Return(Some(expression)).into_spanned(span)
                ])
            }
            ext::ExprOrBlock::Block(block_stmt) => {
                block_stmt.transform(context)?.statements
            }
        };
        Ok(FunctionPointer::Inline(
            FunctionDefinition {
                name: None,
                parameters,
                body,
                captures: [].into(), // Will be filled out later
            }
            .into_spanned(self.range()),
        ))
    }
}

impl Transform for ext::ParameterList {
    type Output = Box<[Spanned<FunctionParameter>]>;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        /// Recursive helper for converting a pattern to a function parameter.
        /// We can't just rely on Pattern::transform because it's used in a few
        /// different places in the AST with different semantics
        fn transform(
            pattern: ext::Pattern,
            context: &ParseContext,
        ) -> Result<Spanned<FunctionParameter>, Spanned<TransformError>>
        {
            match pattern {
                ext::Pattern::RestPattern(rest_pattern) => {
                    let pattern =
                        rest_pattern.pat().required(rest_pattern.range())?;
                    let mut parameter = transform(pattern, context)?;
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
                    let mut parameter = transform(pattern, context)?;

                    let expression = assign_pattern
                        .value()
                        .required(assign_pattern.range())?
                        .transform_spanned(context)?;
                    parameter.data.variable.data.init = Some(expression.into());
                    Ok(parameter)
                }
                pattern => {
                    let binding = pattern.transform_spanned(context)?;
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

        self.parameters()
            .map(|pattern| transform(pattern, context))
            .collect::<Result<_, _>>()
    }
}

impl Transform for ext::Expr {
    type Output = Expression;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            Self::Literal(literal) => {
                literal.transform_spanned(context).map(Expression::Literal)
            }
            Self::Template(template) => template
                .transform_spanned(context)
                .map(Expression::Template),
            Self::NameRef(name_ref) => name_ref
                .transform_spanned(context)
                .map(Expression::Identifier),
            Self::ArrayExpr(array_expr) => {
                array_expr.transform_spanned(context).map(|array| {
                    Expression::Literal(
                        Literal::Array(array.data).into_spanned(array.span),
                    )
                })
            }
            Self::ObjectExpr(object_expr) => {
                object_expr.transform_spanned(context).map(|object| {
                    Expression::Literal(
                        Literal::Object(object.data).into_spanned(object.span),
                    )
                })
            }
            Self::GroupingExpr(grouping_expr) => grouping_expr
                .inner()
                .required(grouping_expr.range())?
                .transform(context),
            Self::BracketExpr(bracket_expr) => bracket_expr
                .transform_spanned(context)
                .map(Expression::Property),
            Self::DotExpr(dot_expr) => dot_expr
                .transform_spanned(context)
                .map(Expression::Property),
            Self::CallExpr(call_expr) => {
                call_expr.transform_spanned(context).map(Expression::Call)
            }
            Self::UnaryExpr(_) => todo!(),
            Self::BinExpr(bin_expr) => {
                bin_expr.transform_spanned(context).map(Expression::Binary)
            }
            Self::CondExpr(_) => todo!(),
            Self::AssignExpr(node) => unsupported(node.range(), "TODO", "TODO"),
            Self::SequenceExpr(_) => todo!(),
            Self::ArrowExpr(arrow_expr) => arrow_expr
                .transform_spanned(context)
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

    fn transform(
        self,
        _: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
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

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        // rslint provides the quasis (literal chunks) and elements (expression
        // chunks) separately. We're responsible for piecing them back together
        // in order. Collect them all into one sequence, then sort by their
        // source position
        let mut chunks: Vec<Spanned<TemplateChunk>> = self
            .quasis()
            .map(|quasi| {
                let range = quasi.text_range();
                TemplateChunk::Literal(
                    quasi.text().to_string().into_spanned(range),
                )
                .into_spanned(range)
            })
            .chain(self.elements().transform_all(context)?)
            .collect();
        chunks.sort_by_key(|chunk| chunk.span.start_offset);
        Ok(TemplateLiteral {
            chunks: chunks.into(),
        })
    }
}

impl Transform for ext::TemplateElement {
    type Output = TemplateChunk;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let expression = self
            .expr()
            .required(self.range())?
            .transform_spanned(context)?;
        Ok(TemplateChunk::Expression(expression))
    }
}

impl Transform for ext::ArrayExpr {
    type Output = ArrayLiteral;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let elements = self
            .elements()
            .map(|element| match element {
                ext::ExprOrSpread::Expr(expr) => {
                    let expression = expr.transform_spanned(context)?;
                    let span = expression.span;
                    Ok(ArrayElement::Expression(expression).into_spanned(span))
                }
                ext::ExprOrSpread::Spread(spread_element) => {
                    let expression = spread_element
                        .element()
                        .required(spread_element.range())?
                        .transform_spanned(context)?;
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

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let properties = self.props().transform_all(context)?;
        Ok(ObjectLiteral { properties })
    }
}

impl Transform for ext::ObjectProp {
    type Output = ObjectProperty;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            Self::LiteralProp(literal_prop) => {
                let property = literal_prop
                    .key()
                    .required(literal_prop.range())?
                    .transform_spanned(context)?;
                let expression = literal_prop
                    .value()
                    .required(literal_prop.range())?
                    .transform_spanned(context)?;
                Ok(ObjectProperty::Property {
                    property,
                    expression,
                })
            }
            Self::IdentProp(ident_prop) => {
                let identifier = ident_prop
                    .name()
                    .required(ident_prop.range())?
                    .transform_spanned(context)?;
                Ok(ObjectProperty::Identifier(identifier))
            }
            Self::SpreadProp(spread_prop) => {
                let expression = spread_prop
                    .value()
                    .required(spread_prop.range())?
                    .transform_spanned(context)?;
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

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            ext::PropName::Ident(name) => {
                // {x: ...}
                let identifier = name.transform_spanned(context)?;
                Ok(PropertyName::Literal(identifier))
            }
            ext::PropName::Computed(computed_property_name) => {
                // {[f()]: ...}
                let expression = computed_property_name
                    .prop()
                    .required(computed_property_name.range())?
                    .transform_spanned(context)?;
                Ok(PropertyName::Expression(expression.into()))
            }
            ext::PropName::Literal(literal) => {
                // {"123": ...}
                if let ext::LiteralKind::String = literal.kind() {
                    let s = literal
                        .inner_string_text()
                        .required(literal.range())?
                        .to_string();
                    let span = literal.range();
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

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let expression = self
            .object()
            .required(self.range())?
            .transform_spanned(context)?;
        let prop = self.prop().required(self.range())?;
        let property_span = prop.range();
        let property = prop.transform_spanned(context)?;
        Ok(PropertyAccess {
            expression: expression.into(),
            property: PropertyName::Expression(property.into())
                .into_spanned(property_span),
        })
    }
}

impl Transform for ext::DotExpr {
    type Output = PropertyAccess;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let expression = self
            .object()
            .required(self.range())?
            .transform_spanned(context)?;
        let identifier = self
            .prop()
            .required(self.range())?
            .transform_spanned(context)?;
        let span = self.range();
        Ok(PropertyAccess {
            expression: expression.into(),
            property: PropertyName::Literal(identifier).into_spanned(span),
        })
    }
}

impl Transform for ext::BinExpr {
    type Output = BinaryOperation;

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let operator = self.op().required(self.range())?.transform(context)?;
        let lhs = self
            .lhs()
            .required(self.range())?
            .transform_spanned(context)?;
        let rhs = self
            .rhs()
            .required(self.range())?
            .transform_spanned(context)?;
        Ok(BinaryOperation {
            operator,
            lhs: lhs.into(),
            rhs: rhs.into(),
        })
    }
}

impl Transform for ext::BinOp {
    type Output = BinaryOperator;

    fn transform(
        self,
        _: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
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

    fn transform(
        self,
        context: &ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let arguments = self
            .arguments()
            .required(self.range())?
            .args()
            .transform_all(context)?;
        let function = self
            .callee()
            .required(self.range())?
            .transform_spanned(context)?;
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
        context: &ParseContext,
    ) -> Result<Spanned<Self::Output>, Spanned<TransformError>>;
}

impl<T: ext::AstNode + Transform> TransformSpanned for T {
    fn transform_spanned(
        self,
        context: &ParseContext,
    ) -> Result<Spanned<Self::Output>, Spanned<TransformError>> {
        let span: Span = self.syntax().text_range().into();
        let data = self.transform(context)?;
        Ok(Spanned { data, span })
    }
}

/// TODO
trait TransformAll<T: Transform> {
    #[allow(clippy::type_complexity)]
    fn transform_all(
        self,
        context: &ParseContext,
    ) -> Result<Box<[Spanned<T::Output>]>, Spanned<TransformError>>;
}

impl<T: ext::AstNode + Transform> TransformAll<T> for ext::AstChildren<T> {
    fn transform_all(
        self,
        context: &ParseContext,
    ) -> Result<Box<[Spanned<T::Output>]>, Spanned<TransformError>> {
        self.map(|value| value.transform_spanned(context))
            .collect::<Result<_, _>>()
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
        context: &ParseContext,
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
            .into_spanned(parent_text_range)
        })
    }

    fn transform_spanned_opt(
        self,
        context: &ParseContext,
    ) -> Result<Option<Spanned<T::Output>>, Spanned<TransformError>>
    where
        T: TransformSpanned,
    {
        self.map(|value| value.transform_spanned(context))
            .transpose()
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
    Err(TransformError::Unsupported { name, help }.into_spanned(text_range))
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
