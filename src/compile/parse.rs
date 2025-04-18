//! Parse some source code via the rslint parser, than convert from their lazy
//! AST to our own types

use crate::{
    ast::{
        source::{
            IntoSpanned, SourceId, SourceTable, Span, SpanTable, Spanned,
        },
        ArrayElement, ArrayLiteral, BinaryOperation, BinaryOperator, Binding,
        Block, Declaration, DoWhileLoop, ExportDeclaration, Expression,
        FunctionBody, FunctionCall, FunctionDeclaration, FunctionDefinition,
        FunctionParameter, FunctionPointer, Identifier, If, ImportDeclaration,
        ImportModule, ImportNamed, LexicalDeclaration, Literal, Module, Node,
        NodeId, ObjectLiteral, ObjectPatternElement, ObjectProperty,
        PropertyAccess, PropertyName, Statement, TemplateChunk,
        TemplateLiteral, UnaryOperation, UnaryOperator, Variable, WhileLoop,
    },
    error::{Error, ParseError, TransformError},
    Source,
};
use rslint_parser::{
    ast::{self as ext},
    AstNode as _, TextRange,
};
use std::{
    any, env,
    ffi::OsStr,
    path::{Path, PathBuf},
};

/// File extensions that can be imported as a local module
pub const SUPPORTED_EXTENSIONS: &[&str] = &["js", "ts"];

/// Parse source code into an Abstract Syntax Tree. Collect all imported sources
/// along the way as well, inserting them into the source table. This assumes
/// the given source table has exactly one source in it to begin with, which is
/// the source of the root module. In addition to the parsed module, this a
/// debug table of source spans for each AST node.
pub fn parse(sources: &mut SourceTable) -> Result<(Module, SpanTable), Error> {
    let source_id = sources.root_id();
    let mut spans = SpanTable::default();
    let context = ParseContext {
        sources,
        current_source_id: source_id,
        next_node_id: &mut 0,
        spans: &mut spans,
    };
    let module = parse_module(context)?;
    Ok((module, spans))
}

/// Helper for parsing a single module. This is called recursively within a
/// parse tree, once per PS module. Each module corresponds to a single source,
/// so this will add a new source to the given source table
fn parse_module(mut context: ParseContext) -> Result<Module, Error> {
    let source = context.current_source();
    let code = source.text()?;
    let ast = rslint_parser::parse_module(&code, 0)
        .ok()
        .map_err(|errors| ParseError {
            source_name: source.name().map(String::from),
            errors,
        })?;
    let module =
        ast.transform(&mut context)
            .map_err(|error| Error::Transform {
                error: error.data,
                span: context.sources.qualify(error.span),
            })?;
    Ok(module)
}

/// Context object to be passed around to every transformation. Each parsed
/// source gets a new context, but the mutable references are passed between
/// contexts.
struct ParseContext<'a> {
    /// All sources that have been reached so far in the parse
    sources: &'a mut SourceTable,
    /// The source of the module currently being parsed
    current_source_id: SourceId,
    /// Each AST node gets a unique ID. Every time we generate a new node, we
    /// assign it this ID then increment this count. This is a mutable
    /// reference so we can share it between contexts of different sources,
    /// allowing node IDs to be unique across the entire AST.
    next_node_id: &'a mut u32,
    /// TODO
    spans: &'a mut SpanTable,
}

impl<'a> ParseContext<'a> {
    /// Current source being parsed
    fn current_source(&self) -> &dyn Source {
        self.sources.get(self.current_source_id)
    }

    /// Convert an `rslint` [TextRange] into a [Span] by attaching the current
    /// source ID to it
    fn span(&self, range: TextRange) -> Span {
        Span {
            source_id: self.current_source_id,
            start_offset: range.start().into(),
            end_offset: range.end().into(),
        }
    }

    /// Generate a new node ID for a new AST node. This ID will be unique within
    /// the entire AST, including sub-modules
    /// TODO more
    fn node_id(&mut self, range: TextRange) -> NodeId {
        let id = NodeId::new(*self.next_node_id);
        *self.next_node_id += 1;
        self.spans.insert(id, self.span(range));
        id
    }
}

/// Transform an rslint AST node to a local AST node
trait Transform {
    type Output;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>>;

    /// TODO
    fn transform_node(
        self,
        context: &mut ParseContext,
    ) -> Result<Node<Self::Output>, Spanned<TransformError>>
    where
        Self: Sized + ext::AstNode,
    {
        let range = self.range();
        Ok(self.transform(context)?.into_node(context, range))
    }
}

impl Transform for ext::Module {
    type Output = Module;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let statements = self.items().transform_all(context)?;
        Ok(Module { statements })
    }
}

impl Transform for ext::ModuleItem {
    type Output = Statement;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            Self::ImportDecl(import_decl) => {
                import_decl.transform_node(context).map(Statement::Import)
            }
            Self::ExportDefaultDecl(_) => todo!(),
            Self::ExportDefaultExpr(export_default_expr) => export_default_expr
                .transform_node(context)
                .map(Statement::Export),
            Self::ExportDecl(export_decl) => {
                export_decl.transform_node(context).map(Statement::Export)
            }
            Self::ExportNamed(_) => todo!(),
            Self::ExportWildcard(_) => todo!(),

            Self::Stmt(stmt) => stmt.transform(context),

            Self::TsImportEqualsDecl(node) => {
                unsupported_ts(context.span(node.range()))
            }
            Self::TsExportAssignment(node) => {
                unsupported_ts(context.span(node.range()))
            }
            Self::TsNamespaceExportDecl(node) => {
                unsupported_ts(context.span(node.range()))
            }
        }
    }
}

impl Transform for ext::ImportDecl {
    type Output = ImportDeclaration;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        // TODO only allow imports at top level

        if let Some(node) = self.assert_token() {
            // https://v8.dev/features/import-assertions
            unsupported(
                context.span(node.text_range()),
                "import assertion",
                "TODO",
            )?;
        }

        // TODO explain
        let mut default: Option<Node<Identifier>> = None;
        let mut named: Vec<Node<ImportNamed>> = vec![];
        for import in self.imports() {
            match import {
                ext::ImportClause::WildcardImport(_) => {
                    todo!("unsupported")
                }
                ext::ImportClause::NamedImports(named_imports) => {
                    named.extend(
                        named_imports.specifiers().transform_all(context)?,
                    );
                }
                ext::ImportClause::Name(name) => {
                    // TODO handle error for duplicate
                    default.replace(name.transform_node(context)?);
                }
                ext::ImportClause::ImportStringSpecifier(_) => {
                    todo!("what is this?")
                }
            }
        }

        let source = self.source().required(context.span(self.range()))?;
        let source_span = context.span(source.range());
        let source_text = source
            .inner_string_text()
            .required(source_span)?
            .to_string();

        // If it's a valid native module name, treat it as such. Otherwise treat
        // it as a path. A path is always going to have a character like . or /
        // that will make it an invalid module name
        let module = match source_text.try_into() {
            Ok(name) => ImportModule::Native(name),
            Err(error) => {
                let ast = resolve_module(error.name, context.current_source())
                    .and_then(|path| {
                        let source_id = context.sources.insert(path);
                        parse_module(ParseContext {
                            sources: context.sources,
                            current_source_id: source_id,
                            next_node_id: context.next_node_id,
                            spans: context.spans,
                        })
                    })
                    .map_err(|error| {
                        TransformError::Import(error.into())
                            .into_spanned(source_span)
                    })?;
                ImportModule::Local(ast)
            }
        }
        .into_node(context, source.range());

        match (default, named.as_slice()) {
            (None, &[]) => todo!("error no imports specified"),
            (default, _) => Ok(ImportDeclaration {
                default,
                named: named.into(),
                module,
            }),
        }
    }
}

/// Resolve a local module specifier into a path. This will resolve relative
/// paths to be relative to the importing module. This does not guarantee the
/// file is actually present!
///
/// ## Errors
///
/// Error if the file
fn resolve_module(
    specifier: String,
    parent_source: &dyn Source,
) -> Result<PathBuf, Error> {
    let path: PathBuf = specifier.into();

    if path.is_absolute() {
        todo!("error");
    }

    let path =
        if let Some(dir) = parent_source.import_root().and_then(Path::parent) {
            dir.join(path)
        } else {
            env::current_dir()
                .map_err(|error| Error::Io { error, path: None })?
                .join(path)
        };

    // Ensure the path is a file with a supported extension
    if path
        .extension()
        .and_then(OsStr::to_str)
        .is_none_or(|extension| !SUPPORTED_EXTENSIONS.contains(&extension))
    {
        todo!("error")
    }
    Ok(path)
}

impl Transform for ext::Specifier {
    type Output = ImportNamed;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let span = context.span(self.range());
        let identifier =
            Identifier::new(self.name().required(span)?.to_string())
                .into_node(context, self.range());
        // Optional `as x` rename
        let rename = self.alias().transform_node_opt(context)?;
        Ok(ImportNamed { identifier, rename })
    }
}

impl Transform for ext::ExportDecl {
    type Output = ExportDeclaration;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        // TODO only allow exports at top level

        self.decl()
            .required(context.span(self.range()))?
            .transform_node(context)
            .map(ExportDeclaration::Declaration)
    }
}

impl Transform for ext::ExportDefaultExpr {
    type Output = ExportDeclaration;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        self.expr()
            .required(context.span(self.range()))?
            .transform_node(context)
            .map(ExportDeclaration::DefaultExpression)
    }
}

impl Transform for ext::Stmt {
    type Output = Statement;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            // Things that can return None
            Self::EmptyStmt(_) => Ok(Statement::Empty),
            Self::ExprStmt(expr_stmt) => expr_stmt
                .expr()
                .required(context.span(expr_stmt.range()))?
                .transform_node(context)
                .map(Statement::Expression),

            // Things that definitely can't return None
            Self::BlockStmt(block_stmt) => {
                block_stmt.transform_node(context).map(Statement::Block)
            }
            Self::IfStmt(if_stmt) => {
                if_stmt.transform_node(context).map(Statement::If)
            }
            Self::DoWhileStmt(do_while_stmt) => do_while_stmt
                .transform_node(context)
                .map(Statement::DoWhileLoop),
            Self::WhileStmt(while_stmt) => {
                while_stmt.transform_node(context).map(Statement::WhileLoop)
            }
            Self::ForStmt(node) => {
                unsupported(context.span(node.range()), "TODO", "TODO")
            }
            // TODO for..of loop?
            Self::ForInStmt(_) => todo!(),
            // TODO error if labels are present
            Self::ContinueStmt(_) => Ok(Statement::Continue),
            Self::BreakStmt(_) => Ok(Statement::Break),
            Self::ReturnStmt(return_stmt) => {
                let expression =
                    return_stmt.value().transform_node_opt(context)?;
                Ok(Statement::Return(expression))
            }
            Self::LabelledStmt(node) => {
                unsupported(context.span(node.range()), "TODO", "TODO")
            }
            Self::SwitchStmt(_) => todo!(),
            Self::ThrowStmt(_) => todo!(),
            Self::TryStmt(_) => todo!(),
            Self::Decl(decl) => {
                decl.transform_node(context).map(Statement::Declaration)
            }
            Self::DebuggerStmt(node) => {
                unsupported(context.span(node.range()), "TODO", "TODO")
            }
            Self::WithStmt(node) => {
                unsupported(context.span(node.range()), "TODO", "TODO")
            }
        }
    }
}

impl Transform for ext::BlockStmt {
    type Output = Block;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let statements = self.stmts().transform_all(context)?;
        Ok(Block { statements })
    }
}

impl Transform for ext::IfStmt {
    type Output = If;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let span = context.span(self.range());
        let condition = self
            .condition()
            .required(span)?
            .condition()
            .required(span)?
            .transform_node(context)?;
        // This body is NOT optional
        let body = self.cons().required(span)?.transform_node(context)?;
        // This body IS optional
        let else_body = self.alt().transform_node_opt(context)?;
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
        _: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        todo!()
    }
}

impl Transform for ext::WhileStmt {
    type Output = WhileLoop;

    fn transform(
        self,
        _: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        todo!()
    }
}

impl Transform for ext::Decl {
    type Output = Declaration;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            Self::VarDecl(var_decl) => {
                var_decl.transform_node(context).map(Declaration::Lexical)
            }
            Self::FnDecl(fn_decl) => {
                fn_decl.transform_node(context).map(Declaration::Function)
            }
            Self::ClassDecl(node) => unsupported(
                context.span(node.range()),
                "`class`",
                "Classes are not supported",
            ),
            Self::TsEnum(node) => unsupported_ts(context.span(node.range())),
            Self::TsTypeAliasDecl(node) => {
                unsupported_ts(context.span(node.range()))
            }
            Self::TsNamespaceDecl(node) => {
                unsupported_ts(context.span(node.range()))
            }
            Self::TsModuleDecl(node) => {
                unsupported_ts(context.span(node.range()))
            }
            Self::TsInterfaceDecl(node) => {
                unsupported_ts(context.span(node.range()))
            }
        }
    }
}

impl Transform for ext::VarDecl {
    type Output = LexicalDeclaration;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        if self.is_var() {
            return unsupported(
                context.span(self.range()),
                "`var`",
                "Use `const` instead",
            );
        }
        if self.is_let() {
            // TODO add link to docs explaining immutability
            return unsupported(
                context.span(self.range()),
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
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let binding = self
            .pattern()
            .required(context.span(self.range()))?
            .transform(context)?
            .into_node(context, self.range());
        let init = self.value().transform_node_opt(context)?.map(Box::new);
        Ok(Variable { binding, init })
    }
}

impl Transform for ext::PatternOrExpr {
    type Output = Binding;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            ext::PatternOrExpr::Pattern(pattern) => pattern.transform(context),
            ext::PatternOrExpr::Expr(ext::Expr::NameRef(name_ref)) => {
                name_ref.transform_node(context).map(Binding::Identifier)
            }
            ext::PatternOrExpr::Expr(_) => todo!("how this happen?"),
        }
    }
}

impl Transform for ext::Pattern {
    type Output = Binding;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            Self::SinglePattern(single_pattern) => single_pattern
                .transform_node(context)
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
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        self.name()
            .required(context.span(self.range()))?
            .transform(context)
    }
}

impl Transform for ext::AssignPattern {
    type Output = (Binding, Expression);

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let span = context.span(self.range());
        let binding = self.key().required(span)?.transform(context)?;
        let expression = self.value().required(span)?.transform(context)?;
        Ok((binding, expression))
    }
}

impl Transform for ext::ObjectPatternProp {
    type Output = ObjectPatternElement;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            Self::AssignPattern(_) => todo!(),
            Self::KeyValuePattern(key_value_pattern) => {
                let span = context.span(key_value_pattern.range());
                let key = key_value_pattern
                    .key()
                    .required(span)?
                    .transform_node(context)?;
                let value = key_value_pattern
                    .value()
                    .required(span)?
                    .transform_node(context)?;
                Ok(ObjectPatternElement::Mapped {
                    key,
                    value,
                    init: None,
                })
            }
            Self::RestPattern(rest_pattern) => {
                let binding = rest_pattern
                    .pat()
                    .required(context.span(rest_pattern.range()))?
                    .transform_node(context)?;
                Ok(ObjectPatternElement::Rest {
                    binding,
                    init: None,
                })
            }
            Self::SinglePattern(single_pattern) => {
                let identifier = single_pattern.transform_node(context)?;
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
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        Ok(Identifier::new(
            self.ident_token()
                .required(context.span(self.range()))?
                .to_string(),
        ))
    }
}

impl Transform for ext::Name {
    type Output = Identifier;

    fn transform(
        self,
        _: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        Ok(Identifier::new(self.text()))
    }
}

impl Transform for ext::FnDecl {
    type Output = FunctionDeclaration;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        // `function f()` is the only supported form; `function()` in invalid
        let span = context.span(self.range());
        let name = self.name().required(span)?.transform_node(context)?;
        let parameters =
            self.parameters().required(span)?.transform(context)?;
        let body_block = self.body().required(span)?.transform_node(context)?;
        let pointer = FunctionPointer::Inline(FunctionDefinition {
            name: Some(name.clone()),
            parameters,
            body: FunctionBody::Block(body_block),
            captures: [].into(), // Will be filled out later
        })
        .into_node(context, self.range());
        Ok(FunctionDeclaration { name, pointer })
    }
}

impl Transform for ext::ArrowExpr {
    type Output = FunctionPointer;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let span = context.span(self.range());
        let parameters = match self.params().required(span)? {
            ext::ArrowExprParams::Name(name) => {
                let range = name.range();
                let identifier = name.transform_node(context)?;
                let parameter = FunctionParameter {
                    variable: Variable {
                        binding: Binding::Identifier(identifier)
                            .into_node(context, range),
                        init: None,
                    }
                    .into_node(context, range),
                    varargs: false,
                }
                .into_node(context, range);
                Box::new([parameter])
            }
            ext::ArrowExprParams::ParameterList(parameter_list) => {
                parameter_list.transform(context)?
            }
        };
        let body = match self.body().required(span)? {
            ext::ExprOrBlock::Expr(expr) => {
                let expression = expr.transform_node(context)?;
                FunctionBody::Expression(expression.into())
            }
            ext::ExprOrBlock::Block(block_stmt) => {
                let block = block_stmt.transform_node(context)?;
                FunctionBody::Block(block)
            }
        };
        Ok(FunctionPointer::Inline(FunctionDefinition {
            name: None,
            parameters,
            body,
            captures: [].into(), // Will be filled out later in the pipeline
        }))
    }
}

impl Transform for ext::ParameterList {
    type Output = Box<[Node<FunctionParameter>]>;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        /// Recursive helper for converting a pattern to a function parameter.
        /// We can't just rely on Pattern::transform because it's used in a few
        /// different places in the AST with different semantics
        fn transform(
            pattern: ext::Pattern,
            context: &mut ParseContext,
        ) -> Result<Node<FunctionParameter>, Spanned<TransformError>> {
            match pattern {
                ext::Pattern::RestPattern(rest_pattern) => {
                    let pattern = rest_pattern
                        .pat()
                        .required(context.span(rest_pattern.range()))?;
                    let mut parameter = transform(pattern, context)?;
                    parameter.varargs = true;
                    Ok(parameter)
                }
                ext::Pattern::AssignPattern(assign_pattern) => {
                    // Param has a default expression. Everything left of the =
                    // is the key, which could be a sub-pattern
                    // TODO why doesn't this work?
                    let span = context.span(assign_pattern.range());
                    let pattern = assign_pattern.key().required(span)?;
                    let mut parameter = transform(pattern, context)?;

                    let expression = assign_pattern
                        .value()
                        .required(span)?
                        .transform_node(context)?;
                    parameter.variable.init = Some(expression.into());
                    Ok(parameter)
                }
                pattern => {
                    let range = pattern.range();
                    Ok(FunctionParameter {
                        variable: Variable {
                            binding: pattern.transform_node(context)?,
                            init: None,
                        }
                        .into_node(context, range),
                        varargs: false,
                    }
                    .into_node(context, range))
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
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            Self::Literal(literal) => {
                literal.transform_node(context).map(Expression::Literal)
            }
            Self::Template(template) => {
                template.transform_node(context).map(Expression::Template)
            }
            Self::NameRef(name_ref) => {
                name_ref.transform_node(context).map(Expression::Identifier)
            }
            Self::ArrayExpr(array_expr) => {
                let range = array_expr.range();
                let array = array_expr.transform_node(context)?;
                Ok(Expression::Literal(
                    Literal::Array(array).into_node(context, range),
                ))
            }
            Self::ObjectExpr(object_expr) => {
                let range = object_expr.range();
                let object = object_expr.transform_node(context)?;
                Ok(Expression::Literal(
                    Literal::Object(object).into_node(context, range),
                ))
            }
            Self::GroupingExpr(grouping_expr) => grouping_expr
                .inner()
                .required(context.span(grouping_expr.range()))?
                .transform(context),
            Self::BracketExpr(bracket_expr) => bracket_expr
                .transform_node(context)
                .map(Expression::Property),
            Self::DotExpr(dot_expr) => {
                dot_expr.transform_node(context).map(Expression::Property)
            }
            Self::CallExpr(call_expr) => {
                call_expr.transform_node(context).map(Expression::Call)
            }
            Self::UnaryExpr(unary_expr) => {
                unary_expr.transform_node(context).map(Expression::Unary)
            }
            Self::BinExpr(bin_expr) => {
                bin_expr.transform_node(context).map(Expression::Binary)
            }
            Self::CondExpr(_) => todo!(),
            Self::AssignExpr(node) => {
                unsupported(context.span(node.range()), "TODO", "TODO")
            }
            Self::SequenceExpr(_) => todo!(),
            Self::ArrowExpr(arrow_expr) => arrow_expr
                .transform_node(context)
                .map(Expression::ArrowFunction),
            Self::FnExpr(_) => todo!("wtf is this?"),

            // Unsupported
            Self::ThisExpr(expr) => unsupported(
                context.span(expr.range()),
                "`this`",
                "`this` is not supported",
            ),
            Self::NewExpr(node) => unsupported(
                context.span(node.range()),
                "`new`",
                "Classes are not supported",
            ),
            Self::SuperCall(node) => unsupported(
                context.span(node.range()),
                "`super`",
                "Classes are not supported",
            ),
            Self::ClassExpr(_) => todo!(),
            Self::NewTarget(_) => todo!(),
            Self::PrivatePropAccess(_) => todo!(),
            Self::ImportMeta(_) => todo!(),
            Self::ImportCall(node) => unsupported(
                context.span(node.range()),
                "`import()`",
                "Use the ES6-style `import` keyword instead",
            ),
            Self::AwaitExpr(node) => unsupported(
                context.span(node.range()),
                "`await`",
                "Async-await is not supported",
            ),
            Self::YieldExpr(node) => unsupported(
                context.span(node.range()),
                "`yield`",
                "Generators are not supported",
            ),
            Self::TsNonNull(node) => unsupported_ts(context.span(node.range())),
            Self::TsAssertion(node) => {
                unsupported_ts(context.span(node.range()))
            }
            Self::TsConstAssertion(node) => {
                unsupported_ts(context.span(node.range()))
            }
        }
    }
}

impl Transform for ext::Literal {
    type Output = Literal;

    fn transform(
        self,
        context: &mut ParseContext,
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
            ext::LiteralKind::BigInt(int) => {
                Literal::Int(int.try_into().or(unsupported(
                    context.span(self.range()),
                    "TODO",
                    "TODO",
                ))?)
            }
            ext::LiteralKind::String => Literal::String(
                self.inner_string_text()
                    .required(context.span(self.range()))?
                    .to_string(),
            ),
            ext::LiteralKind::Regex => {
                unsupported(context.span(self.range()), "TODO", "TODO")?
            }
        };
        Ok(literal)
    }
}

impl Transform for ext::Template {
    type Output = TemplateLiteral;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        // rslint provides the quasis (literal chunks) and elements (expression
        // chunks) separately. We're responsible for piecing them back together
        // in order. Collect them all into one sequence, then sort by their
        // source position
        let mut chunks: Vec<(Node<TemplateChunk>, TextRange)> = self
            .quasis()
            .map(|quasi| {
                let literal = quasi.text().to_string();
                let range = quasi.text_range();
                (
                    TemplateChunk::Literal(literal).into_node(context, range),
                    range,
                )
            })
            .collect();
        chunks.extend(
            // Pair each expression with its range so we can sort
            self.elements()
                .zip(self.elements().transform_all(context)?)
                .map(|(element, chunk)| (chunk, element.range())),
        );
        chunks.sort_by_key(|(_, range)| range.start());
        Ok(TemplateLiteral {
            chunks: chunks
                .into_iter()
                .map(|(chunk, _)| chunk)
                .collect::<Vec<_>>()
                .into(),
        })
    }
}

impl Transform for ext::TemplateElement {
    type Output = TemplateChunk;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let expression = self
            .expr()
            .required(context.span(self.range()))?
            .transform_node(context)?;
        Ok(TemplateChunk::Expression(expression))
    }
}

impl Transform for ext::ArrayExpr {
    type Output = ArrayLiteral;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let elements = self
            .elements()
            .map(|element| match element {
                ext::ExprOrSpread::Expr(expr) => {
                    let range = expr.range();
                    let expression = expr.transform_node(context)?;
                    Ok(ArrayElement::Expression(expression)
                        .into_node(context, range))
                }
                ext::ExprOrSpread::Spread(spread_element) => {
                    let range = spread_element.range();
                    let expression = spread_element
                        .element()
                        .required(context.span(spread_element.range()))?
                        .transform_node(context)?;
                    Ok(ArrayElement::Spread(expression)
                        .into_node(context, range))
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
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let properties = self.props().transform_all(context)?;
        Ok(ObjectLiteral { properties })
    }
}

impl Transform for ext::ObjectProp {
    type Output = ObjectProperty;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            Self::LiteralProp(literal_prop) => {
                let span = context.span(literal_prop.range());
                let property = literal_prop
                    .key()
                    .required(span)?
                    .transform_node(context)?;
                let expression = literal_prop
                    .value()
                    .required(span)?
                    .transform_node(context)?;
                Ok(ObjectProperty::Property {
                    property,
                    expression,
                })
            }
            Self::IdentProp(ident_prop) => {
                let identifier = ident_prop
                    .name()
                    .required(context.span(ident_prop.range()))?
                    .transform_node(context)?;
                Ok(ObjectProperty::Identifier(identifier))
            }
            Self::SpreadProp(spread_prop) => {
                let expression = spread_prop
                    .value()
                    .required(context.span(spread_prop.range()))?
                    .transform_node(context)?;
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
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        match self {
            ext::PropName::Ident(name) => {
                // {x: ...}
                let identifier = name.transform_node(context)?;
                Ok(PropertyName::Literal(identifier))
            }
            ext::PropName::Computed(computed_property_name) => {
                // {[f()]: ...}
                let expression = computed_property_name
                    .prop()
                    .required(context.span(computed_property_name.range()))?
                    .transform_node(context)?;
                Ok(PropertyName::Expression(expression.into()))
            }
            ext::PropName::Literal(literal) => {
                // {"123": ...}
                if let ext::LiteralKind::String = literal.kind() {
                    let s = literal
                        .inner_string_text()
                        .required(context.span(literal.range()))?
                        .to_string();
                    let range = literal.range();
                    let literal = Literal::String(s).into_node(context, range);
                    Ok(PropertyName::Expression(
                        Expression::Literal(literal)
                            .into_node(context, range)
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
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let span = context.span(self.range());
        let expression =
            self.object().required(span)?.transform_node(context)?;
        let prop = self.prop().required(span)?;
        let property_range = prop.range();
        let property = prop.transform_node(context)?;
        Ok(PropertyAccess {
            expression: expression.into(),
            property: PropertyName::Expression(property.into())
                .into_node(context, property_range),
        })
    }
}

impl Transform for ext::DotExpr {
    type Output = PropertyAccess;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let span = context.span(self.range());
        let expression =
            self.object().required(span)?.transform_node(context)?;
        let identifier = self.prop().required(span)?.transform_node(context)?;

        Ok(PropertyAccess {
            expression: expression.into(),
            property: PropertyName::Literal(identifier)
                .into_node(context, self.range()),
        })
    }
}

impl Transform for ext::UnaryExpr {
    type Output = UnaryOperation;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let span = context.span(self.range());
        let operator = match self.op().required(span)? {
            ext::UnaryOp::LogicalNot => UnaryOperator::BooleanNot,
            ext::UnaryOp::Plus => UnaryOperator::Plus,
            ext::UnaryOp::Minus => UnaryOperator::Minus,
            ext::UnaryOp::Typeof => UnaryOperator::Typeof,
            ext::UnaryOp::Increment => {
                unsupported(span, "`++`", "Variable mutation is not supported")?
            }
            ext::UnaryOp::Decrement => {
                unsupported(span, "`--`", "Variable mutation is not supported")?
            }
            ext::UnaryOp::Delete => unsupported(
                span,
                "`delete`",
                "Object mutation is not supported",
            )?,
            ext::UnaryOp::Void => {
                unsupported(span, "`void`", "Void operator is not supported")?
            }
            ext::UnaryOp::BitwiseNot => unsupported(
                span,
                "`~`",
                "Bitwise operations are not supported",
            )?,
            ext::UnaryOp::Await => {
                unsupported(span, "`await`", "Async-await is not supported")?
            }
        };
        let expression = self.expr().required(span)?.transform_node(context)?;
        Ok(UnaryOperation {
            operator,
            expression: expression.into(),
        })
    }
}

impl Transform for ext::BinExpr {
    type Output = BinaryOperation;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let span = context.span(self.range());
        let operator = match self.op().required(span)? {
            ext::BinOp::LessThan => BinaryOperator::LessThan,
            ext::BinOp::LessThanOrEqual => BinaryOperator::LessThanEqual,
            ext::BinOp::GreaterThan => BinaryOperator::GreaterThan,
            ext::BinOp::GreaterThanOrEqual => BinaryOperator::GreaterThanEqual,
            ext::BinOp::StrictEquality => BinaryOperator::Equal,
            ext::BinOp::StrictInequality => BinaryOperator::NotEqual,
            ext::BinOp::Plus => BinaryOperator::Add,
            ext::BinOp::Minus => BinaryOperator::Sub,
            ext::BinOp::Times => BinaryOperator::Mul,
            ext::BinOp::Divide => BinaryOperator::Div,
            ext::BinOp::Remainder => BinaryOperator::Mod,
            ext::BinOp::LogicalOr => BinaryOperator::BooleanOr,
            ext::BinOp::LogicalAnd => BinaryOperator::BooleanAnd,
            ext::BinOp::NullishCoalescing => BinaryOperator::NullishCoalesce,

            ext::BinOp::Exponent => todo!(),
            ext::BinOp::LeftShift => todo!(),
            ext::BinOp::RightShift => todo!(),
            ext::BinOp::Equality => todo!(),
            ext::BinOp::Inequality => todo!(),
            ext::BinOp::UnsignedRightShift => todo!(),
            ext::BinOp::BitwiseAnd => todo!(),
            ext::BinOp::BitwiseOr => todo!(),
            ext::BinOp::BitwiseXor => todo!(),
            ext::BinOp::In => todo!(),
            ext::BinOp::Instanceof => todo!(),
        };
        let lhs = self.lhs().required(span)?.transform_node(context)?;
        let rhs = self.rhs().required(span)?.transform_node(context)?;
        Ok(BinaryOperation {
            operator,
            lhs: lhs.into(),
            rhs: rhs.into(),
        })
    }
}

impl Transform for ext::CallExpr {
    type Output = FunctionCall;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Spanned<TransformError>> {
        let span = context.span(self.range());
        // TODO handle spread operator here. Doesn't look like rslint supports
        // it
        let arguments = self
            .arguments()
            .required(span)?
            .args()
            .transform_all(context)?;
        let function = self.callee().required(span)?.transform_node(context)?;
        Ok(FunctionCall {
            function: function.into(),
            arguments,
        })
    }
}

/// TODO
trait TransformAll<T: Transform> {
    #[allow(clippy::type_complexity)]
    fn transform_all(
        self,
        context: &mut ParseContext,
    ) -> Result<Box<[Node<T::Output>]>, Spanned<TransformError>>;
}

impl<T: ext::AstNode + Transform> TransformAll<T> for ext::AstChildren<T> {
    fn transform_all(
        self,
        context: &mut ParseContext,
    ) -> Result<Box<[Node<T::Output>]>, Spanned<TransformError>> {
        self.map(|value| value.transform_node(context))
            .collect::<Result<_, _>>()
    }
}

trait OptionExt<T> {
    /// Ensure an AST node is present. Rslint has a very forgiving parser that
    /// allows most AST nodes to be missing, because it's designed to work on
    /// incomplete/incorrect code. We need correct code to execute though, so
    /// this method makes it easy to enforce AST nodes are present.
    /// TODO take a node ID here instead of a span
    fn required(self, parent_span: Span) -> Result<T, Spanned<TransformError>>;

    /// Call [Transform::transform_node] on an optional AST node
    fn transform_node_opt(
        self,
        context: &mut ParseContext,
    ) -> Result<Option<Node<T::Output>>, Spanned<TransformError>>
    where
        T: Transform + ext::AstNode;
}

impl<T> OptionExt<T> for Option<T> {
    fn required(self, parent_span: Span) -> Result<T, Spanned<TransformError>> {
        self.ok_or_else(|| {
            TransformError::Missing {
                expected_type: any::type_name::<T>(),
            }
            .into_spanned(parent_span)
        })
    }

    fn transform_node_opt(
        self,
        context: &mut ParseContext,
    ) -> Result<Option<Node<T::Output>>, Spanned<TransformError>>
    where
        T: Transform + ext::AstNode,
    {
        self.map(|value| value.transform_node(context)).transpose()
    }
}

/// Helper for generating an [Unsupported](TransformError::Unsupported) error,
/// wrapped in a `Result` for Added Convenience
fn unsupported<T>(
    span: Span,
    name: &'static str,
    help: &'static str,
) -> Result<T, Spanned<TransformError>> {
    Err(TransformError::Unsupported { name, help }.into_spanned(span))
}

/// TODO
fn unsupported_ts<T>(span: Span) -> Result<T, Spanned<TransformError>> {
    unsupported(
        span,
        "TypeScript",
        "Type annotations and other TypeScript constructs \
            are not supported",
    )
}

/// Helper for attach an ID to an AST node
trait IntoNode: Sized {
    fn into_node(
        self,
        context: &mut ParseContext,
        range: TextRange,
    ) -> Node<Self>;
}

impl<T> IntoNode for T {
    fn into_node(
        self,
        context: &mut ParseContext,
        range: TextRange,
    ) -> Node<Self> {
        Node::new(context.node_id(range), self)
    }
}
