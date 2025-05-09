//! Parse some source code via the rslint parser, than convert from their lazy
//! AST to our own types

use crate::{
    ast::{
        ArrayElement, ArrayLiteral, BinaryOperation, BinaryOperator, Binding,
        Block, Declaration, DoWhileLoop, ExportDeclaration, Expression,
        FunctionBody, FunctionCall, FunctionDefinition, FunctionParameter,
        Identifier, If, ImportDeclaration, ImportModule, ImportNamed, Literal,
        Module, Node, NodeId, ObjectLiteral, ObjectPatternElement,
        ObjectProperty, PropertyAccess, PropertyName, Statement, TemplateChunk,
        TemplateLiteral, UnaryOperation, UnaryOperator, Variable, WhileLoop,
    },
    error::{Error, ParseError, TransformError},
    source::{self, SourceId, SourceTable, Span, SpanTable},
    Source,
};
use rslint_parser::{
    ast::{self as ext},
    AstNode as _, TextRange,
};
use std::{any, ffi::OsStr, path::PathBuf};

/// File extensions that can be imported as a local module
pub const SUPPORTED_EXTENSIONS: &[&str] = &["js", "ts"];

/// Parse source code into an Abstract Syntax Tree. Collect all imported sources
/// along the way as well, inserting them into the source table. This assumes
/// the given source table has exactly one source in it to begin with, which is
/// the source of the root module. In addition to the parsed module, this a
/// debug table of source spans for each AST node.
pub fn parse(
    sources: &mut SourceTable,
) -> Result<(Node<Module>, SpanTable), Error> {
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
fn parse_module(mut context: ParseContext) -> Result<Node<Module>, Error> {
    let source = context.current_source();
    let code = source.text()?;
    let ast = rslint_parser::parse_module(&code, 0)
        .ok()
        .map_err(|errors| ParseError {
            source_name: source.path().and_then(source::display_source_path),
            errors,
        })?;
    let module = ast.transform_node(&mut context).map_err(|error| {
        // We have the ID of the AST node that we were trying to build when the
        // error occurred. Grab the span for that node, then map the span to
        // actual source code for the user
        let span = error
            .node_id
            .and_then(|node_id| context.spans.get(node_id))
            // Shouldn't be possible because we insert spans while generating
            // the node ID
            .expect("Node missing span");
        Error::Transform {
            error: error.error,
            span: context.sources.qualify(span),
        }
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
    /// A map of each transformed node's source span. This will contain an
    /// entry for every node ID that's been handed out.
    spans: &'a mut SpanTable,
}

impl<'a> ParseContext<'a> {
    /// Current source being parsed
    fn current_source(&self) -> &dyn Source {
        self.sources.get(self.current_source_id)
    }

    /// Generate a new node ID for a new AST node. This ID will be unique within
    /// the entire AST, including sub-modules. This will also insert the node's
    /// source span into the span table.
    fn node_id(&mut self, range: TextRange) -> NodeId {
        let id = NodeId::new(*self.next_node_id);
        *self.next_node_id += 1;
        self.spans.insert(
            id,
            Span {
                source_id: self.current_source_id,
                start_offset: range.start().into(),
                end_offset: range.end().into(),
            },
        );
        id
    }
}

/// An error that occurs during transformation. The name is pretty arbitrary
struct TxError {
    /// The nearest parent node to where the error occured. This is optional
    /// because it's set to `None` at first, and populated by the innermost
    /// [Transform::transform_node] call that sees the error. It must be
    /// populated by the time it bubbles to the top.
    node_id: Option<NodeId>,
    error: TransformError,
}

/// Transform an rslint AST node to a local AST node
trait Transform {
    type Output;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError>;

    /// Transform the content of an AST node and wrap it in a [Node]. This will
    /// generate the needed node ID and attach it to either the generated node
    /// or the error that may occur.
    fn transform_node(
        self,
        context: &mut ParseContext,
    ) -> Result<Node<Self::Output>, TxError>
    where
        Self: Sized + ext::AstNode,
    {
        // Pre-generate the ID, because we'll need it rain or shine
        let node_id = context.node_id(self.range());
        match self.transform(context) {
            Ok(node) => Ok(Node::new(node_id, node)),
            // If the error is missing context, that means it occurred within
            // this node. Attach our ID to it!
            Err(TxError {
                node_id: None,
                error,
            }) => Err(TxError {
                node_id: Some(node_id),
                error,
            }),
            Err(
                error @ TxError {
                    node_id: Some(_), ..
                },
            ) => Err(error),
        }
    }
}

impl Transform for ext::Module {
    type Output = Module;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let statements = self.items().transform_all(context)?;
        Ok(Module { statements })
    }
}

impl Transform for ext::ModuleItem {
    type Output = Statement;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
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

            Self::TsImportEqualsDecl(_)
            | Self::TsExportAssignment(_)
            | Self::TsNamespaceExportDecl(_) => unsupported_ts(),
        }
    }
}

impl Transform for ext::ImportDecl {
    type Output = ImportDeclaration;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        // TODO only allow imports at top level

        if self.assert_token().is_some() {
            // https://v8.dev/features/import-assertions
            unsupported("import assertion", "TODO")?;
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

        let source = self.source().required()?;
        let source_text = source.inner_string_text().required()?.to_string();

        // If it's a valid native module name, treat it as such. Otherwise treat
        // it as a path. A path is always going to have a character like . or /
        // that will make it an invalid module name
        let module = match source_text.try_into() {
            Ok(name) => ImportModule::Native(name),
            Err(error) => {
                let module =
                    resolve_module(error.name, context.current_source())
                        .and_then(|path| {
                            let source_id = context.sources.insert(path);
                            parse_module(ParseContext {
                                sources: context.sources,
                                current_source_id: source_id,
                                next_node_id: context.next_node_id,
                                spans: context.spans,
                            })
                        })
                        .map_err(|error| TxError {
                            node_id: None,
                            error: TransformError::Import(error.into()),
                        })?;
                ImportModule::Local(module)
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

    let path = parent_source.import_root()?.join(path);

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
    ) -> Result<Self::Output, TxError> {
        let identifier =
            Identifier::try_from(self.name().required()?.to_string())
                // Parsing should never fail because we theoretically use the
                // same parsing rules as rslint
                .map_err(|_| todo!())?
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
    ) -> Result<Self::Output, TxError> {
        // TODO only allow exports at top level

        self.decl()
            .required()?
            .transform_node(context)
            .map(ExportDeclaration::Declaration)
    }
}

impl Transform for ext::ExportDefaultExpr {
    type Output = ExportDeclaration;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        self.expr()
            .required()?
            .transform_node(context)
            .map(ExportDeclaration::DefaultExpression)
    }
}

impl Transform for ext::Stmt {
    type Output = Statement;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        match self {
            // Things that can return None
            Self::EmptyStmt(_) => Ok(Statement::Empty),
            Self::ExprStmt(expr_stmt) => expr_stmt
                .expr()
                .required()?
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
            Self::ForStmt(_) => unsupported("TODO", "TODO"),
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
            Self::LabelledStmt(_) => unsupported("TODO", "TODO"),
            Self::SwitchStmt(_) => todo!(),
            Self::ThrowStmt(_) => todo!(),
            Self::TryStmt(_) => todo!(),
            Self::Decl(decl) => {
                decl.transform_node(context).map(Statement::Declaration)
            }
            Self::DebuggerStmt(_) => unsupported("TODO", "TODO"),
            Self::WithStmt(_) => unsupported("TODO", "TODO"),
        }
    }
}

impl Transform for ext::BlockStmt {
    type Output = Block;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let statements = self.stmts().transform_all(context)?;
        Ok(Block { statements })
    }
}

impl Transform for ext::IfStmt {
    type Output = If;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let condition = self
            .condition()
            .required()?
            .condition()
            .required()?
            .transform_node(context)?;
        // This body is NOT optional
        let body = self.cons().required()?.transform_node(context)?;
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

    fn transform(self, _: &mut ParseContext) -> Result<Self::Output, TxError> {
        todo!()
    }
}

impl Transform for ext::WhileStmt {
    type Output = WhileLoop;

    fn transform(self, _: &mut ParseContext) -> Result<Self::Output, TxError> {
        todo!()
    }
}

impl Transform for ext::Decl {
    type Output = Declaration;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        match self {
            Self::VarDecl(var_decl) => var_decl.transform(context),
            Self::FnDecl(_) => unsupported(
                "`function`",
                "Use arrow syntax instead: `const f = (...) => {...}`",
            ),
            Self::ClassDecl(_) => {
                unsupported("`class`", "Classes are not supported")
            }
            Self::TsEnum(_)
            | Self::TsTypeAliasDecl(_)
            | Self::TsNamespaceDecl(_)
            | Self::TsModuleDecl(_)
            | Self::TsInterfaceDecl(_) => unsupported_ts(),
        }
    }
}

impl Transform for ext::VarDecl {
    type Output = Declaration;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        if self.is_var() {
            return unsupported("`var`", "Use `const` instead");
        }
        if self.is_let() {
            // TODO add link to docs explaining immutability
            return unsupported(
                "`let`",
                "Mutable bindings are not supported; use `const` instead",
            );
        }
        let variables = self.declared().transform_all(context)?;
        Ok(Declaration { variables })
    }
}

impl Transform for ext::Declarator {
    type Output = Variable;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let binding = self
            .pattern()
            .required()?
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
    ) -> Result<Self::Output, TxError> {
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
    ) -> Result<Self::Output, TxError> {
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
    ) -> Result<Self::Output, TxError> {
        self.name().required()?.transform(context)
    }
}

impl Transform for ext::AssignPattern {
    type Output = (Binding, Expression);

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let binding = self.key().required()?.transform(context)?;
        let expression = self.value().required()?.transform(context)?;
        Ok((binding, expression))
    }
}

impl Transform for ext::ObjectPatternProp {
    type Output = ObjectPatternElement;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        match self {
            Self::AssignPattern(_) => todo!(),
            Self::KeyValuePattern(key_value_pattern) => {
                let key = key_value_pattern
                    .key()
                    .required()?
                    .transform_node(context)?;
                let value = key_value_pattern
                    .value()
                    .required()?
                    .transform_node(context)?;
                Ok(ObjectPatternElement::Mapped {
                    key,
                    value,
                    init: None,
                })
            }
            Self::RestPattern(rest_pattern) => {
                let binding =
                    rest_pattern.pat().required()?.transform_node(context)?;
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

    fn transform(self, _: &mut ParseContext) -> Result<Self::Output, TxError> {
        Identifier::try_from(self.ident_token().required()?.to_string())
            // Parsing should never fail because we theoretically use the
            // same parsing rules as rslint
            .map_err(|_| todo!())
    }
}

impl Transform for ext::Name {
    type Output = Identifier;

    fn transform(self, _: &mut ParseContext) -> Result<Self::Output, TxError> {
        Identifier::try_from(self.text())
            // Parsing should never fail because we theoretically use the
            // same parsing rules as rslint
            .map_err(|_| todo!())
    }
}

impl Transform for ext::ArrowExpr {
    type Output = FunctionDefinition;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let parameters = match self.params().required()? {
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
        let body = match self.body().required()? {
            ext::ExprOrBlock::Expr(expr) => {
                let expression = expr.transform_node(context)?;
                FunctionBody::Expression(expression.into())
            }
            ext::ExprOrBlock::Block(block_stmt) => {
                let block = block_stmt.transform_node(context)?;
                FunctionBody::Block(block)
            }
        };
        Ok(FunctionDefinition {
            name: None,
            parameters,
            body,
            captures: [].into(), // Will be filled out later in the pipeline
        })
    }
}

impl Transform for ext::ParameterList {
    type Output = Box<[Node<FunctionParameter>]>;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        /// Recursive helper for converting a pattern to a function parameter.
        /// We can't just rely on Pattern::transform because it's used in a few
        /// different places in the AST with different semantics
        fn transform(
            pattern: ext::Pattern,
            context: &mut ParseContext,
        ) -> Result<Node<FunctionParameter>, TxError> {
            match pattern {
                ext::Pattern::RestPattern(rest_pattern) => {
                    let pattern = rest_pattern.pat().required()?;
                    let mut parameter = transform(pattern, context)?;
                    parameter.varargs = true;
                    Ok(parameter)
                }
                ext::Pattern::AssignPattern(assign_pattern) => {
                    // Param has a default expression. Everything left of the =
                    // is the key, which could be a sub-pattern
                    // TODO why doesn't this work?
                    let pattern = assign_pattern.key().required()?;
                    let mut parameter = transform(pattern, context)?;

                    let expression = assign_pattern
                        .value()
                        .required()?
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
    ) -> Result<Self::Output, TxError> {
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
            Self::GroupingExpr(grouping_expr) => {
                grouping_expr.inner().required()?.transform(context)
            }
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
            Self::AssignExpr(_) => unsupported("TODO", "TODO"),
            Self::SequenceExpr(_) => todo!(),
            Self::ArrowExpr(arrow_expr) => arrow_expr
                .transform_node(context)
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
            | Self::TsConstAssertion(_) => unsupported_ts(),
        }
    }
}

impl Transform for ext::Literal {
    type Output = Literal;

    fn transform(self, _: &mut ParseContext) -> Result<Self::Output, TxError> {
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
                Literal::Int(int.try_into().or(unsupported("TODO", "TODO"))?)
            }
            ext::LiteralKind::String => Literal::String(
                self.inner_string_text().required()?.to_string(),
            ),
            ext::LiteralKind::Regex => unsupported("TODO", "TODO")?,
        };
        Ok(literal)
    }
}

impl Transform for ext::Template {
    type Output = TemplateLiteral;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
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
    ) -> Result<Self::Output, TxError> {
        let expression = self.expr().required()?.transform_node(context)?;
        Ok(TemplateChunk::Expression(expression))
    }
}

impl Transform for ext::ArrayExpr {
    type Output = ArrayLiteral;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
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
                        .required()?
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
    ) -> Result<Self::Output, TxError> {
        let properties = self.props().transform_all(context)?;
        Ok(ObjectLiteral { properties })
    }
}

impl Transform for ext::ObjectProp {
    type Output = ObjectProperty;

    fn transform(
        self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        match self {
            Self::LiteralProp(literal_prop) => {
                let property =
                    literal_prop.key().required()?.transform_node(context)?;
                let expression =
                    literal_prop.value().required()?.transform_node(context)?;
                Ok(ObjectProperty::Property {
                    property,
                    expression,
                })
            }
            Self::IdentProp(ident_prop) => {
                let identifier =
                    ident_prop.name().required()?.transform_node(context)?;
                Ok(ObjectProperty::Identifier(identifier))
            }
            Self::SpreadProp(spread_prop) => {
                let expression =
                    spread_prop.value().required()?.transform_node(context)?;
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
    ) -> Result<Self::Output, TxError> {
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
                    .required()?
                    .transform_node(context)?;
                Ok(PropertyName::Expression(expression.into()))
            }
            ext::PropName::Literal(literal) => {
                // {"123": ...}
                if let ext::LiteralKind::String = literal.kind() {
                    let s = literal.inner_string_text().required()?.to_string();
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
    ) -> Result<Self::Output, TxError> {
        let expression = self.object().required()?.transform_node(context)?;
        let prop = self.prop().required()?;
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
    ) -> Result<Self::Output, TxError> {
        let expression = self.object().required()?.transform_node(context)?;
        let identifier = self.prop().required()?.transform_node(context)?;

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
    ) -> Result<Self::Output, TxError> {
        let operator = match self.op().required()? {
            ext::UnaryOp::LogicalNot => UnaryOperator::BooleanNot,
            ext::UnaryOp::Plus => UnaryOperator::Plus,
            ext::UnaryOp::Minus => UnaryOperator::Minus,
            ext::UnaryOp::Typeof => UnaryOperator::Typeof,
            ext::UnaryOp::Increment => {
                unsupported("`++`", "Variable mutation is not supported")?
            }
            ext::UnaryOp::Decrement => {
                unsupported("`--`", "Variable mutation is not supported")?
            }
            ext::UnaryOp::Delete => {
                unsupported("`delete`", "Object mutation is not supported")?
            }
            ext::UnaryOp::Void => {
                unsupported("`void`", "Void operator is not supported")?
            }
            ext::UnaryOp::BitwiseNot => {
                unsupported("`~`", "Bitwise operations are not supported")?
            }
            ext::UnaryOp::Await => {
                unsupported("`await`", "Async-await is not supported")?
            }
        };
        let expression = self.expr().required()?.transform_node(context)?;
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
    ) -> Result<Self::Output, TxError> {
        let operator = match self.op().required()? {
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
        let lhs = self.lhs().required()?.transform_node(context)?;
        let rhs = self.rhs().required()?.transform_node(context)?;
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
    ) -> Result<Self::Output, TxError> {
        // TODO handle spread operator here. Doesn't look like rslint supports
        // it
        let arguments =
            self.arguments().required()?.args().transform_all(context)?;
        let function = self.callee().required()?.transform_node(context)?;
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
    ) -> Result<Box<[Node<T::Output>]>, TxError>;
}

impl<T: ext::AstNode + Transform> TransformAll<T> for ext::AstChildren<T> {
    fn transform_all(
        self,
        context: &mut ParseContext,
    ) -> Result<Box<[Node<T::Output>]>, TxError> {
        self.map(|value| value.transform_node(context))
            .collect::<Result<_, _>>()
    }
}

trait OptionExt<T> {
    /// Ensure an AST node is present. Rslint has a very forgiving parser that
    /// allows most AST nodes to be missing, because it's designed to work on
    /// incomplete/incorrect code. We need correct code to execute though, so
    /// this method makes it easy to enforce AST nodes are present.
    fn required(self) -> Result<T, TxError>;

    /// Call [Transform::transform_node] on an optional AST node
    fn transform_node_opt(
        self,
        context: &mut ParseContext,
    ) -> Result<Option<Node<T::Output>>, TxError>
    where
        T: Transform + ext::AstNode;
}

impl<T> OptionExt<T> for Option<T> {
    fn required(self) -> Result<T, TxError> {
        self.ok_or_else(|| TxError {
            node_id: None,
            error: TransformError::Missing {
                expected_type: any::type_name::<T>(),
            },
        })
    }

    fn transform_node_opt(
        self,
        context: &mut ParseContext,
    ) -> Result<Option<Node<T::Output>>, TxError>
    where
        T: Transform + ext::AstNode,
    {
        self.map(|value| value.transform_node(context)).transpose()
    }
}

/// Helper for generating an [Unsupported](TransformError::Unsupported) error,
/// wrapped in a `Result` for Added Convenience
fn unsupported<T>(
    name: &'static str,
    help: &'static str,
) -> Result<T, TxError> {
    Err(TxError {
        node_id: None,
        error: TransformError::Unsupported { name, help },
    })
}

/// TODO get rid of this; just ignore TS features
fn unsupported_ts<T>() -> Result<T, TxError> {
    unsupported(
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::IntoExpression;

    /// Test an escaped newline `\n` in a string literal
    /// ```notrust
    /// "test\nnewline"
    /// // parses to
    /// `test
    /// newline`
    /// ```
    #[test]
    fn string_literal_newline() {
        let source = r#"'test\nnewline'"#;
        let module = parse_test(source);
        assert_eq!(
            module.data(),
            &Module::new(["test\nnewline".into_expr().into()])
        )
    }

    /// Test \ before a literal newline escapes the newline, allowing for
    /// wrapped strings:
    ///
    /// ```notrust
    /// "test \
    /// no newline"
    /// // parses to
    /// "test no newline"
    /// ```
    #[test]
    fn string_literal_escape_newline() {
        let source = r#"'test \
no newline'"#;
        let module = parse_test(source);
        assert_eq!(
            module.data(),
            &Module::new(["test no newline".into_expr().into()])
        )
    }

    fn parse_test(source: &'static str) -> Node<Module> {
        let mut source_table = SourceTable::default();
        source_table.insert(source);
        let (module, _) =
            parse(&mut source_table).unwrap_or_else(|error| panic!("{error}"));
        module
    }
}
