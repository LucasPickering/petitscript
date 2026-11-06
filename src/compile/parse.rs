//! Parse some source code to the PetitScript [ast]
//!
//! This uses [oxc_parser] for the actual parsing, then converts from [oxc_ast]
//! to the simplified PetitScript AST. Simplifying the AST here makes the
//! evaluation engine much simpler and provides errors for invalid syntax
//! up front.

use crate::{
    Source,
    ast::{
        ArrayElement, ArrayLiteral, BinaryOperation, BinaryOperator, Binding,
        Block, Declaration, DoWhileLoop, ExportDeclaration, Expression,
        ForOfLoop, FunctionBody, FunctionCall, FunctionDefinition,
        FunctionParameter, Identifier, If, ImportDeclaration, ImportModule,
        ImportNamed, Literal, Module, Node, NodeId, ObjectLiteral,
        ObjectPatternElement, ObjectProperty, OptionalPropertyAccess,
        PropertyAccess, PropertyName, Statement, TemplateChunk,
        TemplateLiteral, TernaryConditional, UnaryOperation, UnaryOperator,
        Variable, WhileLoop,
    },
    error::{Error, ParseError, TransformError, Unsupported},
    source::{self, SourceId, SourceTable, Span, SpanTable},
};
use oxc_allocator::Allocator;
use oxc_ast::ast::{self as oxc};
use oxc_parser::{Parser, ParserReturn};
use oxc_span::{GetSpan, SourceType};
use std::{ffi::OsStr, ops::Deref, path::PathBuf};

/// File extensions that can be imported as a local module
pub const SUPPORTED_EXTENSIONS: &[&str] = &["js", "ts"];

/// Parse source code into an Abstract Syntax Tree
///
/// Collect all imported sources along the way as well, inserting them into the
/// source table. This assumes the given source table has exactly one source in
/// it to begin with, which is the source of the root module. In addition to the
/// parsed module, this returns a debug table of source spans for each AST node.
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

/// Helper for parsing a single module
///
/// This is called recursively within a parse tree, once per PS module. Each
/// module corresponds to a single source, so this will add a new source to the
/// given source table.
fn parse_module(mut context: ParseContext) -> Result<Node<Module>, Error> {
    // TODO maybe refactor this? the oxc nodes carry their source text so we
    // may be able to simplify the context instead of carrying source around
    let source = context.current_source();
    let code = source.text()?.into_owned();

    // Parse via oxc
    let source_type = source
        .path()
        .and_then(|path| SourceType::from_path(path).ok())
        .unwrap_or(SourceType::mjs());
    let allocator = Allocator::default();
    let ParserReturn {
        program,
        diagnostics,
        fatal_error,
        ..
    } = Parser::new(&allocator, &code, source_type).parse();

    // Check for errors
    if !diagnostics.is_empty() {
        return Err(ParseError {
            source_name: source.path().and_then(source::display_source_path),
            errors: diagnostics,
        }
        .into());
    }

    // Sanity check: I don't think it can return fatal_error=true if the
    // diagnostics list is empty
    assert!(
        !fatal_error,
        "fatal_error should be false with no diagnostics"
    );

    // Transform to a PS AST
    let module = program.transform_node(&mut context).map_err(|error| {
        Error::Transform {
            error: error.error,
            span: context.sources.qualify(error.span),
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
    /// Next node ID to hand out
    ///
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

    /// Generate a new node ID for a new AST node
    ///
    /// This ID will be unique within the entire AST, including sub-modules.
    /// This will also insert the node's source span into the span table.
    fn node_id(&mut self, span: oxc_span::Span) -> NodeId {
        self.node_id_inner(self.transform_span(span))
    }

    /// Map an [oxc_span::Span] to a PetitScript [Span]
    fn transform_span(&self, span: oxc_span::Span) -> Span {
        Span {
            source_id: self.current_source_id,
            // Assuming this will never run on a <32-bit platform
            start_offset: span.start as usize,
            end_offset: span.end as usize,
        }
    }

    // TODO combine with node_id(). Maybe a separate fn to map oxc->our Span
    fn node_id_inner(&mut self, span: Span) -> NodeId {
        let id = NodeId::new(*self.next_node_id);
        *self.next_node_id += 1;
        self.spans.insert(id, span);
        id
    }
}

/// An error that occurs during transformation
///
/// The name is pretty arbitrary, I wanted something short since it's used a lot
/// and only internally.
struct TxError {
    /// Source location of the node that failed to transform
    span: Span,
    error: TransformError,
}

impl TxError {
    /// Attach a node ID and span to an error
    fn new<T>(context: &ParseContext, node: &T, error: TransformError) -> Self
    where
        T: GetSpan,
    {
        Self {
            span: context.transform_span(node.span()),
            error,
        }
    }
}

/// Helper for returning an unsupported error
///
/// Context and node are needed to attach the source span.
macro_rules! unsupported {
    ($context:expr, $node:expr, $reason:expr) => {
        return Err(TxError::new(
            $context,
            $node,
            TransformError::Unsupported($reason),
        ))
    };
}

/// Transform an [oxc_ast] node to a local [ast](crate::ast) node
trait Transform {
    type Output;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError>;

    /// Transform the content of an AST node and wrap it in a [Node]
    ///
    /// This will generate the needed node ID and attach it to the generated
    /// node.
    fn transform_node(
        &self,
        context: &mut ParseContext,
    ) -> Result<Node<Self::Output>, TxError>
    where
        Self: GetSpan,
    {
        self.transform(context)
            .map(|node| Node::new(context.node_id(self.span()), node))
    }
}

impl Transform for oxc::Program<'_> {
    type Output = Module;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let statements = self.body.transform_all(context)?;
        Ok(Module { statements })
    }
}

impl Transform for oxc::Statement<'_> {
    // TODO return option and eliminate Statement::Empty
    type Output = Statement;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        match self {
            // Imports/exports
            Self::ImportDeclaration(import_decl) => {
                import_decl.transform_node(context).map(Statement::Import)
            }
            Self::ExportDeclaration(export_decl) => {
                export_decl.transform_node(context).map(Statement::Export)
            }
            Self::ExportAllDeclaration(_) => todo!(),
            Self::ExportDefaultDeclaration(export_decl) => {
                export_decl.transform_node(context).map(Statement::Export)
            }
            Self::ExportNamedDeclaration(_) => todo!(),
            Self::ExportFromDeclaration(_) => todo!(),

            // Declarations
            Self::VariableDeclaration(decl) => {
                decl.transform_node(context).map(Statement::Declaration)
            }
            Self::FunctionDeclaration(function_decl) => {
                let declaration = function_decl.transform_node(context)?;
                Ok(match declaration.transpose() {
                    Some(declaration) => Statement::Declaration(declaration),
                    None => Statement::Empty,
                })
            }

            // Generic
            Self::EmptyStatement(_) => Ok(Statement::Empty),
            Self::ExpressionStatement(expr_stmt) => expr_stmt
                .expression
                .transform_node(context)
                .map(Statement::Expression),
            Self::BlockStatement(block_stmt) => {
                block_stmt.transform_node(context).map(Statement::Block)
            }

            // Control flow
            Self::IfStatement(if_stmt) => {
                if_stmt.transform_node(context).map(Statement::If)
            }
            Self::DoWhileStatement(do_while_stmt) => do_while_stmt
                .transform_node(context)
                .map(Statement::DoWhileLoop),
            Self::WhileStatement(while_stmt) => {
                while_stmt.transform_node(context).map(Statement::WhileLoop)
            }
            Self::ForOfStatement(for_of_stmt) => for_of_stmt
                .transform_node(context)
                .map(Statement::ForOfLoop),
            // TODO error if labels are present
            Self::ContinueStatement(_) => Ok(Statement::Continue),
            Self::BreakStatement(_) => Ok(Statement::Break),
            Self::ReturnStatement(return_stmt) => {
                let expression = return_stmt
                    .argument
                    .as_ref()
                    .transform_node_opt(context)?;
                Ok(Statement::Return(expression))
            }

            // Unsupported
            Self::ForStatement(_) => {
                unsupported!(context, self, Unsupported::Todo);
            }
            Self::ForInStatement(_) => {
                unsupported!(context, self, Unsupported::ForIn);
            }
            Self::LabeledStatement(_) => {
                unsupported!(context, self, Unsupported::Todo);
            }
            Self::SwitchStatement(_) => {
                unsupported!(context, self, Unsupported::Todo);
            }
            Self::ThrowStatement(_) => {
                unsupported!(context, self, Unsupported::Todo);
            }
            Self::TryStatement(_) => {
                unsupported!(context, self, Unsupported::Todo);
            }
            Self::DebuggerStatement(_) => {
                unsupported!(context, self, Unsupported::Todo);
            }
            Self::WithStatement(_) => {
                unsupported!(context, self, Unsupported::Todo);
            }
            Self::ClassDeclaration(_) => {
                unsupported!(context, self, Unsupported::Class);
            }

            // TODO we should just ignore TS instead of returning an error
            Self::TSEnumDeclaration(_)
            | Self::TSExportAssignment(_)
            | Self::TSExternalModuleDeclaration(_)
            | Self::TSGlobalDeclaration(_)
            | Self::TSImportEqualsDeclaration(_)
            | Self::TSInterfaceDeclaration(_)
            | Self::TSNamespaceDeclaration(_)
            | Self::TSNamespaceExportDeclaration(_)
            | Self::TSTypeAliasDeclaration(_) => {
                unsupported!(context, self, Unsupported::TypeScript);
            }
        }
    }
}

/// TODO
impl Transform for oxc::ImportDeclaration<'_> {
    type Output = ImportDeclaration;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        // TODO only allow imports at top level

        // TODO explain
        let mut default: Option<Node<Identifier>> = None;
        let mut named: Vec<Node<ImportNamed>> = vec![];
        let Some(specifiers) = &self.specifiers else {
            todo!("bare imports not allowed")
        };
        // Each specifier is one element between `import` and `from`
        for specifier in specifiers {
            match specifier {
                // import * as local from 'source'
                //        ^^^^^^^^^^
                oxc::ImportDeclarationSpecifier::ImportNamespaceSpecifier(
                    _,
                ) => {
                    todo!("unsupported")
                }
                // import { a } from 'source'
                //          ^
                oxc::ImportDeclarationSpecifier::ImportSpecifier(specifier) => {
                    named.push(specifier.transform_node(context)?);
                }
                // import local from 'source'
                //        ^^^^^
                oxc::ImportDeclarationSpecifier::ImportDefaultSpecifier(
                    specifier,
                ) => {
                    let identifier = specifier.local.transform_node(context)?;
                    // TODO handle error if there's already a default import
                    default.replace(identifier);
                }
            }
        }

        let source_text = self.source.value.as_str().expect("TODO").to_string();

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
                        .map_err(|error| {
                            TxError::new(
                                context,
                                self,
                                TransformError::Import(error.into()),
                            )
                        })?;
                ImportModule::Local(module)
            }
        }
        .into_node(context, self.source.span());

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

/// TODO
impl Transform for oxc::ImportSpecifier<'_> {
    type Output = ImportNamed;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        match self.import_kind {
            oxc::ImportOrExportKind::Value => {
                // ES supports arbitrary unicode as the import source, but I
                // don't want to support that
                let Some(imported) = self.imported.identifier_name() else {
                    unsupported!(context, self, Unsupported::Todo);
                };
                let imported_span = self.imported.span();
                let imported =
                    transform_ident(imported).into_node(context, imported_span);
                let local = self.local.transform_node(context)?;

                Ok(ImportNamed { imported, local })
            }
            oxc::ImportOrExportKind::Type => {
                unsupported!(context, self, Unsupported::TypeScript);
            }
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

impl Transform for oxc::ExportDeclaration<'_> {
    type Output = ExportDeclaration;

    fn transform(&self, _: &mut ParseContext) -> Result<Self::Output, TxError> {
        todo!()
    }
}

impl Transform for oxc::ExportDefaultDeclaration<'_> {
    type Output = ExportDeclaration;

    fn transform(&self, _: &mut ParseContext) -> Result<Self::Output, TxError> {
        todo!()
    }
}

impl Transform for oxc::BlockStatement<'_> {
    type Output = Block;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let statements = self.body.transform_all(context)?;
        Ok(Block { statements })
    }
}

impl Transform for oxc::IfStatement<'_> {
    type Output = If;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let condition = self.test.transform_node(context)?;
        // This body is NOT optional
        let body = self.consequent.transform_node(context)?;
        // This body IS optional
        let else_body = self.alternate.as_ref().transform_node_opt(context)?;
        Ok(If {
            condition,
            body: body.into(),
            else_body: else_body.map(Box::new),
        })
    }
}

impl Transform for oxc::DoWhileStatement<'_> {
    type Output = DoWhileLoop;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let condition = self.test.transform_node(context)?;
        let body = self.body.transform_node(context)?;
        Ok(DoWhileLoop {
            condition,
            body: Box::new(body),
        })
    }
}

impl Transform for oxc::WhileStatement<'_> {
    type Output = WhileLoop;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let condition = self.test.transform_node(context)?;
        let body = self.body.transform_node(context)?;
        Ok(WhileLoop {
            condition,
            body: Box::new(body),
        })
    }
}

impl Transform for oxc::ForOfStatement<'_> {
    type Output = ForOfLoop;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        if self.r#await {
            unsupported!(context, self, Unsupported::Async);
        }
        let binding = self.left.transform_node(context)?;
        let iterable = self.right.transform_node(context)?;
        let body = self.body.transform_node(context)?;
        Ok(ForOfLoop {
            binding,
            iterable,
            body: Box::new(body),
        })
    }
}

/// Left side (binding) of a `for of` loop
impl Transform for oxc::ForStatementLeft<'_> {
    type Output = Binding;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        match self {
            Self::VariableDeclaration(variable_declaration) => {
                // We expect a single variable with no init expression. It's
                // possible to pass 0 or 2 variables though, and define an init
                // expression, so we need to catch those cases here.
                let declaration = variable_declaration.transform(context)?;
                let Ok([variable]) =
                    <Box<[_; 1]>>::try_from(declaration.variables).map(|b| *b)
                else {
                    todo!("expected exactly 1 variable")
                };
                match variable.into_data() {
                    Variable {
                        binding,
                        init: None,
                    } => Ok(binding.into_data()),
                    _ => todo!("expected no init expression"),
                }
            }
            // for (x of arr) {} (reassigns to x)
            Self::AssignmentTargetIdentifier(_) => {
                unsupported!(context, self, Unsupported::Mutation)
            }
            // No idea if/how these are accessible
            Self::TSAsExpression(_) => todo!(),
            Self::TSSatisfiesExpression(_) => todo!(),
            Self::TSNonNullExpression(_) => todo!(),
            Self::TSTypeAssertion(_) => todo!(),
            Self::ComputedMemberExpression(_) => todo!(),
            Self::StaticMemberExpression(_) => todo!(),
            Self::PrivateFieldExpression(_) => todo!(),
            Self::ArrayAssignmentTarget(_) => todo!(),
            Self::ObjectAssignmentTarget(_) => todo!(),
        }
    }
}

impl Transform for oxc::VariableDeclaration<'_> {
    type Output = Declaration;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        match self.kind {
            oxc::VariableDeclarationKind::Const => {
                let variables = self.declarations.transform_all(context)?;
                Ok(Declaration { variables })
            }
            oxc::VariableDeclarationKind::Var
            | oxc::VariableDeclarationKind::Let => {
                unsupported!(context, self, Unsupported::Mutation);
            }
            oxc::VariableDeclarationKind::Using
            | oxc::VariableDeclarationKind::AwaitUsing => {
                unsupported!(context, self, Unsupported::Using);
            }
        }
    }
}

impl Transform for oxc::VariableDeclarator<'_> {
    type Output = Variable;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        // Can't have an assignment binding here because the right side of the
        // = is stored in self.init
        let binding = self.id.transform(context)?.try_into_binding()?;

        let init = self
            .init
            .as_ref()
            .transform_node_opt(context)?
            .map(Box::new);
        Ok(Variable { binding, init })
    }
}

/// Pattern for declaring variables
///
/// This is used where variables are declared, like `const` or function params.
/// It's also recursive because of array/object unpack patterns.
///
/// The docs for [oxc::BindingPattern] are really helpful.
impl Transform for oxc::BindingPattern<'_> {
    /// TODO explain temp type
    type Output = BindingPattern;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        match self {
            Self::BindingIdentifier(binding_identifier) => {
                let binding = binding_identifier
                    .transform_node(context)?
                    .wrap(context, Binding::Identifier);
                Ok(BindingPattern {
                    binding,
                    expression: None,
                })
            }
            Self::ArrayPattern(_) => todo!(),
            // TODO check `rest` field
            Self::ObjectPattern(object_pattern) => {
                let properties =
                    object_pattern.properties.transform_all(context)?;
                Ok(BindingPattern {
                    binding: Binding::Object(properties)
                        .into_node(context, object_pattern.span()),
                    expression: None,
                })
            }
            // Pattern with an initializer expression, like `{x = 3}`. As far
            // as I can tell, this is only possible for a nested binding within
            // an array/object unpack. Function params and declarations use
            // different constructs to track the right side of the `=`.
            Self::AssignmentPattern(assignment_pattern) => {
                // Recursion!
                let binding = assignment_pattern
                    .left
                    .transform(context)?
                    // I think a nested assignment expression is illegal
                    .try_into_binding()?;
                let expression =
                    assignment_pattern.right.transform_node(context)?;
                Ok(BindingPattern {
                    binding,
                    expression: Some(expression),
                })
            }
        }
    }
}

/// TODO explain difference from IdentifierName/IdentifierReference
impl Transform for oxc::BindingIdentifier<'_> {
    type Output = Identifier;

    fn transform(&self, _: &mut ParseContext) -> Result<Self::Output, TxError> {
        Ok(transform_ident(self.name))
    }
}

/// PS doesn't differentiate between identifier declarations and references, so
/// this just maps to [Identifier]
impl Transform for oxc::IdentifierName<'_> {
    type Output = Identifier;

    fn transform(&self, _: &mut ParseContext) -> Result<Self::Output, TxError> {
        Ok(transform_ident(self.name))
    }
}

/// PS doesn't differentiate between identifier declarations and references, so
/// this just maps to [Identifier]
impl Transform for oxc::IdentifierReference<'_> {
    type Output = Identifier;

    fn transform(&self, _: &mut ParseContext) -> Result<Self::Output, TxError> {
        Ok(transform_ident(self.name))
    }
}

/// One property in an object binding
///
///
/// Here's an example of three bindings:
///
/// ```norust
/// const { x, y: y1, z: z1 = 3 } = obj;
///          ^  ^^^^^  ^^^^^^^^^
/// ```
///
/// This does *not* include `...rest` bindings; those are handled one layer
/// above, in the object pattern.
impl Transform for oxc::BindingProperty<'_> {
    type Output = ObjectPatternElement;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let key = self.key.transform_node(context)?;
        // Value binding and init expression both come from this transformation
        let BindingPattern {
            binding: value,
            expression: init,
        } = self.value.transform(context)?;

        Ok(ObjectPatternElement::Mapped { key, value, init })
    }
}

/// `function` declaration
///
/// This maps to the same AST node as an arrow function bound via `const`.
/// Return `None` for function declarations and overloads:
///
/// ```norust
/// declare function foo(a: number): number;
/// function bar(a: number): number;
/// ```
impl Transform for oxc::Function<'_> {
    type Output = Option<Declaration>;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        // Check illegal function types
        if self.r#async {
            unsupported!(context, self, Unsupported::Async);
        }
        if self.generator {
            unsupported!(context, self, Unsupported::Generator);
        }

        // A bare function is an expression rather than a statement. Allowing
        // it here would make this more complicated.
        let Some(identifier) = &self.id else {
            unsupported!(context, self, Unsupported::AnonymousFunction);
        };
        let identifier = identifier.transform_node(context)?;

        // Functions without a body are just type declarations, so ignore them
        let Some(body) = &self.body else {
            return Ok(None);
        };
        let body = body.transform(context)?;
        let parameters = self.params.transform(context)?;

        // Declare the function as a variable. There is no separate function
        // declaration in the PS AST, for simplicity
        let function = FunctionDefinition {
            name: Some(identifier.clone()),
            parameters,
            body,
            captures: [].into(), // Captures are populated post-parse
        }
        .into_node(context, self.span());
        let variable = Variable {
            binding: identifier.wrap(context, Binding::Identifier),
            init: Some(function.wrap(context, Expression::Function).into()),
        }
        .into_node(context, self.span());

        Ok(Some(Declaration {
            variables: Box::new([variable]),
        }))
    }
}

/// Arrow function body: `(...) => {...}` or `(...) => <expr>`
///
/// This does *not* include the declaration; just the expression.
impl Transform for oxc::ArrowFunctionExpression<'_> {
    type Output = FunctionDefinition;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let parameters = self.params.transform(context)?;
        let body = match &self.body {
            // block w/ braces
            oxc::ArrowFunctionBody::FunctionBody(body) => {
                body.transform(context)?
            }
            // bare expression
            _ => {
                let expression = self
                    .body
                    .as_expression()
                    .expect("Unhandled non-expression branch")
                    .transform_node(context)?;
                FunctionBody::Expression(Box::new(expression))
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

/// Body block of a `function` or a `=>` function
///
/// This always produces a [FunctionBody::Block]
impl Transform for oxc::FunctionBody<'_> {
    type Output = FunctionBody;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        // directives are ignored
        let block = Block {
            statements: self.statements.transform_all(context)?,
        };
        Ok(FunctionBody::Block(block.into_node(context, self.span())))
    }
}

impl Transform for oxc::FormalParameters<'_> {
    type Output = Box<[Node<FunctionParameter>]>;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        // TODO is the `kind` field important? what do with it?
        self.items
            .iter()
            .map(|parameter| parameter.transform_node(context))
            .collect::<Result<_, _>>()
    }
}

impl Transform for oxc::FormalParameter<'_> {
    type Output = FunctionParameter;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        // Type and visibility annotations are ignored because they can
        // provide value at compile time, outside the PS runtime

        // Can't have an assignment binding here because the right side of the
        // = is stored in self.init
        let binding = self.pattern.transform(context)?.try_into_binding()?;

        let init = if let Some(initializer) = &self.initializer {
            Some(Box::new(initializer.transform_node(context)?))
        } else {
            None
        };
        Ok(FunctionParameter {
            variable: Variable { binding, init }
                .into_node(context, self.span()),
            varargs: false,
        })
    }
}

impl Transform for oxc::Expression<'_> {
    type Output = Expression;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        match self {
            Self::NullLiteral(literal) => {
                literal.transform_node(context).map(Expression::Literal)
            }
            Self::BooleanLiteral(literal) => {
                literal.transform_node(context).map(Expression::Literal)
            }
            Self::NumericLiteral(literal) => {
                literal.transform_node(context).map(Expression::Literal)
            }
            Self::BigIntLiteral(_) => {
                unsupported!(context, self, Unsupported::Todo);
            }
            Self::StringLiteral(literal) => {
                literal.transform_node(context).map(Expression::Literal)
            }
            Self::RegExpLiteral(_) => {
                unsupported!(context, self, Unsupported::Todo);
            }
            Self::TemplateLiteral(template) => {
                template.transform_node(context).map(Expression::Template)
            }
            Self::Identifier(reference) => {
                if reference.is_undefined() {
                    // undefined is an identifier, not a literal, in ES
                    Ok(Literal::Undefined.into())
                } else {
                    reference
                        .transform_node(context)
                        .map(Expression::Identifier)
                }
            }
            Self::ArrayExpression(array_expression) => {
                // Span needs to wrap both the Array and the outer Literal
                let array = array_expression.transform_node(context)?;
                Ok(Expression::Literal(
                    Literal::Array(array)
                        .into_node(context, array_expression.span()),
                ))
            }
            Self::ObjectExpression(object_expression) => {
                // Span needs to wrap both the Object and the outer Literal
                let object = object_expression.transform_node(context)?;
                Ok(Expression::Literal(
                    Literal::Object(object)
                        .into_node(context, object_expression.span()),
                ))
            }
            Self::ParenthesizedExpression(parenthesized) => {
                parenthesized.expression.transform(context)
            }
            Self::ComputedMemberExpression(expression) => {
                expression.transform_node(context).map(Expression::Property)
            }
            Self::StaticMemberExpression(expression) => {
                expression.transform_node(context).map(Expression::Property)
            }
            Self::ChainExpression(chain_expression) => {
                chain_expression.transform(context)
            }
            Self::CallExpression(call_expression) => call_expression
                .transform_node(context)
                .map(Expression::Call),
            Self::UnaryExpression(unary_expression) => unary_expression
                .transform_node(context)
                .map(Expression::Unary),
            Self::BinaryExpression(binary_expression) => binary_expression
                .transform_node(context)
                .map(Expression::Binary),
            // Logical binary operators are in a separate type
            Self::LogicalExpression(logical_expression) => logical_expression
                .transform_node(context)
                .map(Expression::Binary),
            Self::ConditionalExpression(conditional_expression) => {
                conditional_expression
                    .transform_node(context)
                    .map(Expression::Ternary)
            }
            Self::SequenceExpression(_) => todo!(),
            Self::ArrowFunctionExpression(arrow_expr) => {
                arrow_expr.transform_node(context).map(Expression::Function)
            }
            Self::FunctionExpression(_) => todo!("wtf is this?"),

            // Unsupported
            Self::AssignmentExpression(_) => {
                unsupported!(context, self, Unsupported::Mutation);
            }
            Self::UpdateExpression(_) => {
                unsupported!(context, self, Unsupported::Mutation);
            }
            Self::ThisExpression(_) => {
                unsupported!(context, self, Unsupported::This);
            }
            Self::NewExpression(_)
            | Self::Super(_)
            | Self::ClassExpression(_)
            | Self::NewTarget(_) => {
                unsupported!(context, self, Unsupported::Class);
            }
            Self::PrivateFieldExpression(_) | Self::PrivateInExpression(_) => {
                unsupported!(context, self, Unsupported::Private);
            }
            Self::ImportMeta(_) => {
                unsupported!(context, self, Unsupported::Todo);
            }
            Self::ImportExpression(_) => {
                unsupported!(context, self, Unsupported::ImportExpression);
            }
            Self::AwaitExpression(_) => {
                unsupported!(context, self, Unsupported::Async);
            }
            Self::YieldExpression(_) => {
                unsupported!(context, self, Unsupported::Generator);
            }
            Self::TaggedTemplateExpression(_) => {
                unsupported!(context, self, Unsupported::TaggedTemplate);
            }
            Self::JSXElement(_) | Self::JSXFragment(_) => {
                unsupported!(context, self, Unsupported::Jsx);
            }
            Self::V8IntrinsicExpression(_) => {
                unsupported!(context, self, Unsupported::Todo);
            }
            Self::TSAsExpression(_)
            | Self::TSInstantiationExpression(_)
            | Self::TSNonNullExpression(_)
            | Self::TSSatisfiesExpression(_)
            | Self::TSTypeAssertion(_) => {
                unsupported!(context, self, Unsupported::TypeScript);
            }
        }
    }
}

impl Transform for oxc::BooleanLiteral {
    type Output = Literal;

    fn transform(&self, _: &mut ParseContext) -> Result<Self::Output, TxError> {
        Ok(Literal::Boolean(self.value))
    }
}

impl Transform for oxc::NullLiteral {
    type Output = Literal;

    fn transform(&self, _: &mut ParseContext) -> Result<Self::Output, TxError> {
        Ok(Literal::Null)
    }
}

impl Transform for oxc::NumericLiteral<'_> {
    type Output = Literal;

    fn transform(&self, _: &mut ParseContext) -> Result<Self::Output, TxError> {
        // oxc reports all numbers as floats. We want to convert
        // anything without a '.' back to ints
        if self
            .raw
            .expect("AST node must have corresponding source")
            .contains('.')
        {
            Ok(Literal::Float(self.value))
        } else {
            // TODO handle floats that are too big
            Ok(Literal::Int(self.value as i64))
        }
    }
}

impl Transform for oxc::StringLiteral<'_> {
    type Output = Literal;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        match self.value.as_str() {
            Some(s) => Ok(Literal::String(s.into())),
            // JS supports invalid UTF-8 but Rust strings don't
            None => unsupported!(context, self, Unsupported::NonUtf),
        }
    }
}

impl Transform for oxc::TemplateLiteral<'_> {
    type Output = TemplateLiteral;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        // oxc provides the quasis (literal chunks) and expressions (dynamic
        // chunks) separately. We're responsible for piecing them back together
        // in order. Collect them all into one sequence, then sort by their
        // source position
        let mut chunks: Vec<(Node<TemplateChunk>, oxc_span::Span)> = self
            .quasis
            .iter()
            .map(|quasi| {
                // TODO check the `cooked` field
                let literal = quasi.value.raw;
                let span = quasi.span;
                (
                    TemplateChunk::Literal(literal.into_string())
                        .into_node(context, span),
                    span,
                )
            })
            .collect();
        chunks.extend(
            // Pair each expression with its range so we can sort
            self.expressions
                .iter()
                .zip(self.expressions.transform_all(context)?)
                .map(|(element, chunk)| {
                    let span = element.span();
                    (
                        TemplateChunk::Expression(chunk)
                            .into_node(context, span),
                        span,
                    )
                }),
        );
        chunks.sort_by_key(|(_, range)| range.start);
        Ok(TemplateLiteral {
            chunks: chunks
                .into_iter()
                .map(|(chunk, _)| chunk)
                .collect::<Vec<_>>()
                .into(),
        })
    }
}

impl Transform for oxc::ArrayExpression<'_> {
    type Output = ArrayLiteral;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let elements = self
            .elements
            .iter()
            .map(|element| match element {
                oxc::ArrayExpressionElement::SpreadElement(spread_element) => {
                    let expression =
                        spread_element.argument.transform_node(context)?;
                    Ok(expression.wrap(context, ArrayElement::Spread))
                }
                oxc::ArrayExpressionElement::Elision(elision) => {
                    unsupported!(
                        context,
                        elision.deref(),
                        Unsupported::ArrayElision
                    );
                }
                _ => {
                    // TODO dedupe this pattern
                    let expression = element
                        .as_expression()
                        .expect("Unhandled non-expression branch")
                        .transform_node(context)?;
                    Ok(expression.wrap(context, ArrayElement::Expression))
                }
            })
            .collect::<Result<_, _>>()?;
        Ok(ArrayLiteral { elements })
    }
}

impl Transform for oxc::ObjectExpression<'_> {
    type Output = ObjectLiteral;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let properties = self.properties.transform_all(context)?;
        Ok(ObjectLiteral { properties })
    }
}

impl Transform for oxc::ObjectPropertyKind<'_> {
    type Output = ObjectProperty;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        match self {
            Self::ObjectProperty(property) => property.transform(context),
            Self::SpreadProperty(element) => {
                let expression = element.argument.transform_node(context)?;
                Ok(ObjectProperty::Spread(expression))
            }
        }
    }
}

impl Transform for oxc::ObjectProperty<'_> {
    type Output = ObjectProperty;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        match self.kind {
            // Getters, setters, and methods aren't allowed
            oxc::PropertyKind::Get | oxc::PropertyKind::Set => {
                unsupported!(context, self, Unsupported::ObjectMethod);
            }
            oxc::PropertyKind::Init if self.method => {
                unsupported!(context, self, Unsupported::ObjectMethod);
            }
            // Normal expression
            oxc::PropertyKind::Init => {
                // Spread elements can't appear here, they're handled above in
                // oxc::ObjectPropertyKind
                let property = self.key.transform_node(context)?;
                let expression = self.value.transform_node(context)?;
                Ok(ObjectProperty::Property {
                    property,
                    expression,
                })
            }
        }
    }
}

impl Transform for oxc::PropertyKey<'_> {
    type Output = PropertyName;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        match self {
            Self::StaticIdentifier(ident) => {
                let identifier = ident.transform_node(context)?;
                Ok(PropertyName::Literal(identifier))
            }
            Self::PrivateIdentifier(_) => {
                unsupported!(context, self, Unsupported::Private);
            }
            _ => {
                let expression = self
                    .as_expression()
                    .expect("Unhandled non-expression branch")
                    .transform_node(context)?;
                Ok(PropertyName::Expression(Box::new(expression)))
            }
        }
    }
}

impl Transform for oxc::ComputedMemberExpression<'_> {
    type Output = PropertyAccess;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let object = self.object.transform_node(context)?;
        let property = self.expression.transform_node(context)?;
        Ok(PropertyAccess {
            object: object.into(),
            property: property
                .wrap(context, |prop| PropertyName::Expression(Box::new(prop))),
        })
    }
}

impl Transform for oxc::StaticMemberExpression<'_> {
    type Output = PropertyAccess;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let object = self.object.transform_node(context)?;
        let property = self.property.transform_node(context)?;

        Ok(PropertyAccess {
            object: object.into(),
            property: property.wrap(context, PropertyName::Literal),
        })
    }
}

/// `?.` operator
///
/// - `a?.b`
/// - `a?.["B"]`
/// - `a.f?.()`
/// - `a?.b!`
impl Transform for oxc::ChainExpression<'_> {
    /// The `?.` operator covers a few different use cases that can map to
    /// multiple different expression types
    type Output = Expression;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let from_property_access =
            |context, property_access: PropertyAccess| -> Expression {
                Expression::OptionalProperty(
                    OptionalPropertyAccess {
                        object: property_access.object,
                        property: property_access.property,
                    }
                    .into_node(context, self.span()),
                )
            };

        match &self.expression {
            oxc::ChainElement::CallExpression(call_expression) => {
                // CallExpression transformer handles the conditional case
                // itself, so we can just forward that
                call_expression
                    .transform_node(context)
                    .map(Expression::Call)
            }
            oxc::ChainElement::ComputedMemberExpression(
                computed_member_expression,
            ) => {
                // Re-use the other transformer
                let property_access =
                    computed_member_expression.transform(context)?;
                Ok(from_property_access(context, property_access))
            }
            oxc::ChainElement::StaticMemberExpression(
                static_member_expression,
            ) => {
                // Re-use the other transformer
                let property_access =
                    static_member_expression.transform(context)?;
                Ok(from_property_access(context, property_access))
            }
            oxc::ChainElement::TSNonNullExpression(non_null_expression) => {
                // Ignore TS assertions, just unpack the inner expression
                non_null_expression.expression.transform(context)
            }
            oxc::ChainElement::PrivateFieldExpression(expression) => {
                unsupported!(context, expression.deref(), Unsupported::Private)
            }
        }
    }
}

impl Transform for oxc::UnaryExpression<'_> {
    type Output = UnaryOperation;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let operator = match self.operator {
            oxc::UnaryOperator::LogicalNot => UnaryOperator::BooleanNot,
            oxc::UnaryOperator::UnaryPlus => UnaryOperator::Plus,
            oxc::UnaryOperator::UnaryNegation => UnaryOperator::Minus,
            oxc::UnaryOperator::Typeof => UnaryOperator::Typeof,
            oxc::UnaryOperator::Delete => {
                unsupported!(context, self, Unsupported::Mutation);
            }
            oxc::UnaryOperator::Void => {
                unsupported!(context, self, Unsupported::Void);
            }
            oxc::UnaryOperator::BitwiseNot => {
                unsupported!(context, self, Unsupported::Bitwise);
            }
        };
        let expression = self.argument.transform_node(context)?;
        Ok(UnaryOperation {
            operator,
            expression: expression.into(),
        })
    }
}

impl Transform for oxc::BinaryExpression<'_> {
    type Output = BinaryOperation;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let operator = match self.operator {
            oxc::BinaryOperator::LessThan => BinaryOperator::LessThan,
            oxc::BinaryOperator::LessEqualThan => BinaryOperator::LessThanEqual,
            oxc::BinaryOperator::GreaterThan => BinaryOperator::GreaterThan,
            oxc::BinaryOperator::GreaterEqualThan => {
                BinaryOperator::GreaterThanEqual
            }
            oxc::BinaryOperator::StrictEquality => BinaryOperator::Equal,
            oxc::BinaryOperator::StrictInequality => BinaryOperator::NotEqual,
            oxc::BinaryOperator::Addition => BinaryOperator::Add,
            oxc::BinaryOperator::Subtraction => BinaryOperator::Sub,
            oxc::BinaryOperator::Multiplication => BinaryOperator::Mul,
            oxc::BinaryOperator::Division => BinaryOperator::Div,
            oxc::BinaryOperator::Remainder => BinaryOperator::Mod,

            // TODO
            oxc::BinaryOperator::In => todo!(),
            oxc::BinaryOperator::Instanceof => todo!(),

            // Unsupported
            oxc::BinaryOperator::Exponential => {
                unsupported!(context, self, Unsupported::Exponent);
            }
            oxc::BinaryOperator::Equality | oxc::BinaryOperator::Inequality => {
                unsupported!(context, self, Unsupported::LooseEquality);
            }
            oxc::BinaryOperator::BitwiseAnd
            | oxc::BinaryOperator::BitwiseOR
            | oxc::BinaryOperator::BitwiseXOR
            | oxc::BinaryOperator::ShiftLeft
            | oxc::BinaryOperator::ShiftRight
            | oxc::BinaryOperator::ShiftRightZeroFill => {
                unsupported!(context, self, Unsupported::Bitwise);
            }
        };
        let lhs = self.left.transform_node(context)?;
        let rhs = self.right.transform_node(context)?;
        Ok(BinaryOperation {
            operator,
            lhs: lhs.into(),
            rhs: rhs.into(),
        })
    }
}

impl Transform for oxc::LogicalExpression<'_> {
    type Output = BinaryOperation;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let operator = match self.operator {
            oxc::LogicalOperator::Or => BinaryOperator::BooleanOr,
            oxc::LogicalOperator::And => BinaryOperator::BooleanAnd,
            oxc::LogicalOperator::Coalesce => BinaryOperator::NullishCoalesce,
        };
        let lhs = self.left.transform_node(context)?;
        let rhs = self.right.transform_node(context)?;
        Ok(BinaryOperation {
            operator,
            lhs: lhs.into(),
            rhs: rhs.into(),
        })
    }
}

impl Transform for oxc::ConditionalExpression<'_> {
    type Output = TernaryConditional;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        let condition = self.test.transform_node(context)?;
        let true_expression = self.consequent.transform_node(context)?;
        let false_expression = self.alternate.transform_node(context)?;
        Ok(TernaryConditional {
            condition: condition.into(),
            true_expression: true_expression.into(),
            false_expression: false_expression.into(),
        })
    }
}

impl Transform for oxc::CallExpression<'_> {
    type Output = FunctionCall;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        // Like this: a.f?.()
        if self.optional {
            todo!("support optional functional calls")
        }

        let arguments = self.arguments.transform_all(context)?;
        let function = self.callee.transform_node(context)?;
        Ok(FunctionCall {
            function: function.into(),
            arguments,
        })
    }
}

impl Transform for oxc::Argument<'_> {
    type Output = Expression;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, TxError> {
        match self {
            Self::SpreadElement(_) => todo!("support spread in function args"),
            // All other variants are inherited from Expression
            _ => self
                .as_expression()
                .expect("Unhandled non-expression branch")
                .transform(context),
        }
    }
}

/// TODO
trait TransformAll<T: Transform> {
    #[allow(clippy::type_complexity)]
    fn transform_all(
        &self,
        context: &mut ParseContext,
    ) -> Result<Box<[Node<T::Output>]>, TxError>;
}

impl<T: GetSpan + Transform> TransformAll<T> for oxc_allocator::Vec<'_, T> {
    fn transform_all(
        &self,
        context: &mut ParseContext,
    ) -> Result<Box<[Node<T::Output>]>, TxError> {
        self.into_iter()
            .map(|value| value.transform_node(context))
            .collect::<Result<_, _>>()
    }
}

trait OptionExt<T> {
    /// Call [Transform::transform_node] on an optional AST node
    fn transform_node_opt(
        &self,
        context: &mut ParseContext,
    ) -> Result<Option<Node<T::Output>>, TxError>
    where
        T: Transform + GetSpan;
}

impl<T> OptionExt<T> for Option<&T> {
    fn transform_node_opt(
        &self,
        context: &mut ParseContext,
    ) -> Result<Option<Node<T::Output>>, TxError>
    where
        T: Transform + GetSpan,
    {
        self.map(|value| value.transform_node(context)).transpose()
    }
}

/// Transform an oxc identifier to a PS identifier
///
/// Panics if the identifier fails to re-parse, which *should* be
/// impossible because we use the same identifier rules.
fn transform_ident(ident: oxc::Ident<'_>) -> Identifier {
    // Error here *should* be impossible because we use the same identifier
    // rules as ES
    ident
        .as_str()
        .parse()
        .expect("Valid ES identifier not accepted by PetitScript")
}

impl<T> Node<T> {
    /// Wrap this node in an additional node that shares the same source span
    ///
    /// Use this when applying a wrapper to a node type that doesn't change the
    /// span of the source code. The wrapped node will get a new ID, but that
    /// ID will point to the same source code as the given node.
    ///
    /// ## Example
    ///
    /// In this code:
    ///
    /// ```norust
    /// const x = 3;
    /// ```
    ///
    /// An [Identifier] node will point to `x`, then that node will be wrapped
    /// in a [Binding]. Both nodes refer to the same source code. You could wrap
    /// the `Identifier` node like so:
    ///
    /// ```notest
    /// let binding = identifier.wrap(context, Binding::Identifier);
    /// ```
    fn wrap<U>(
        self,
        context: &mut ParseContext,
        f: impl FnOnce(Node<T>) -> U,
    ) -> Node<U> {
        let new_id = context.node_id_inner(
            context
                .spans
                .get(self.id())
                .expect("Node must be in span table"),
        );
        Node::new(new_id, f(self))
    }
}

/// Helper for attach an ID to an AST node
trait IntoNode: Sized {
    fn into_node(
        self,
        context: &mut ParseContext,
        span: oxc_span::Span,
    ) -> Node<Self>;
}

impl<T> IntoNode for T {
    fn into_node(
        self,
        context: &mut ParseContext,
        span: oxc_span::Span,
    ) -> Node<Self> {
        Node::new(context.node_id(span), self)
    }
}

/// Temporary type to help with mapping an [oxc::BindingPattern] to a [Binding]
///
/// Some use cases support assignment expressions, which carry an expression on
/// the right side. This type makes it easy to assert those expressions are
/// absent in cases where they're illegal.
struct BindingPattern {
    binding: Node<Binding>,
    /// Right-hand side of an assignment expression
    ///
    /// This is illegal in some cases. See [oxc::BindingPattern] for where.
    expression: Option<Node<Expression>>,
}

impl BindingPattern {
    /// Get the inner binding if there is no expression
    ///
    /// Return an error if the assignment expression is present. Use this for
    /// use cases where the assignment case is illegal.
    fn try_into_binding(self) -> Result<Node<Binding>, TxError> {
        match self {
            Self {
                binding,
                expression: None,
            } => Ok(binding),
            Self { .. } => {
                todo!("error")
            }
        }
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
