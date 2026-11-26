//! Transform an [oxc_ast] AST to a PetitScript [ast](crate::ast)
//!
//! This contains all the transformation logic, but leaves the overhead of
//! initial parsing to the parent.

use crate::{
    NativeModuleName, Source,
    ast::{
        Argument, ArrayElement, ArrayLiteral, ArrayPatternElement,
        ArrayPatternElementKind, BinaryOperation, BinaryOperator, Binding,
        BindingKind, Block, Declaration, ExportDeclaration,
        ExportDeclarationKind, Expression, ExpressionKind, FunctionBody,
        FunctionCall, FunctionDefinition, FunctionParameter, Identifier,
        IdentifierNode, If, ImportDeclaration, ImportModule, ImportModuleKind,
        ImportNamed, Literal, Module, Node, NodeId, ObjectLiteral,
        ObjectPatternElement, ObjectProperty, ObjectPropertyKind,
        OptionalPropertyAccess, PropertyAccess, PropertyName, PropertyNameKind,
        Statement, StatementKind, TemplateChunk, TemplateChunkKind,
        TemplateLiteral, TernaryConditional, UnaryOperation, UnaryOperator,
        Variable,
    },
    compile::{SUPPORTED_EXTENSIONS, parse::ParseContext},
    error::{ParseError, ParseErrorKind, Unsupported},
    source::Span,
    util::OptionExt,
};
use oxc_ast::ast::{self as oxc};
use oxc_span::GetSpan;
use std::{
    ffi::OsStr,
    ops::Deref,
    path::{Path, PathBuf},
};

/// Transform the AST of an ES program into a PS module
///
/// Invalid nodes are dropped and registered as errors. Errors are registered
/// in the context instead of bubbling up. We don't want errors to cancel
/// transformation, they should all be collected and reported together.
///
/// Individual nodes can fail to transform, but multiple-error collection is
/// done via transform_all(). When there's a list of nodes (e.g a list of
/// statements in a block), invalid statements are tossed. Check
/// `context.errors` to if any errors occur.
pub fn transform_module(
    context: &mut ParseContext,
    program: oxc::Program<'_>,
) -> Module {
    let id = context.id(&program);
    let statements = program.body.transform_all(context);
    Module { id, statements }
}

/// Helper for returning an unsupported error
///
/// Context and node are needed to attach the source span.
macro_rules! unsupported {
    ($context:expr, $node:expr, $reason:expr) => {
        return Err($context
            .register_error($node, ParseErrorKind::Unsupported($reason)))
    };
}

/// Downcast an AST node type to an expression by calling its `as_expression`
/// method
///
/// Return an error if the cast fails. The `as_expression` method is not part
/// of a trait, so this has to be implemented as a macro instead of a generic
/// function.
///
/// [oxc_ast] uses a macro-based inheritance system where macro enums inherit
/// variants from other enums. In those cases, the inheriting enum can be
/// casted back to the base enum if the value is one of the inherited variants.
macro_rules! as_expression {
    ($context:expr, $node:expr) => {
        match $node.as_expression() {
            Some(expression) => expression,
            None => {
                return Err($context.register_error(
                    $node,
                    ParseErrorKind::Unhandled {
                        node: format!("{:?}", $node),
                    },
                ));
            }
        }
    };
}

impl ParseContext<'_> {
    /// Generate and register a transformation error at a node
    fn register_error(
        &mut self,
        node: &impl GetSpan,
        error: ParseErrorKind,
    ) -> Reg {
        // Map the span from oxc -> PS -> miette
        // The intermediate mapping is helpful to handle source ID encoding
        let span = self.map_span(node.span());
        self.errors.push(ParseError { error, span });
        Reg
    }

    /// Generate a new node ID for a transformed AST node
    ///
    /// This will convert the given oxc node's span to [`Span`], then insert it
    /// into the table. Use this for all transformed nodes.
    fn id(&mut self, node: &impl GetSpan) -> NodeId {
        self.node_id_inner(self.map_span(node.span()))
    }

    /// Generate a new node ID for a node overlapping an *already transformed
    /// node*
    ///
    /// Use this for nodes that syntactically overlap a node that's already
    /// been transformed, e.g. a [`Binding`] wrapping an [`IdentifierNode`]. The
    /// new node ID will be inserted into the span map with the same span as
    /// the given node.
    fn id_from(&mut self, node: &impl Node) -> NodeId {
        let span = self
            .spans
            .get(node.id())
            .expect("TODO error! node should already be in the map");
        self.node_id_inner(span)
    }

    // TODO combine with node_id(). Maybe a separate fn to map oxc->our Span
    fn node_id_inner(&mut self, span: Span) -> NodeId {
        let id = NodeId(*self.next_node_id);
        *self.next_node_id += 1;
        self.spans.insert(id, span);
        id
    }
}

/// Transform an [oxc_ast] node to a local [ast](crate::ast) node
trait Transform {
    type Output;

    /// Transform the AST node
    ///
    /// If the transformation fails, the error will be registered with the
    /// parse context. [`Reg`] is returned to indicate the error was stored.
    /// Practically, the error variant could just be `()`, but that would make
    /// it very easy to return `()` without actually registering an error.
    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg>;
}

/// Transform a collection of nodes
trait TransformAll<T: Transform> {
    /// Transform a collection of nodes
    ///
    /// Invalid nodes (nodes that generate an error instead of transforming)
    /// will be **skipped** and omitted from the output. This allows the
    /// transformation to collect as many errors as possible, rather than
    /// failing immediately. Any error will cause the top-level transformation
    /// to fail, but the intermediate transformations will continue.
    fn transform_all(&self, context: &mut ParseContext) -> Box<[T::Output]>;
}

/// Transform oxc-provided vecs
impl<T: GetSpan + Transform> TransformAll<T> for oxc_allocator::Vec<'_, T> {
    fn transform_all(&self, context: &mut ParseContext) -> Box<[T::Output]> {
        self.iter()
            // Errors are already registered, so just skip em
            .filter_map(|value| value.transform(context).ok())
            .collect()
    }
}

/// Transform self-constructed vecs
///
/// This is a little hacky but it's the best/simplest solution I could come
/// up with. Can't implement `TransformAll` on an iterator because the iter
/// item needs to support both `T` and `&T`. I think it would work if `GetSpan`
/// had a blanket impl on `&T`.
impl<T: GetSpan + Transform> TransformAll<T> for Vec<T> {
    fn transform_all(&self, context: &mut ParseContext) -> Box<[T::Output]> {
        self.iter()
            .filter_map(|value| value.transform(context).ok())
            .collect()
    }
}

impl Transform for oxc::Statement<'_> {
    type Output = Statement;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        let kind = match self {
            // Imports/exports
            Self::ImportDeclaration(import_declaration) => {
                StatementKind::Import(import_declaration.transform(context)?)
            }
            // export const a = 5;
            Self::ExportDeclaration(export_declaration) => export_declaration
                .transform(context)?
                // Filter out type-only exports
                .map(StatementKind::Export)
                .unwrap_or(StatementKind::Empty),

            // export default c;
            Self::ExportDefaultDeclaration(export_default_declaration) => {
                export_default_declaration
                    .declaration
                    .transform(context)?
                    // Filter out type-only exports
                    .map(StatementKind::Export)
                    .unwrap_or(StatementKind::Empty)
            }

            // Declarations
            Self::VariableDeclaration(variable_declaration) => {
                StatementKind::Declaration(
                    variable_declaration.transform(context)?,
                )
            }

            Self::FunctionDeclaration(function) => function
                .transform(context)?
                .map(StatementKind::Declaration)
                .unwrap_or(StatementKind::Empty),

            // Generic
            Self::EmptyStatement(_) => StatementKind::Empty,
            Self::ExpressionStatement(expr_stmt) => StatementKind::Expression(
                expr_stmt.expression.transform(context)?,
            ),
            Self::BlockStatement(block_stmt) => {
                StatementKind::Block(block_stmt.transform(context)?)
            }

            // Control flow
            Self::IfStatement(if_stmt) => {
                StatementKind::If(if_stmt.transform(context)?)
            }
            Self::ReturnStatement(return_statement) => {
                return_statement.transform(context)?
            }

            // TypeScript - ignore things that don't affect runtime semantics
            Self::TSEnumDeclaration(_)
            | Self::TSExportAssignment(_)
            | Self::TSExternalModuleDeclaration(_)
            | Self::TSGlobalDeclaration(_)
            | Self::TSImportEqualsDeclaration(_)
            | Self::TSInterfaceDeclaration(_)
            | Self::TSNamespaceDeclaration(_)
            | Self::TSNamespaceExportDeclaration(_)
            | Self::TSTypeAliasDeclaration(_) => StatementKind::Empty,

            // Unsupported
            Self::ExportFromDeclaration(_) => {
                // export { c } from 'source';
                unsupported!(context, self, Unsupported::ExportFrom)
            }
            Self::BreakStatement(_)
            | Self::ContinueStatement(_)
            | Self::DoWhileStatement(_)
            | Self::ForStatement(_)
            | Self::ForInStatement(_)
            | Self::ForOfStatement(_)
            | Self::WhileStatement(_) => {
                unsupported!(context, self, Unsupported::Loop);
            }
            Self::LabeledStatement(_) => {
                unsupported!(context, self, Unsupported::Label);
            }
            Self::SwitchStatement(_) => {
                unsupported!(context, self, Unsupported::Switch);
            }
            Self::ThrowStatement(_) => {
                unsupported!(context, self, Unsupported::Exception);
            }
            Self::TryStatement(_) => {
                unsupported!(context, self, Unsupported::Exception);
            }
            Self::DebuggerStatement(_) => {
                unsupported!(context, self, Unsupported::Debugger);
            }
            Self::WithStatement(_) => {
                unsupported!(context, self, Unsupported::With);
            }
            Self::ClassDeclaration(_) => {
                unsupported!(context, self, Unsupported::Class);
            }

            // export * as c from 'source';
            Self::ExportAllDeclaration(_) => {
                unsupported!(context, self, Unsupported::ExportFrom);
            }
            // const c = 4; export { c };
            // There may be value in allowing this, but I'm not for now
            Self::ExportNamedDeclaration(_) => {
                unsupported!(
                    context,
                    self,
                    Unsupported::ExportNamedDeclaration
                );
            }
        };
        Ok(Statement {
            id: context.id(self),
            kind,
        })
    }
}

impl Transform for oxc::ImportDeclaration<'_> {
    type Output = ImportDeclaration;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        // TODO only allow imports at top level

        // There can only be zero or one default per import statement, plus any
        // number of named imports
        let mut default: Option<IdentifierNode> = None;
        let mut named: Vec<ImportNamed> = vec![];
        let Some(specifiers) = &self.specifiers else {
            // None for `import 'source'`
            unsupported!(context, self, Unsupported::BareImport);
        };

        // Each specifier is one element between `import` and `from`
        for specifier in specifiers {
            match specifier {
                // import * as local from 'source'
                //        ^^^^^^^^^^
                oxc::ImportDeclarationSpecifier::ImportNamespaceSpecifier(
                    _,
                ) => {
                    unsupported!(context, self, Unsupported::NamespaceImport);
                }
                // import { a } from 'source'
                //          ^
                oxc::ImportDeclarationSpecifier::ImportSpecifier(specifier) => {
                    let import_named = specifier.transform(context)?;
                    // Filter out type imports, which will be None
                    if let Some(import_named) = import_named {
                        named.push(import_named);
                    }
                }
                // import local from 'source'
                //        ^^^^^
                oxc::ImportDeclarationSpecifier::ImportDefaultSpecifier(
                    specifier,
                ) => {
                    let identifier = specifier.local.transform(context)?;
                    // TODO handle error if there's already a default import
                    default.replace(identifier);
                }
            }
        }

        let source_text = self.source.value.as_str().expect("TODO").to_string();

        let source_kind = match NativeModuleName::try_from(source_text) {
            // If it's a valid native module name, treat it as such. Otherwise
            // treat it as a path. A path is always going to have a character
            // like . or / that will make it an invalid module name
            Ok(name) => ImportModuleKind::Native(name),
            Err(error) => {
                // We're importing a file path. Resolve it to an absolute path
                let name = error.name;
                let source = resolve_module(&name, context.current_source())
                    .and_then(|path| {
                        // Load source code
                        Source::file(path).map_err(ParseErrorKind::from)
                    })
                    .map_err(|error| {
                        context.register_error(&self.source, error)
                    })?;
                ImportModuleKind::Local {
                    name,
                    // Store this source and it will be parsed later
                    source: context.insert_source(source),
                }
            }
        };
        let module = ImportModule {
            id: context.id(&self.source),
            kind: source_kind,
        };

        match (default, named.as_slice()) {
            // This branch should be impossible because an import without
            // specifiers leads to an AST node that would be rejected above
            (None, &[]) => unreachable!("no imports specified"),
            (default, _) => Ok(ImportDeclaration {
                id: context.id(self),
                default,
                named: named.into(),
                module,
            }),
        }
    }
}

/// Transform one specifier of an import statement
///
/// See [oxc::ImportSpecifier] docs.
impl Transform for oxc::ImportSpecifier<'_> {
    /// `None` for TypeScript type imports
    type Output = Option<ImportNamed>;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        match self.import_kind {
            oxc::ImportOrExportKind::Value => {
                // ES supports arbitrary unicode as the import source, but I
                // don't want to support that
                let Some(imported) = self.imported.identifier_name() else {
                    unsupported!(
                        context,
                        self,
                        Unsupported::ImportStringIdentifier
                    );
                };
                let imported = IdentifierNode {
                    id: context.id(&self.imported),
                    identifier: transform_ident(imported).map_err(|error| {
                        context.register_error(&self.imported, error)
                    })?,
                };
                let local = self.local.transform(context)?;

                Ok(Some(ImportNamed {
                    id: context.id(self),
                    imported,
                    local,
                }))
            }
            oxc::ImportOrExportKind::Type => Ok(None),
        }
    }
}

/// Resolve a local module specifier into a path
///
/// This will resolve relative paths to be relative to the importing module.
/// This does not guarantee the file is actually present!
///
/// # Errors
///
/// Error if the source does not allow local imports (e.g. in-memory string
/// source) or the imported path is not an allowed extension.
fn resolve_module(
    specifier: &str,
    parent_source: &Source,
) -> Result<PathBuf, ParseErrorKind> {
    let path = Path::new(specifier);

    if path.is_absolute() {
        return Err(ParseErrorKind::ImportAbsolute {
            path: path.to_owned(),
        });
    }

    // The import is relative to the parent directory of this file
    let relative_to = match parent_source {
        Source::String { root, .. } => root,
        Source::File { path, .. } => path.parent().expect("TODO handle error"),
    };
    let path = relative_to.join(path);

    // Ensure the path is a file with a supported extension
    if path
        .extension()
        .and_then(OsStr::to_str)
        .is_some_and(|extension| SUPPORTED_EXTENSIONS.contains(&extension))
    {
        Ok(path)
    } else {
        Err(ParseErrorKind::ImportInvalidExtension { path })
    }
}

/// `export const a = 5;`
impl Transform for oxc::ExportDeclaration<'_> {
    /// `None` for TypeScript type exports like `export interface T {...}`
    type Output = Option<ExportDeclaration>;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        let declaration = self.declaration.transform(context)?;
        Ok(declaration.map(|declaration| ExportDeclaration {
            id: context.id(self),
            kind: ExportDeclarationKind::Declaration(declaration),
        }))
    }
}

/// `export default c;`
impl Transform for oxc::ExportDefaultDeclarationKind<'_> {
    /// `None` for TypeScript exports
    type Output = Option<ExportDeclaration>;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        match self {
            // export function() { ... } (ILLEGAL)
            Self::FunctionDeclaration(_) => {
                unsupported!(context, self, Unsupported::AnonymousFunction);
            }
            Self::ClassDeclaration(_) => {
                unsupported!(context, self, Unsupported::Class);
            }
            Self::TSInterfaceDeclaration(_) => Ok(None),
            // Everything else is an expression
            _ => {
                let expression =
                    as_expression!(context, self).transform(context)?;
                Ok(Some(ExportDeclaration {
                    id: context.id(self),
                    kind: ExportDeclarationKind::DefaultExpression(expression),
                }))
            }
        }
    }
}

impl Transform for oxc::Declaration<'_> {
    /// `None` for TypeScript type declarations
    type Output = Option<Declaration>;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        match self {
            Self::VariableDeclaration(variable_declaration) => {
                let declaration = variable_declaration.transform(context)?;
                Ok(Some(declaration))
            }
            Self::FunctionDeclaration(function) => {
                let declaration = function.transform(context)?;
                Ok(declaration)
            }
            Self::ClassDeclaration(_) => {
                unsupported!(context, self, Unsupported::Class)
            }
            Self::TSTypeAliasDeclaration(_)
            | Self::TSInterfaceDeclaration(_)
            | Self::TSEnumDeclaration(_)
            | Self::TSExternalModuleDeclaration(_)
            | Self::TSNamespaceDeclaration(_)
            | Self::TSGlobalDeclaration(_)
            | Self::TSImportEqualsDeclaration(_) => Ok(None),
        }
    }
}

impl Transform for oxc::BlockStatement<'_> {
    type Output = Block;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        let statements = self.body.transform_all(context);
        Ok(Block {
            id: context.id(self),
            statements,
        })
    }
}

impl Transform for oxc::IfStatement<'_> {
    type Output = If;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        let condition = self.test.transform(context)?;
        // This body is NOT optional
        let body = self.consequent.transform(context)?;
        // This body IS optional
        let else_body = if let Some(alternate) = &self.alternate {
            Some(alternate.transform(context)?)
        } else {
            None
        };
        Ok(If {
            id: context.id(self),
            condition,
            body: body.into(),
            else_body: else_body.map(Box::new),
        })
    }
}

impl Transform for oxc::ReturnStatement<'_> {
    type Output = StatementKind;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        let expression = if let Some(argument) = &self.argument {
            Some(argument.transform(context)?)
        } else {
            None
        };
        Ok(StatementKind::Return(expression))
    }
}

impl Transform for oxc::VariableDeclaration<'_> {
    type Output = Declaration;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        match self.kind {
            oxc::VariableDeclarationKind::Const => {
                let variables = self.declarations.transform_all(context);
                Ok(Declaration {
                    id: context.id(self),
                    variables,
                })
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
    ) -> Result<Self::Output, Reg> {
        // Can't have an assignment binding here because the right side of the
        // = is stored in self.init
        let binding = BindingOnly(&self.id).transform(context)?;

        // = must be present. SHOULD be guaranteed by the parser
        let init = if let Some(init) = &self.init {
            Box::new(init.transform(context)?)
        } else {
            return Err(context.register_error(
                self,
                ParseErrorKind::Unhandled {
                    node: format!("{self:?}"),
                },
            ));
        };
        Ok(Variable {
            id: context.id(self),
            binding,
            init,
        })
    }
}

/// Pattern for declaring variables
///
/// This is used where variables are declared, like `const` or function params.
/// It's also recursive because of array/object unpack patterns.
///
/// The docs for [oxc::BindingPattern] are really helpful.
impl Transform for oxc::BindingPattern<'_> {
    /// A binding pattern always has a binding, and can also have an init
    /// expression on the right side
    ///
    /// Not all use cases support the init expression. For those that don't,
    /// wrap the node in [`BindingOnly`] before calling `transform()`.
    type Output = (Binding, Option<Expression>);

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        let (kind, init) = match self {
            Self::BindingIdentifier(binding_identifier) => {
                let binding = BindingKind::Identifier(
                    binding_identifier.transform(context)?,
                );
                (binding, None)
            }
            Self::ArrayPattern(array_pattern) => {
                // Map to an intermediate type so we can call transform_all().
                // We can't map directly from the Option<BindingPattern> because
                // that doesn't carry all the span info we need
                let elements: Vec<_> = array_pattern
                    .elements
                    .iter()
                    .map(|pattern_option| {
                        if let Some(pattern) = pattern_option {
                            ArrayPatternBinding::Bound(pattern)
                        } else {
                            // Skip comma doesn't carry a span, so we have to
                            // use the parent
                            ArrayPatternBinding::Skip(array_pattern.span())
                        }
                    })
                    .collect();
                let elements = elements.transform_all(context);
                let rest = array_pattern.rest.as_ref().try_map(|rest| {
                    // = isn't supported here, because it defaults to []
                    BindingOnly(&rest.argument).transform(context).map(Box::new)
                })?;

                (BindingKind::Array { elements, rest }, None)
            }
            Self::ObjectPattern(object_pattern) => {
                let properties =
                    object_pattern.properties.transform_all(context);
                // ...rest always goes at the end
                let rest = object_pattern.rest.as_ref().try_map(|rest| {
                    // = isn't supported here, because it defaults to []
                    BindingOnly(&rest.argument).transform(context).map(Box::new)
                })?;
                (BindingKind::Object { properties, rest }, None)
            }
            // Pattern with an initializer expression, like `{x = 3}`. As far
            // as I can tell, this is only possible for a nested binding within
            // an array/object unpack. Function params and declarations use
            // different constructs to track the right side of the `=`.
            Self::AssignmentPattern(assignment_pattern) => {
                // Recursion!
                let binding =
                    BindingOnly(&assignment_pattern.left).transform(context)?;
                let expression = assignment_pattern.right.transform(context)?;
                return Ok((binding, Some(expression)));
            }
        };
        Ok((
            Binding {
                id: context.id(self),
                kind,
            },
            init,
        ))
    }
}

/// A wrapper for [oxc::BindingPattern] enforcing it is not an assignment
/// pattern
///
/// Use this for use cases where `AssignmentPattern` is an impossible variant.
/// This is in cases where the `=` is either illegal, or is handled by other
/// AST types.
struct BindingOnly<'a>(&'a oxc::BindingPattern<'a>);

impl GetSpan for BindingOnly<'_> {
    fn span(&self) -> oxc::Span {
        self.0.span()
    }
}

impl Transform for BindingOnly<'_> {
    /// This uses an internal type to carry the binding as well as the
    /// associated expression that *may* be present.
    type Output = Binding;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        match &self.0 {
            // If we hit this case, it means the caller made a false assumption
            oxc::BindingPattern::AssignmentPattern(_) => Err(context
                .register_error(
                    self,
                    ParseErrorKind::Unhandled {
                        node: format!("{:?}", self.0),
                    },
                )),
            // Everything else can't have an expression, so we can transform
            // as usual
            _ => {
                match self.0.transform(context)? {
                    (binding, None) => Ok(binding),
                    // Expression is illegal!!
                    (_, Some(_)) => Err(context.register_error(
                        self.0,
                        ParseErrorKind::Unhandled {
                            node: format!("{:?}", self.0),
                        },
                    )),
                }
            }
        }
    }
}

/// Wrapper for transforming elements of [oxc::BindingPattern::ArrayPattern]
///
/// These elements are options to indicate bound vs skipped elements. Since
/// the option isn't inherent specific to arrays, this wrapper type makes the
/// transformation more specific.
enum ArrayPatternBinding<'a> {
    Bound(&'a oxc::BindingPattern<'a>),
    /// oxc doesn't provide any node for the skip comma, so we have to hold
    /// onto the span for the parent binding
    Skip(oxc_span::Span),
}

impl GetSpan for ArrayPatternBinding<'_> {
    fn span(&self) -> oxc::Span {
        match self {
            Self::Bound(binding_pattern) => binding_pattern.span(),
            Self::Skip(span) => *span,
        }
    }
}

impl Transform for ArrayPatternBinding<'_> {
    type Output = ArrayPatternElement;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        let kind = match self {
            Self::Bound(binding_pattern) => {
                let (binding, init) = binding_pattern.transform(context)?;
                ArrayPatternElementKind::Identifier { binding, init }
            }
            Self::Skip(_) => ArrayPatternElementKind::Skip,
        };
        Ok(ArrayPatternElement {
            id: context.id(self),
            kind,
        })
    }
}

/// TODO explain difference from IdentifierName/IdentifierReference
impl Transform for oxc::BindingIdentifier<'_> {
    type Output = IdentifierNode;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        Ok(IdentifierNode {
            id: context.id(self),
            identifier: transform_ident(self.name)
                .map_err(|error| context.register_error(self, error))?,
        })
    }
}

/// PS doesn't differentiate between identifier declarations and references, so
/// this just maps to [`Identifier`]
impl Transform for oxc::IdentifierName<'_> {
    type Output = IdentifierNode;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        Ok(IdentifierNode {
            id: context.id(self),
            identifier: transform_ident(self.name)
                .map_err(|error| context.register_error(self, error))?,
        })
    }
}

/// PS doesn't differentiate between identifier declarations and references, so
/// this just maps to [`Identifier`]
impl Transform for oxc::IdentifierReference<'_> {
    type Output = IdentifierNode;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        Ok(IdentifierNode {
            id: context.id(self),
            identifier: transform_ident(self.name)
                .map_err(|error| context.register_error(self, error))?,
        })
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
    ) -> Result<Self::Output, Reg> {
        let key = self.key.transform(context)?;
        // Value binding and init expression both come from this transformation
        let (binding, init) = self.value.transform(context)?;

        Ok(ObjectPatternElement {
            id: context.id(self),
            key,
            binding,
            init,
        })
    }
}

/// `function` declaration
///
/// This maps to the same AST node as an arrow function bound via `const`.
impl Transform for oxc::Function<'_> {
    /// `None` for TypeScript stub declarations
    ///
    /// ```norust
    /// declare function foo(a: number): number;
    /// function bar(a: number): number;
    /// ```
    type Output = Option<Declaration>;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
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

        let identifier = identifier.transform(context)?;
        let name = identifier.identifier.clone();
        let binding = Binding {
            id: context.id_from(&identifier),
            kind: BindingKind::Identifier(identifier),
        };

        // Functions without a body are just type declarations, so ignore them
        let Some(body) = &self.body else {
            return Ok(None);
        };
        let body = body.transform(context)?;
        let parameters = self.params.transform(context)?;

        // Declare the function as a variable. There is no separate function
        // declaration in the PS AST, for simplicity
        let function = FunctionDefinition {
            id: context.id(self),
            name: Some(name),
            parameters,
            body,
            captures: [].into(), // Captures are populated post-parse
        };
        let init = Expression {
            id: context.id_from(&function),
            kind: ExpressionKind::Function(function),
        };
        let variable = Variable {
            id: context.id(self),
            binding,
            init: Box::new(init),
        };

        Ok(Some(Declaration {
            id: context.id(self),
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
    ) -> Result<Self::Output, Reg> {
        let parameters = self.params.transform(context)?;
        let body = match &self.body {
            // block w/ braces
            oxc::ArrowFunctionBody::FunctionBody(body) => {
                body.transform(context)?
            }
            // bare expression
            _ => {
                let expression =
                    as_expression!(context, &self.body).transform(context)?;
                FunctionBody::Expression(Box::new(expression))
            }
        };
        Ok(FunctionDefinition {
            id: context.id(self),
            name: None,
            parameters,
            body,
            captures: [].into(), // Will be filled out later in the pipeline
        })
    }
}

/// Body block of a `function` or a `=>` function
///
/// This always produces a [`FunctionBody::Block`]
impl Transform for oxc::FunctionBody<'_> {
    type Output = FunctionBody;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        // directives are ignored
        Ok(FunctionBody::Block(Block {
            id: context.id(self),
            statements: self.statements.transform_all(context),
        }))
    }
}

impl Transform for oxc::FormalParameters<'_> {
    type Output = Box<[FunctionParameter]>;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        let mut parameters: Vec<_> = self
            .items
            .iter()
            .map(|parameter| parameter.transform(context))
            .collect::<Result<_, _>>()?;
        // Add the rest param in separately
        if let Some(rest) = &self.rest {
            // = isn't allowed here because the default is always []
            let binding =
                BindingOnly(&rest.rest.argument).transform(context)?;
            parameters.push(FunctionParameter {
                id: context.id(&**rest),
                binding,
                init: None,
                varargs: true,
            });
        }
        Ok(parameters.into())
    }
}

impl Transform for oxc::FormalParameter<'_> {
    type Output = FunctionParameter;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        // Type and visibility annotations are ignored because they can
        // provide value at compile time, outside the PS runtime

        // Can't have an assignment binding here because the right side of the
        // = is stored in self.init
        let binding = BindingOnly(&self.pattern).transform(context)?;

        let init = if let Some(initializer) = &self.initializer {
            Some(Box::new(initializer.transform(context)?))
        } else {
            None
        };
        Ok(FunctionParameter {
            id: context.id(self),
            binding,
            init,
            varargs: false,
        })
    }
}

impl Transform for oxc::Expression<'_> {
    type Output = Expression;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        let kind = match self {
            Self::NullLiteral(literal) => {
                ExpressionKind::Literal(literal.transform(context)?)
            }
            Self::BooleanLiteral(literal) => {
                ExpressionKind::Literal(literal.transform(context)?)
            }
            Self::NumericLiteral(literal) => {
                ExpressionKind::Literal(literal.transform(context)?)
            }
            Self::BigIntLiteral(_) => {
                unsupported!(context, self, Unsupported::BigIntLiteral);
            }
            Self::StringLiteral(literal) => {
                ExpressionKind::Literal(literal.transform(context)?)
            }
            Self::RegExpLiteral(_) => {
                unsupported!(context, self, Unsupported::RegExp);
            }
            Self::TemplateLiteral(template) => {
                ExpressionKind::Template(template.transform(context)?)
            }
            Self::Identifier(reference) => {
                if reference.is_undefined() {
                    // undefined is an identifier, not a literal, in ES
                    ExpressionKind::Literal(Literal::Undefined)
                } else {
                    ExpressionKind::Identifier(reference.transform(context)?)
                }
            }
            Self::ArrayExpression(array_expression) => {
                // Span needs to wrap both the Array and the outer Literal
                let array = array_expression.transform(context)?;
                ExpressionKind::Literal(Literal::Array(array))
            }
            Self::ObjectExpression(object_expression) => {
                // Span needs to wrap both the Object and the outer Literal
                let object = object_expression.transform(context)?;
                ExpressionKind::Literal(Literal::Object(object))
            }
            Self::ParenthesizedExpression(parenthesized) => {
                // Needs no extra wrap, so we can return immediately
                return parenthesized.expression.transform(context);
            }
            Self::ComputedMemberExpression(expression) => {
                ExpressionKind::Property(expression.transform(context)?)
            }
            Self::StaticMemberExpression(expression) => {
                ExpressionKind::Property(expression.transform(context)?)
            }
            Self::ChainExpression(chain_expression) => {
                return chain_expression.transform(context);
            }
            Self::CallExpression(call_expression) => {
                ExpressionKind::Call(call_expression.transform(context)?)
            }
            Self::UnaryExpression(unary_expression) => {
                ExpressionKind::Unary(unary_expression.transform(context)?)
            }
            Self::BinaryExpression(binary_expression) => {
                ExpressionKind::Binary(binary_expression.transform(context)?)
            }
            // Logical binary operators are in a separate type
            Self::LogicalExpression(logical_expression) => {
                ExpressionKind::Binary(logical_expression.transform(context)?)
            }
            Self::ConditionalExpression(conditional_expression) => {
                ExpressionKind::Ternary(
                    conditional_expression.transform(context)?,
                )
            }
            Self::ArrowFunctionExpression(arrow_expr) => {
                ExpressionKind::Function(arrow_expr.transform(context)?)
            }

            // TypeScript stuff - ignore the types and transform transparently
            Self::TSAsExpression(expression) => {
                return expression.expression.transform(context);
            }
            Self::TSInstantiationExpression(expression) => {
                return expression.expression.transform(context);
            }
            Self::TSNonNullExpression(expression) => {
                return expression.expression.transform(context);
            }
            Self::TSSatisfiesExpression(expression) => {
                return expression.expression.transform(context);
            }
            Self::TSTypeAssertion(assertion) => {
                return assertion.expression.transform(context);
            }

            // Unsupported
            Self::AssignmentExpression(_) => {
                unsupported!(context, self, Unsupported::Mutation);
            }
            Self::FunctionExpression(_) => {
                unsupported!(context, self, Unsupported::FunctionExpression)
            }
            Self::SequenceExpression(_) => {
                unsupported!(context, self, Unsupported::Sequence)
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
                unsupported!(context, self, Unsupported::ImportMeta);
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
                unsupported!(context, self, Unsupported::V8Intrinstic);
            }
        };

        Ok(Expression {
            id: context.id(self),
            kind,
        })
    }
}

impl Transform for oxc::BooleanLiteral {
    type Output = Literal;

    fn transform(&self, _: &mut ParseContext) -> Result<Self::Output, Reg> {
        Ok(Literal::Boolean(self.value))
    }
}

impl Transform for oxc::NullLiteral {
    type Output = Literal;

    fn transform(&self, _: &mut ParseContext) -> Result<Self::Output, Reg> {
        Ok(Literal::Null)
    }
}

impl Transform for oxc::NumericLiteral<'_> {
    type Output = Literal;

    fn transform(&self, _: &mut ParseContext) -> Result<Self::Output, Reg> {
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
    ) -> Result<Self::Output, Reg> {
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
    ) -> Result<Self::Output, Reg> {
        // oxc provides the quasis (literal chunks) and expressions (dynamic
        // chunks) separately. We're responsible for piecing them back together
        // in order. Collect them all into one sequence, then sort by their
        // source position
        let mut chunks: Vec<(TemplateChunk, oxc_span::Span)> = self
            .quasis
            .iter()
            .map(|quasi| {
                let literal = quasi
                    .value
                    .cooked
                    .expect("TODO what do if None")
                    .as_str()
                    .expect("TODO")
                    .to_owned();
                let span = quasi.span;
                (
                    TemplateChunk {
                        id: context.id(quasi),
                        kind: TemplateChunkKind::Literal(literal),
                    },
                    span,
                )
            })
            .collect();
        chunks.extend(
            // Pair each expression with its range so we can sort
            self.expressions
                .iter()
                .map(GetSpan::span)
                .zip(self.expressions.transform_all(context))
                .map(|(span, expression)| {
                    (
                        TemplateChunk {
                            id: context.id_from(&expression),
                            kind: TemplateChunkKind::Expression(expression),
                        },
                        span,
                    )
                }),
        );
        chunks.sort_by_key(|(_, range)| range.start);
        Ok(TemplateLiteral {
            id: context.id(self),
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
    ) -> Result<Self::Output, Reg> {
        let elements = self
            .elements
            .iter()
            .map(|element| match element {
                oxc::ArrayExpressionElement::SpreadElement(spread_element) => {
                    let expression =
                        spread_element.argument.transform(context)?;
                    Ok(ArrayElement {
                        id: context.id_from(&expression),
                        expression,
                        spread: true,
                    })
                }
                oxc::ArrayExpressionElement::Elision(elision) => {
                    unsupported!(
                        context,
                        elision.deref(),
                        Unsupported::ArrayElision
                    );
                }
                _ => {
                    let expression =
                        as_expression!(context, element).transform(context)?;
                    Ok(ArrayElement {
                        id: context.id_from(&expression),
                        expression,
                        spread: false,
                    })
                }
            })
            .collect::<Result<_, _>>()?;
        Ok(ArrayLiteral {
            id: context.id(self),
            elements,
        })
    }
}

impl Transform for oxc::ObjectExpression<'_> {
    type Output = ObjectLiteral;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        let properties = self.properties.transform_all(context);
        Ok(ObjectLiteral {
            id: context.id(self),
            properties,
        })
    }
}

impl Transform for oxc::ObjectPropertyKind<'_> {
    type Output = ObjectProperty;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        match self {
            Self::ObjectProperty(property) => property.transform(context),
            Self::SpreadProperty(element) => {
                let expression = element.argument.transform(context)?;
                Ok(ObjectProperty {
                    id: context.id(self),
                    kind: ObjectPropertyKind::Spread(expression),
                })
            }
        }
    }
}

impl Transform for oxc::ObjectProperty<'_> {
    type Output = ObjectProperty;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
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
                let property = self.key.transform(context)?;
                let expression = self.value.transform(context)?;
                Ok(ObjectProperty {
                    id: context.id(self),
                    kind: ObjectPropertyKind::Property {
                        property,
                        expression,
                    },
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
    ) -> Result<Self::Output, Reg> {
        let kind = match self {
            Self::StaticIdentifier(ident) => {
                let identifier = ident.transform(context)?;
                PropertyNameKind::Identifier(identifier)
            }
            Self::PrivateIdentifier(_) => {
                unsupported!(context, self, Unsupported::Private);
            }
            _ => {
                let expression =
                    as_expression!(context, self).transform(context)?;
                PropertyNameKind::Expression(Box::new(expression))
            }
        };
        Ok(PropertyName {
            id: context.id(self),
            kind,
        })
    }
}

impl Transform for oxc::ComputedMemberExpression<'_> {
    type Output = PropertyAccess;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        let object = self.object.transform(context)?;
        let property = self.expression.transform(context)?;
        Ok(PropertyAccess {
            id: context.id(self),
            object: object.into(),
            property: PropertyName {
                id: context.id_from(&property),
                kind: PropertyNameKind::Expression(Box::new(property)),
            },
        })
    }
}

impl Transform for oxc::StaticMemberExpression<'_> {
    type Output = PropertyAccess;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        let object = self.object.transform(context)?;
        let property = self.property.transform(context)?;

        Ok(PropertyAccess {
            id: context.id(self),
            object: object.into(),
            property: PropertyName {
                id: context.id_from(&property),
                kind: PropertyNameKind::Identifier(property),
            },
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
    ) -> Result<Self::Output, Reg> {
        let from_property_access = |context: &mut ParseContext,
                                    property_access: PropertyAccess|
         -> Expression {
            let access = OptionalPropertyAccess {
                id: property_access.id,
                object: property_access.object,
                property: property_access.property,
            };
            Expression {
                id: context.id_from(&access),
                kind: ExpressionKind::OptionalProperty(access),
            }
        };

        match &self.expression {
            oxc::ChainElement::CallExpression(call_expression) => {
                // CallExpression transformer handles the conditional case
                // itself, so we can just forward that
                let call = call_expression.transform(context)?;
                Ok(Expression {
                    id: context.id_from(&call),
                    kind: ExpressionKind::Call(call),
                })
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
    ) -> Result<Self::Output, Reg> {
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
        let expression = self.argument.transform(context)?;
        Ok(UnaryOperation {
            id: context.id(self),
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
    ) -> Result<Self::Output, Reg> {
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
            oxc::BinaryOperator::In => {
                unsupported!(context, self, Unsupported::In);
            }
            oxc::BinaryOperator::Instanceof => {
                unsupported!(context, self, Unsupported::Instanceof);
            }
        };
        let lhs = self.left.transform(context)?;
        let rhs = self.right.transform(context)?;
        Ok(BinaryOperation {
            id: context.id(self),
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
    ) -> Result<Self::Output, Reg> {
        let operator = match self.operator {
            oxc::LogicalOperator::Or => BinaryOperator::LogicalOr,
            oxc::LogicalOperator::And => BinaryOperator::LogicalAnd,
            oxc::LogicalOperator::Coalesce => BinaryOperator::NullishCoalesce,
        };
        let lhs = self.left.transform(context)?;
        let rhs = self.right.transform(context)?;
        Ok(BinaryOperation {
            id: context.id(self),
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
    ) -> Result<Self::Output, Reg> {
        let condition = self.test.transform(context)?;
        let true_expression = self.consequent.transform(context)?;
        let false_expression = self.alternate.transform(context)?;
        Ok(TernaryConditional {
            id: context.id(self),
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
    ) -> Result<Self::Output, Reg> {
        let arguments = self.arguments.transform_all(context);
        let function = self.callee.transform(context)?;
        Ok(FunctionCall {
            id: context.id(self),
            function: function.into(),
            arguments,
            // Like this: a.f?.()
            optional: self.optional,
        })
    }
}

impl Transform for oxc::Argument<'_> {
    type Output = Argument;

    fn transform(
        &self,
        context: &mut ParseContext,
    ) -> Result<Self::Output, Reg> {
        match self {
            Self::SpreadElement(spread_element) => spread_element
                .argument
                .transform(context)
                .map(|expression| Argument::Spread(Box::new(expression))),
            // All other variants are inherited from Expression
            _ => as_expression!(context, self)
                .transform(context)
                .map(|expression| Argument::Expression(Box::new(expression))),
        }
    }
}

/// Transform an oxc identifier to a PS identifier
///
/// Returns an error if the identifier fails to parse. PS uses a more strict
/// identifier definition than ES.
fn transform_ident(
    ident: oxc::Ident<'_>,
) -> Result<Identifier, ParseErrorKind> {
    // Error here *should* be impossible because we use the same identifier
    // rules as ES
    ident.as_str().parse().map_err(|_| {
        ParseErrorKind::Unsupported(Unsupported::UnicodeIdentifier)
    })
}

/// Indicator that an error has been registered via [`ParseContext::error`]
///
/// This is the error variant for all transformations because it forces
/// transformers to register their errors with the context.
///
/// **Do not construct this manually!**
struct Reg;
