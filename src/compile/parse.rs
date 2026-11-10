//! Parse some source code to the PetitScript [ast]
//!
//! This uses [oxc_parser] for the actual parsing, then converts from [oxc_ast]
//! to the simplified PetitScript AST. Simplifying the AST here makes the
//! evaluation engine much simpler and provides errors for invalid syntax
//! up front.

mod transform;

use crate::{
    Source,
    ast::{Module, Node, NodeId},
    error::{ParseError, ParseErrorKind, ParseErrors},
    source::{SourceId, SourceTable, Span, SpanTable},
};
use oxc_allocator::Allocator;
use oxc_diagnostics::OxcDiagnostic;
use oxc_parser::{Parser, ParserReturn};
use oxc_span::SourceType;
use std::borrow::Cow;

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
) -> Result<(Node<Module>, SpanTable), ParseErrors> {
    let source_id = sources.root_id();
    let mut spans = SpanTable::default();
    let context = ParseContext {
        sources,
        current_source_id: source_id,
        next_node_id: &mut 0,
        spans: &mut spans,
        errors: vec![],
    };
    let module = parse_module(context)?;
    Ok((module, spans))
}

/// Helper for parsing a single module
///
/// This is called recursively within a parse tree, once per PS module. Each
/// module corresponds to a single source, so this will add a new source to the
/// given source table.
fn parse_module(context: ParseContext) -> Result<Node<Module>, ParseErrors> {
    let source = context.current_source();
    // TODO don't clone
    let source_code = source.text().to_owned();

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
    } = Parser::new(&allocator, &source_code, source_type).parse();

    // Check for errors
    if !diagnostics.is_empty() {
        // TODO explain why we map from oxc to miette
        let errors = diagnostics
            .into_iter()
            .map(map_oxc_error)
            .collect::<Vec<_>>();
        return Err(ParseErrors {
            errors,
            source_code: source.clone(),
        });
    }

    // Sanity check: I don't think it can return fatal_error=true if the
    // diagnostics list is empty
    assert!(
        !fatal_error,
        "fatal_error should be false when there are no diagnostics"
    );

    // Transform to a PS AST
    transform::transform_module(context, program)
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
    /// TODO explain error collection
    errors: Vec<ParseError>,
}

impl ParseContext<'_> {
    /// Current source being parsed
    fn current_source(&self) -> &Source {
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

/// Map a parse error from oxc to our parse type
///
/// Oxc's errors are defined in the [oxc_diagnostics] crate, which is a fork of
/// [miette]. We use miette's error type for parse errors, so the mapping is
/// mostly 1:1.
fn map_oxc_error(diagnostic: OxcDiagnostic) -> ParseError {
    // Map the oxc diagnostic to our error type
    let diagnostic = diagnostic.inner_owned();
    let labels = diagnostic
        .labels
        .into_iter()
        .map(|label| {
            // Map the oxc label to a miette label
            let span = label.span();
            let span = miette::SourceSpan::new(
                (span.start as usize).into(),
                (span.end - span.start) as usize,
            );
            let name = label.label().map(String::from);
            if label.primary() {
                miette::LabeledSpan::new_primary_with_span(name, span)
            } else {
                miette::LabeledSpan::new_with_span(name, span)
            }
        })
        .collect();
    let error = ParseErrorKind::Parse {
        message: diagnostic.message.into_owned(),
        help: diagnostic.help.map(Cow::into_owned),
        note: diagnostic.note.map(Cow::into_owned),
        severity: match diagnostic.severity {
            oxc_diagnostics::Severity::Advice => miette::Severity::Advice,
            oxc_diagnostics::Severity::Warning => miette::Severity::Warning,
            oxc_diagnostics::Severity::Error => miette::Severity::Error,
        },
    };
    ParseError { error, labels }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{assert_matches, ast::IntoExpression, error::Unsupported};
    use miette::LabeledSpan;

    /// Test an escaped newline `\n` in a string literal
    /// ```notrust
    /// "test\nnewline"
    /// // parses to
    /// `test
    /// newline`
    /// ```
    #[test]
    fn string_literal_newline() {
        let source = r"'test\nnewline'";
        let module = parse_test(source).unwrap();
        assert_eq!(
            module.data(),
            &Module::new(["test\nnewline".into_expr().into()])
        );
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
        let source = r"'test \
no newline'";
        let module = parse_test(source).unwrap();
        assert_eq!(
            module.data(),
            &Module::new(["test no newline".into_expr().into()])
        );
    }

    /// If multiple AST nodes cannot be transformed, we should get errors for
    /// all of them
    #[test]
    fn multiple_errors() {
        fn labeled_span(offset: usize, length: usize) -> LabeledSpan {
            LabeledSpan::new_primary_with_span(
                None,
                miette::SourceSpan::new(offset.into(), length),
            )
        }

        let source = "
            let x = 3;
            var y = 4;
            async function f() {
                // This does *not* produce an error; it stops at the async fn
                let x = 3;
            }
        ";
        let errors = parse_test(source).unwrap_err();
        // Source matches what we put in
        assert_eq!(errors.source_code, Source::from(source));
        // There should be 2 errors
        let (labels_let, labels_var, labels_async) = assert_matches!(
            errors.errors.as_slice(),
            [
                ParseError {
                    error: ParseErrorKind::Unsupported(Unsupported::Mutation),
                    labels: labels_let,
                },
                ParseError {
                    error: ParseErrorKind::Unsupported(Unsupported::Mutation),
                    labels: labels_var,
                },
                ParseError {
                    error: ParseErrorKind::Unsupported(Unsupported::Async),
                    labels: labels_async,
                },
            ]
            => (labels_let, labels_var, labels_async)
        );
        // Spans are where we expect them to be
        assert_eq!(labels_let.as_slice(), [labeled_span(13, 10)]);
        assert_eq!(labels_var.as_slice(), [labeled_span(36, 10)]);
        assert_eq!(labels_async.as_slice(), [labeled_span(59, 139)]);
    }

    fn parse_test(source: &'static str) -> Result<Node<Module>, ParseErrors> {
        let mut source_table = SourceTable::default();
        source_table.insert(source);
        parse(&mut source_table).map(|(module, _)| module)
    }
}
