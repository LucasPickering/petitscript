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
    error::ParseError,
    source::{SourceId, SourceTable, Span, SpanTable},
};
use oxc_allocator::Allocator;
use oxc_parser::{Parser, ParserReturn};
use oxc_span::SourceType;

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
) -> Result<(Node<Module>, SpanTable), ParseError> {
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
fn parse_module(mut context: ParseContext) -> Result<Node<Module>, ParseError> {
    // TODO maybe refactor this? the oxc nodes carry their source text so we
    // may be able to simplify the context instead of carrying source around
    let source = context.current_source();
    let code = source.text().expect("TODO").into_owned();

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
        // TODO map oxc error to our parse error
        todo!();
        // return Err(ParseError::Parse {
        //     source_name: source.path().and_then(source::display_source_path),
        //     errors: diagnostics,
        // });
    }

    // Sanity check: I don't think it can return fatal_error=true if the
    // diagnostics list is empty
    assert!(
        !fatal_error,
        "fatal_error should be false when there are no diagnostics"
    );

    // Transform to a PS AST
    transform::transform_module(&mut context, program)
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
        let source = r"'test\nnewline'";
        let module = parse_test(source);
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
        let module = parse_test(source);
        assert_eq!(
            module.data(),
            &Module::new(["test no newline".into_expr().into()])
        );
    }

    fn parse_test(source: &'static str) -> Node<Module> {
        let mut source_table = SourceTable::default();
        source_table.insert(source);
        let (module, _) =
            parse(&mut source_table).unwrap_or_else(|error| panic!("{error}"));
        module
    }
}
