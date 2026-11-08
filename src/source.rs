//! Utilities for interacting with source code

use crate::{ast::NodeId, error::SourceError};
use normalize_path::NormalizePath;
use std::{
    borrow::Cow,
    collections::HashMap,
    fmt::{self, Debug, Display},
    fs,
    path::{Path, PathBuf},
};

/// Source code provider
///
/// Examples of a `Source` are a string literal containing code or a path to a
/// source file.
pub trait Source: 'static + Debug + Send + Sync {
    /// Descriptive name for this source
    ///
    /// The name is used in user-facing output (such as errors), so it should
    /// be readable rather than specific.
    fn name(&self) -> Cow<'_, str>;

    /// For file-based sources, the path to the source
    ///
    /// Return `None` if the source isn't a file.
    ///
    /// ## Relative (Local) Imports
    ///
    /// Imported local modules (`import ... from './other.js'`) from this source
    /// will be performed relative to this path. Sources that don't have a
    /// path cannot import local modules. Doing so will result in a
    /// [ParseError::ImportNotAllowed](crate::error::ParseError::ImportNotAllowed).
    fn path(&self) -> Option<&Path>;

    /// Load source code from this source
    fn text(&self) -> Result<Cow<'_, str>, SourceError>;
}

/// Load source code from a string literal
impl Source for &'static str {
    fn name(&self) -> Cow<'_, str> {
        "<source>".into()
    }

    fn path(&self) -> Option<&Path> {
        None
    }

    fn text(&self) -> Result<Cow<'_, str>, SourceError> {
        Ok(Cow::Borrowed(self))
    }
}

/// Load source code directly from a string
impl Source for String {
    fn name(&self) -> Cow<'_, str> {
        "<source>".into()
    }

    fn path(&self) -> Option<&Path> {
        None
    }

    fn text(&self) -> Result<Cow<'_, str>, SourceError> {
        Ok(self.as_str().into())
    }
}

/// Load source code from a file
impl Source for PathBuf {
    fn name(&self) -> Cow<'_, str> {
        self.normalize().display().to_string().into()
    }

    fn path(&self) -> Option<&Path> {
        Some(self)
    }

    fn text(&self) -> Result<Cow<'_, str>, SourceError> {
        Ok(fs::read_to_string(self)
            .map_err(|error| SourceError {
                path: self.clone(),
                error,
            })?
            .into())
    }
}

/// TODO
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) struct SourceId(u32);

/// TODO
#[derive(Debug, Default)]
pub(crate) struct SourceTable(Vec<Box<dyn Source>>);

impl SourceTable {
    /// Get the ID of the first source in the table. Panic if the table is empty
    pub fn root_id(&self) -> SourceId {
        assert!(
            !self.0.is_empty(),
            "Cannot get root ID of empty source table"
        );
        SourceId(0)
    }

    /// Get a source by its unique ID
    pub fn get(&self, id: SourceId) -> &dyn Source {
        &**self.0.get(id.0 as usize).expect("TODO")
    }

    /// Add a new source to the table, returning its new unique ID
    pub fn insert(&mut self, source: impl Source) -> SourceId {
        // The ID is the index that the pushed element will get
        let index = self.0.len();
        let id = SourceId(index as u32);
        self.0.push(Box::new(source));
        id
    }

    /// Get an iterator over the sources
    pub fn iter(&self) -> impl Iterator<Item = &dyn Source> {
        self.0.iter().map(|s| &**s)
    }

    /// Map a span from cheap IDs and byte offsets to user-friendly source names
    /// and lines/columns
    pub fn qualify(&self, span: Span) -> QualifiedSpan {
        let source = self.get(span.source_id);
        let text = &source.text().expect("TODO");
        // Lines/columns are 1-indexed, because reasons
        let mut line = 1;
        let mut column = 1;
        let mut start_line = 0;
        let mut start_column = 0;
        let mut end_line = 0;
        let mut end_column = 0;
        for (index, c) in text.char_indices() {
            if index == span.start_offset {
                start_line = line;
                start_column = column;
            }
            if index == span.end_offset {
                end_line = line;
                end_column = column;
            }
            if index > span.end_offset {
                break;
            }

            // Reset at the end of the line
            if c == '\n' {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
        }

        QualifiedSpan::Source {
            source_name: source.name().into_owned(),
            start_line,
            start_column,
            end_line,
            end_column,
        }
    }
}

/// A range of source code, defined as byte offsets
///
/// This is the raw mapping that we pass around within the compiler/interpreter.
/// To get a user-friendly version of this, use [SourceTable::qualify].
///
/// The parser we use only provides byte offsets for its AST nodes, so we defer
/// mapping bytes to line/column until it's actually needed.
#[derive(Copy, Clone, Debug)]
pub(crate) struct Span {
    /// ID of the [Source] (typically a file) containing the code
    ///
    /// This is just an ID because we create thousands of spans, so we need
    /// this to be cheap. Use [SourceTable] to look up the actual source.
    pub source_id: SourceId,
    /// Byte offset for the beginning of this span (inclusive)
    ///
    /// Always <= end_span.
    pub start_offset: usize,
    /// Byte offset for the end of this span (inclusive)
    ///
    /// Always >= start_span.
    pub end_offset: usize,
}

/// A range of source code, mapped to lines/columns. This format is suitable
/// for error printing.
///
/// TODO should this just be "Span" since it's external-facing?
#[derive(Debug)]
pub enum QualifiedSpan {
    /// A location in PetitScript source code
    Source {
        // TODO comments
        source_name: String,
        start_line: usize,
        start_column: usize,
        end_line: usize,
        end_column: usize,
    },
    /// A location in Rust native code
    Native,
}

impl Display for QualifiedSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            QualifiedSpan::Source {
                source_name,
                start_line,
                start_column,
                ..
            } => {
                write!(f, "{source_name}:{start_line}:{start_column}")
            }
            QualifiedSpan::Native => write!(f, "<native code>"),
        }
    }
}

/// A table of all spans for each node in the AST. The parser is responsible for
/// generating this and ensuring every AST node has an entry.
#[derive(Debug, Default)]
pub(crate) struct SpanTable(HashMap<NodeId, Span>);

impl SpanTable {
    /// Get a span from the table, or `None` if the node ID isn't in the table
    pub fn get(&self, id: NodeId) -> Option<Span> {
        self.0.get(&id).copied()
    }

    /// Insert a span into the table
    ///
    /// Panic if the AST node already has an entry. That indicates an ID has
    /// been repeated by the parser which is a bug.
    pub fn insert(&mut self, id: NodeId, span: Span) {
        let inserted = self.0.insert(id, span);
        assert!(
            inserted.is_none(),
            "AST node {id:?} already has an entry in the span table"
        );
    }
}
