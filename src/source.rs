//! Utilities for interacting with source code

use crate::{ast::NodeId, error::SourceError};
use miette::MietteError;
use normalize_path::NormalizePath;
use std::{
    collections::HashMap,
    fmt::{self, Debug, Display},
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};

/// Source code provider
///
/// TODO more
///
/// ## Cloning
///
/// Cloning is cheap because it stores source code in an `Arc<str>`.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Source {
    /// Load source code from a string literal
    Static(&'static str),
    /// Load source code from an owned string
    String(Arc<str>),
    /// Load source code from a file
    File {
        /// Descriptive name derived from the path
        name: String,
        /// TODO
        path: PathBuf,
        /// TODO
        text: Arc<str>,
    },
}

impl Source {
    /// TODO
    pub fn file(path: impl AsRef<Path>) -> Result<Self, SourceError> {
        let path = path.as_ref();
        let text = fs::read_to_string(path).map_err(|error| SourceError {
            path: path.to_owned(),
            error,
        })?;
        let name = path.normalize().display().to_string();
        Ok(Self::File {
            name,
            path: path.to_owned(),
            text: text.into(),
        })
    }

    /// Descriptive name for this source
    ///
    /// The name is used in user-facing output (such as errors), so it's
    /// readable rather than specific.
    pub fn name(&self) -> &str {
        match self {
            Self::Static(_) | Self::String(_) => "<source>",
            Self::File { name, .. } => name,
        }
    }

    /// Get the path to to the source
    ///
    /// Return `None` if the source isn't a file.
    ///
    /// ## Relative (Local) Imports
    ///
    /// Imported local modules (`import ... from './other.js'`) from this source
    /// will be performed relative to this path. Sources that don't have a
    /// path cannot import local modules. Doing so will result in a
    /// [ParseErrorKind::InvalidLocalImport](crate::error::ParseErrorKind::InvalidLocalImport).
    pub fn path(&self) -> Option<&Path> {
        match self {
            Self::Static(_) | Self::String(_) => None,
            Self::File { path, .. } => Some(path),
        }
    }

    /// Get the source code
    ///
    /// Source code is preloaded during source creation, so this is cheap and
    /// infallible.
    pub fn text(&self) -> &str {
        match self {
            Self::Static(s) => s,
            Self::String(s) => s,
            Self::File { text, .. } => text,
        }
    }
}

/// TODO
impl miette::SourceCode for Source {
    fn read_span<'a>(
        &'a self,
        span: &miette::SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn miette::SpanContents<'a> + 'a>, MietteError> {
        // TODO include name
        self.text()
            .read_span(span, context_lines_before, context_lines_after)
    }
}

/// Convert a string literal to [Source::Static]
impl From<&'static str> for Source {
    fn from(value: &'static str) -> Self {
        Self::Static(value)
    }
}

/// Convert a [String] to [Source::String]
impl From<String> for Source {
    fn from(value: String) -> Self {
        Self::String(value.into())
    }
}

/// TODO
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct SourceId(u32);

/// TODO
#[derive(Debug, Default)]
pub(crate) struct SourceTable(HashMap<SourceId, Source>);

impl SourceTable {
    /// Get the ID of the first source in the table
    ///
    /// ## Panics
    ///
    /// Panic if the table is empty.
    pub fn root_id(&self) -> SourceId {
        assert!(
            !self.0.is_empty(),
            "Cannot get root ID of empty source table"
        );
        SourceId(0)
    }

    /// Get a source by its unique ID
    ///
    /// ## Panics
    ///
    /// Panic if the ID isn't in the table. Any ID returned by
    /// [SourceTable::insert] will be valid for that table.
    pub fn get(&self, id: SourceId) -> &Source {
        self.0.get(&id).expect("TODO")
    }

    /// Add a new source to the table, returning its new unique ID
    pub fn insert(&mut self, source: impl Into<Source>) -> SourceId {
        // The ID is the index that the pushed element will get
        let index = self.0.len();
        let id = SourceId(index as u32);
        self.0.insert(id, source.into());
        id
    }

    /// Get an iterator over the sources
    pub fn iter(&self) -> impl Iterator<Item = &Source> {
        self.0.values()
    }

    /// Map a span from cheap IDs and byte offsets to user-friendly source names
    /// and lines/columns
    pub fn qualify(&self, span: Span) -> QualifiedSpan {
        let source = self.get(span.source_id);
        let text = &source.text();
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
            source_name: source.name().to_owned(),
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
