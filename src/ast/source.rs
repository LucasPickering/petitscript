//! Utilities for interacting with source code

use crate::{ast::NodeId, Error};
use normalize_path::NormalizePath;
use std::{
    borrow::Cow,
    collections::HashMap,
    env,
    fmt::{self, Debug, Display},
    fs,
    path::{Path, PathBuf},
};

/// A source of source code. E.g. a string literal or a file path
pub trait Source: 'static + Debug + Send + Sync {
    /// Descriptive user-friendly display name for this source
    fn name(&self) -> Option<String>;

    /// Root path for local module imports. Import paths will be relative to
    /// this path. For file-less source strings, return `None` and the current
    /// working directory will be used.
    fn import_root(&self) -> Option<&Path>;

    /// Get the source code from this source
    fn text(&self) -> Result<Cow<'_, str>, Error>;
}

impl Source for &'static str {
    fn name(&self) -> Option<String> {
        None
    }

    fn import_root(&self) -> Option<&Path> {
        // Use cwd for imports
        None
    }

    fn text(&self) -> Result<Cow<'_, str>, Error> {
        Ok(Cow::Borrowed(self))
    }
}

impl Source for String {
    fn name(&self) -> Option<String> {
        None
    }

    fn import_root(&self) -> Option<&Path> {
        // Use cwd for imports
        None
    }

    fn text(&self) -> Result<Cow<'_, str>, Error> {
        Ok(self.as_str().into())
    }
}

impl Source for PathBuf {
    fn name(&self) -> Option<String> {
        // Absolute-ify the path and normalize it, but do _not_ canonicalize.
        // Resolving links may lead to surprising behavior for the user. We use
        // absolute paths because PS doesn't use any singular project root
        env::current_dir()
            .ok()?
            .join(self)
            .normalize()
            .into_os_string()
            .into_string()
            .ok()
    }

    fn import_root(&self) -> Option<&Path> {
        Some(self)
    }

    fn text(&self) -> Result<Cow<'_, str>, Error> {
        Ok(fs::read_to_string(self)
            .map_err(|error| Error::Io {
                path: Some(self.clone()),
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
            source_name: source.name().unwrap_or_else(|| "???".into()),
            start_line,
            start_column,
            end_line,
            end_column,
        }
    }
}

/// A range of source code, defined as byte offsets. This is the raw mapping
/// that we pass around within the compiler/interpreter. To get a user-friendly
/// version of this TODO
///
/// The parser we use only provides byte offsets for its AST nodes, so we defer
/// mapping bytes to line/column until it's actually needed.
#[derive(Copy, Clone, Debug)]
pub(crate) struct Span {
    /// The ID of the [Source] (typically a file) containing the code. This
    /// is just an ID because we create thousands of spans, so we need this
    /// to be cheap. Use [SourceTable] to look up the actual source.
    pub source_id: SourceId,
    /// Byte offset for the beginning of this span (inclusive). Always <=
    /// end_span.
    pub start_offset: usize,
    /// Byte offset for the end of this span (inclusive). Always >=
    /// start_span.
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
                write!(f, "{}:{}:{}", source_name, start_line, start_column)
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

    /// Insert a span into the table. Panic if the AST node already has an
    /// entry, because that indicates an ID has been repeated by the parser
    /// which is a bug
    pub fn insert(&mut self, id: NodeId, span: Span) {
        if self.0.insert(id, span).is_some() {
            panic!("AST node {id:?} already has an entry in the span table");
        }
    }
}

/// A list of source locations representing the function call stack at a
/// particular moment in time. Most recent call is _last_ on this stack.
#[derive(Debug, Default)]
pub struct StackTrace(Vec<StackTraceFrame>);

impl Display for StackTrace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO reverse order?
        writeln!(f, "Error (most recent call last)")?;
        for frame in &self.0 {
            writeln!(f, "  {frame}")?;
        }
        Ok(())
    }
}

impl IntoIterator for StackTrace {
    type Item = StackTraceFrame;
    type IntoIter = <Vec<StackTraceFrame> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl FromIterator<StackTraceFrame> for StackTrace {
    fn from_iter<T: IntoIterator<Item = StackTraceFrame>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

/// One frame in a stack trace
#[derive(Debug)]
pub struct StackTraceFrame {
    /// TODO
    pub span: QualifiedSpan,
    /// TODO
    pub function_name: String,
}

impl Display for StackTraceFrame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "in {} at {}", self.function_name, self.span)
    }
}
