//! Utilities for interacting with source code

use crate::{
    ast::NodeId,
    error::{SourceError, TooManyModules},
};
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
/// This provides PetitScript source code. All variants store the loaded code,
/// so any fallible processes (e.g. loading from a file) happen during
/// *construction*. Storing the full code could use a lot of memory, but PS
/// programs should generally be small. Storing it makes error reporting later
/// much easier.
///
/// ## Cloning
///
/// Cloning is cheap because source code is stored via `Arc<str>`.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Source {
    /// Source code from a string literal
    Static(&'static str),
    /// Source code from an owned string
    String(Arc<str>),
    /// Source code loaded from a file
    File {
        /// Descriptive name derived from the path
        name: String,
        /// Path to the file that this was loaded from
        path: PathBuf,
        /// Full text of the file
        text: Arc<str>,
    },
}

impl Source {
    /// Load source code from a file
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

/// Cheap ID for a specific [Source] in a [SourceTable]
///
/// Sources get aliased by ID because each [Span] needs to point to its source,
/// and each AST node needs a span.
///
/// This uses `u8` because these IDs get bitpacked into the offset of
/// [miette::SourceSpan] during error reporting. This limits us to 256 sources
/// per program, but that should be more than enough. PS programs are supposed
/// to be small and simple.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct SourceId(u8);

/// TODO
#[derive(Clone, Debug, Default)]
pub(crate) struct SourceTable(HashMap<SourceId, Source>);

impl SourceTable {
    /// Maximum number of sources in the table
    ///
    /// This is the maximum possible value for [SourceId].
    pub const MAX_SOURCES: usize = 256;

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
    ///
    /// Return an error if the table already has the max number of sources
    /// (256).
    pub fn insert(
        &mut self,
        source: Source,
    ) -> Result<SourceId, TooManyModules> {
        // The ID is the index that the pushed element will get
        let index = self.0.len();
        let id = SourceId(u8::try_from(index).map_err(|_| TooManyModules)?);
        self.0.insert(id, source);
        Ok(id)
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

/// Provide source for for error presentation
///
/// Miette spans only track offset/length for each span, i.e. there's no field
/// to track source *file*. Since we want to report errors across multiple
/// files, this packs the source ID into the upper 8 bits of the span offset.
/// That limits us to 256 source files and `2^56` bytes per file which is...
/// totally fine.
///
/// This seems hacky (and it is), but miette has no support for multiple files:
/// <https://github.com/zkat/miette/issues/193>
impl miette::SourceCode for SourceTable {
    fn read_span<'a>(
        &'a self,
        span: &miette::SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn miette::SpanContents<'a> + 'a>, MietteError> {
        // Source ID is encoded into the top 8 bits of the source span offset
        let source_id = SourceId((span.offset() >> Span::SOURCE_ID_BITS) as u8);
        let offset = span.offset() & !Span::SOURCE_ID_MASK;
        let span = miette::SourceSpan::new(offset.into(), span.len());
        let source = self.0.get(&source_id).ok_or(MietteError::OutOfBounds)?;

        // TODO include the source name
        source.text().read_span(
            &span,
            context_lines_before,
            context_lines_after,
        )
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

impl Span {
    /// Bits to left-shift the source ID before packing it into the span offset
    const SOURCE_ID_BITS: u32 = usize::BITS - u8::BITS;
    /// Bitmask to extract the source ID from the span offset
    const SOURCE_ID_MASK: usize = !0 << Self::SOURCE_ID_BITS;

    /// Convert this span to a [miette::SourceSpan]
    ///
    /// See [SourceTable]'s `SourceCode` impl for an explanation of the source
    /// ID encoding.
    pub(crate) fn into_miette(self) -> miette::SourceSpan {
        const MAX_OFFSET: usize = 1 << Span::SOURCE_ID_BITS;
        let length = self.end_offset - self.start_offset;

        // Encode the source ID into the top 8 bits of the start. We definitely
        // don't need more than 24 bits (or 56 bits on x64 systems) for source
        // len. This is just a safety check.
        assert!(
            self.start_offset < MAX_OFFSET,
            "Source offset too large; must be < {MAX_OFFSET}. \
            This file is too big!"
        );
        let start: usize = (usize::from(self.source_id.0)
            << Self::SOURCE_ID_BITS)
            | self.start_offset;
        miette::SourceSpan::new(start.into(), length)
    }
}

/// A range of source code, mapped to lines/columns
///
/// This format is suitable for error printing.
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

#[cfg(test)]
mod tests {
    use super::*;
    use miette::SourceCode;

    /// Test [Span] <=> [miette::SourceSpan] conversions
    ///
    /// We pack the [SourceId] into the upper bits of the miette span.
    #[test]
    fn miette_spans() {
        let mut sources = SourceTable::default();
        let id1 = sources.insert("const x = 1;".into()).unwrap();
        let id2 = sources.insert("const y = 2;".into()).unwrap();

        // PS -> miette: test bit packing
        let miette_span1 = Span {
            source_id: id1,
            start_offset: 6,
            end_offset: 7,
        }
        .into_miette();
        assert_eq!(
            miette_span1,
            // ID is 0 so the offset is the same as the original
            miette::SourceSpan::new(6.into(), 1)
        );
        let miette_span2 = Span {
            source_id: id2,
            start_offset: 6,
            end_offset: 7,
        }
        .into_miette();
        assert_eq!(
            miette_span2,
            // ID is 1
            miette::SourceSpan::new(((1 << 56) | 6).into(), 1)
        );

        // Read source code back, it should dispatch to the correct source
        assert_eq!(
            sources.read_span(&miette_span1, 0, 0).unwrap().data(),
            b"x"
        );
        assert_eq!(
            sources.read_span(&miette_span2, 0, 0).unwrap().data(),
            b"y"
        );
    }
}
