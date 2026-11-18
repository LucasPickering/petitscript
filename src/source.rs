//! Utilities for interacting with source code

use crate::{ast::NodeId, error::SourceError};
use normalize_path::NormalizePath;
use std::{
    collections::HashMap,
    fmt::Debug,
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
    /// [`ParseErrorKind::ImportInvalidLocal`](crate::error::ParseErrorKind::ImportInvalidLocal).
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

/// Convert a string literal to [`Source::Static`]
impl From<&'static str> for Source {
    fn from(value: &'static str) -> Self {
        Self::Static(value)
    }
}

/// Convert a [`String`] to [`Source::String`]
impl From<String> for Source {
    fn from(value: String) -> Self {
        Self::String(value.into())
    }
}

/// Cheap ID for a specific [`Source`] in a [`SourceTable`]
///
/// Sources get aliased by ID because each [`Span`] needs to point to its
/// source, and each AST node needs a span.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct SourceId(u16);

/// A table of [`Source`]s for a single program
///
/// The parser builds, uses, and returns this as a centralized store of all
/// source code that produced a program. Each source (each file typically) is
/// assigned an ID that can be used in [`Span`] throughout the AST.
#[derive(Clone, Debug, Default)]
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
    /// [`SourceTable::insert`] will be valid for that table.
    pub fn get(&self, id: SourceId) -> &Source {
        self.0.get(&id).expect("TODO")
    }

    /// Add a new source to the table, returning its new unique ID
    ///
    /// Return an error if the table already has the max number of sources
    /// (256).
    pub fn insert(&mut self, source: Source) -> SourceId {
        let id = SourceId(self.0.len() as u16);
        self.0.insert(id, source);
        id
    }

    /// Get an iterator over the sources
    pub fn iter(&self) -> impl Iterator<Item = &Source> {
        self.0.values()
    }
}

/// A range of source code, defined as byte offsets
///
/// TODO more
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Span {
    /// ID of the [`Source`] (typically a file) containing the code
    ///
    /// This is just an ID because we create thousands of spans, so we need
    /// this to be cheap. Use [`SourceTable`] to look up the actual source.
    pub source_id: SourceId,
    /// Byte offset for the beginning of this span (inclusive)
    ///
    /// Always <= `end_span`.
    pub start_offset: u32,
    /// Byte offset for the end of this span (inclusive)
    ///
    /// Always >= `start_span`.
    pub end_offset: u32,
}

impl Span {
    /// Construct a new span
    #[cfg(test)]
    pub fn new(
        source_id: SourceId,
        start_offset: u32,
        end_offset: u32,
    ) -> Self {
        Self {
            source_id,
            start_offset,
            end_offset,
        }
    }
}

/// A table of all spans for each node in the AST
///
/// The parser is responsible for generating this and ensuring every AST node
/// has an entry.
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
