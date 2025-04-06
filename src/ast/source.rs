//! Utilities for interacting with source code

use crate::Error;
use std::{
    borrow::Cow,
    env,
    fmt::{self, Debug, Display},
    fs,
    hash::Hash,
    ops::{Deref, DerefMut},
    path::{Path, PathBuf},
};

/// A source of source code. E.g. a string literal or a file path
pub trait Source: 'static + Debug + Send + Sync {
    /// TODO
    fn name(&self) -> Option<&str>;

    /// Root path for local module imports. Import paths will be relative to
    /// this path. For file-less source strings, return `None` and the current
    /// working directory will be used.
    fn import_root(&self) -> Option<&Path>;

    /// TODO
    fn text(&self) -> Result<Cow<'_, str>, Error>;
}

impl Source for &'static str {
    fn name(&self) -> Option<&str> {
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
    fn name(&self) -> Option<&str> {
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
    fn name(&self) -> Option<&str> {
        self.to_str()
    }

    fn import_root(&self) -> Option<&Path> {
        Some(self)
    }

    fn text(&self) -> Result<Cow<'_, str>, Error> {
        Ok(fs::read_to_string(self)
            .map_err(|error| Error::Io {
                path: self.clone(),
                error,
            })?
            .into())
    }
}

/// A range of source code, defined as byte offsets. This is the raw mapping
/// that we pass around within the compiler/interpreter. To get a user-friendly
/// version of this TODO
///
/// The parser we use only provides byte offsets for its AST nodes, so we defer
/// mapping bytes to line/column until it's actually needed.
#[derive(Copy, Clone, Debug)]
pub enum Span {
    /// A location in PetitScript source code
    Source {
        /// Byte offset for the beginning of this span (inclusize). Always <=
        /// end_span.
        start_offset: usize,
        /// Byte offset for the end of this span (inclusive). Always >=
        /// start_span.
        end_offset: usize,
    },
    /// A location in Rust native code. This carries no actual data because we
    /// don't track Rust source locations.
    /// TODO maybe we _can_ track native code too? look into it
    Native,
    /// A test-only variant that is considered equal to any other span. Useful
    /// for creating assertions that don't care about source spans
    #[cfg(test)]
    Any,
}

impl Span {
    /// Create a new source span
    pub fn new(start_offset: usize, end_offset: usize) -> Self {
        Self::Source {
            start_offset,
            end_offset,
        }
    }

    /// Map this span to source code. This converts byte offsets into
    /// lines/columns, so it can be displayed to the user.
    pub fn qualify(self, source: &dyn Source) -> QualifiedSpan {
        match self {
            Self::Source {
                start_offset,
                end_offset,
            } => {
                let text = &source.text().expect("TODO");
                // Lines/columns are 1-indexed, because reasons
                let mut line = 1;
                let mut column = 1;
                let mut start_line = 0;
                let mut start_column = 0;
                let mut end_line = 0;
                let mut end_column = 0;
                for (index, c) in text.char_indices() {
                    if index == start_offset {
                        start_line = line;
                        start_column = column;
                    }
                    if index == end_offset {
                        end_line = line;
                        end_column = column;
                    }
                    if index > end_offset {
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
                    source_name: source.name().unwrap_or("").to_owned(),
                    start_line,
                    start_column,
                    end_line,
                    end_column,
                }
            }
            Self::Native => QualifiedSpan::Native,
            #[cfg(test)]
            Self::Any => unimplemented!("`Any` spans cannot be qualified"),
        }
    }
}

#[cfg(test)]
impl PartialEq for Span {
    fn eq(&self, other: &Self) -> bool {
        todo!()
    }
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

/// Some data, with a source span attached. This is used to attach source
/// mappings to AST nodes, so error messages can point back to the originating
/// source code.
#[derive(Copy, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Spanned<T> {
    pub data: T,
    pub span: Span,
}

/// A transparent Debug implementation. This makes AST debug printing much
/// easier to read. Generally the spans are not useful.
impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.data.fmt(f)
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

/// This `Hash` implementation **ignores** the span, because AST hashes should
/// only be a function of the structure of a program, not its concrete source
/// code.
impl<T: Hash> Hash for Spanned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.data.hash(state);
    }
}

/// TODO
pub trait IntoSpanned: Sized {
    /// TODO
    fn into_spanned(self, span: impl Into<Span>) -> Spanned<Self>;

    /// Wrap an AST node in a [Spanned], with a wildcard span that will
    /// match anything in quality checking. For AST comparisons
    /// where you don't care about source spans
    #[cfg(test)]
    fn s(self) -> Spanned<Self>;
}

impl<T> IntoSpanned for T {
    fn into_spanned(self, span: impl Into<Span>) -> Spanned<Self> {
        Spanned {
            data: self,
            span: span.into(),
        }
    }

    #[cfg(test)]
    fn s(self) -> Spanned<Self> {
        Spanned {
            data: self,
            span: Span::Any,
        }
    }
}

/// A list of source locations representing the function call stack at a
/// particular moment in time. Most recent call is _last_ on this stack.
#[derive(Debug)]
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_any() {
        assert_eq!(
            Span::Source {
                start_offset: 0,
                end_offset: 0
            },
            Span::Any
        );
    }
}
