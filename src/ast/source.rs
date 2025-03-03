//! Utilities for interacting with source code

use crate::Error;
use std::{
    borrow::Cow,
    fmt::{self, Debug, Display},
    fs,
    hash::Hash,
    ops::{Deref, DerefMut},
    path::PathBuf,
};

/// A source of source code. E.g. a string literal or a file path
pub trait Source: 'static + Debug + Send + Sync {
    /// TODO
    fn name(&self) -> Option<&str>;

    /// TODO
    fn text(&self) -> Result<Cow<'_, str>, Error>;
}

impl Source for &'static str {
    fn name(&self) -> Option<&str> {
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

    fn text(&self) -> Result<Cow<'_, str>, Error> {
        Ok(self.as_str().into())
    }
}

impl Source for PathBuf {
    fn name(&self) -> Option<&str> {
        self.to_str()
    }

    fn text(&self) -> Result<Cow<'_, str>, Error> {
        Ok(fs::read_to_string(self)?.into())
    }
}

/// A range of source code, defined as byte offsets. This is the raw mapping
/// that we pass around within the compiler/interpreter. To get a user-friendly
/// version of this TODO
///
/// The parser we use only provides byte offsets for its AST nodes, so we defer
/// mapping bytes to line/column until it's actually needed.
#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Span {
    /// Byte offset for the beginning of this span (inclusize). Always <=
    /// end_span.
    pub start_offset: usize,
    /// Byte offset for the end of this span (inclusive). Always >= start_span.
    pub end_offset: usize,
}

impl Span {
    pub fn new(start_offset: usize, end_offset: usize) -> Self {
        Self {
            start_offset,
            end_offset,
        }
    }

    /// Map this span to source code. This converts byte offsets into
    /// lines/columns, so it can be displayed to the user.
    pub fn qualify(self, source: &dyn Source) -> QualifiedSpan {
        let text = &source.text().expect("TODO");
        // Lines/columns are 1-indexed, because reasons
        let mut line = 1;
        let mut column = 1;
        let mut span = QualifiedSpan {
            source_name: source.name().unwrap_or("").to_owned(),
            start_line: 0,
            start_column: 0,
            end_line: 0,
            end_column: 0,
        };
        for (index, c) in text.char_indices() {
            if index == self.start_offset {
                span.start_line = line;
                span.start_column = column;
            }
            if index == self.end_offset {
                span.end_line = line;
                span.end_column = column;
            }
            if index > self.end_offset {
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
        span
    }
}

/// A range of source code, mapped to lines/columns. This format is suitable
/// for error printing.
///
/// TODO should this just be "Span" since it's external-facing?
#[derive(Debug)]
pub struct QualifiedSpan {
    // TODO comments
    pub source_name: String,
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

impl Display for QualifiedSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO should we show end line/column as well?
        write!(
            f,
            "{}:{}:{}",
            self.source_name, self.start_line, self.start_column
        )
    }
}

/// TODO
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Spanned<T> {
    pub data: T,
    pub span: Span,
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
    fn into_spanned(self, span: Span) -> Spanned<Self>;
}

impl<T> IntoSpanned for T {
    fn into_spanned(self, span: Span) -> Spanned<Self> {
        Spanned { data: self, span }
    }
}
