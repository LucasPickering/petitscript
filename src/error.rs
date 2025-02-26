use crate::{
    ast::source::{IntoSpanned, QualifiedSpan, Span, Spanned},
    function::Function,
    value::ValueType,
    Number,
};
use rslint_parser::ParserError;
use std::{
    fmt::{self, Display},
    io,
    string::FromUtf8Error,
};
use thiserror::Error;

/// Any error that can occur within PetitJS
#[derive(Debug, Error)]
pub enum Error {
    /// Attempted to register two values of the same type as app data
    #[error("Multiple app data values of type `{type_name}` were registered")]
    DuplicateAppData { type_name: &'static str },

    /// Error occurred while loading source code from a file or other I/O
    /// source
    #[error(transparent)]
    Io(#[from] io::Error),

    /// Error occurred while parsing source code. This indicates the source is
    /// not valid ECMAScript code
    #[error(transparent)]
    Parse(#[from] ParseError),

    /// An error that occurs while transforming a parsed program from
    /// ECMAScript to the PetitJS abstract syntax tree. This indicates the
    /// source is valid JavaScript syntax, but is illegal in PetitJS.
    #[error(transparent)]
    Transform(#[from] TransformError),

    /// An error that occurs while executing a program. All runtime errors have
    /// an attached source span, indicating the point in the program where the
    /// error occurred.
    #[error("Error at {span}: {error}")]
    Runtime {
        #[source]
        error: RuntimeError,
        span: QualifiedSpan,
    },

    /// Requested app data of a type that has not been set
    #[error("No app data of type `{type_name}` is registered")]
    UnknownAppData { type_name: &'static str },
}

#[cfg(test)]
static_assertions::assert_impl_all!(Error: Send, Sync);

/// Error occurred while parsing source code. This indicates the source is not
/// valid ECMAScript code
#[derive(Debug, Error)]
pub struct ParseError {
    pub source_name: Option<String>,
    pub errors: Vec<ParserError>,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.source_name.as_deref().unwrap_or("source code");
        writeln!(f, "Error parsing {name}:")?;
        for error in &self.errors {
            writeln!(f, "  {error:?}")?;
        }
        Ok(())
    }
}

/// An error that occurs while transforming a parsed program from ECMAScript to
/// the PetitJS abstract syntax tree. This indicates the source is valid
/// JavaScript syntax, but is illegal in PetitJS.
#[derive(Debug, Error)]
pub enum TransformError {
    /// Source code contains a syntax construct that isn't supported in our
    /// semantics
    #[error("Unsupported: {name}; {help}")]
    Unsupported {
        name: &'static str,
        help: &'static str,
    },
    /// AST is missing a node that should be present
    #[error("TODO")]
    Missing,
}

/// Any error that can occur during the execution of a program
#[derive(Debug, Error)]
pub enum RuntimeError {
    /// TODO
    #[error("`{name}` is already exported")]
    AlreadyExported { name: String },

    /// Custom error type, for errors originating in user-provided native
    /// functions
    #[error(transparent)]
    Custom(Box<dyn std::error::Error + Send + Sync>),

    /// Attempted to export from within a subscope
    #[error("Export only allowed in program root")]
    IllegalExport,

    /// Attempted to return while not in a function
    #[error("Cannot return while not in function")]
    IllegalReturn,

    /// Oopsies!
    /// TODO include bug report link here
    #[error(
        "Internal error occurred in the PetitJS engine. \
        This is a bug; please report it! {0}"
    )]
    Internal(String),

    /// Second assignment to a `const` variable
    #[error("Assignment to immutable variable `{name}`")]
    ImmutableAssign { name: String },

    /// Reference to an identifier that isn't bound
    #[error("`{name}` is not defined")]
    Reference { name: String },

    /// TODO
    #[error("Unknown function {function}")]
    UnknownFunction { function: Function },

    /// Error converting to/from JS values
    #[error(transparent)]
    Value(#[from] ValueError),
}

impl RuntimeError {
    /// Wrap a custom error type
    pub fn custom(
        error: impl 'static + std::error::Error + Send + Sync,
    ) -> Self {
        Self::Custom(error.into())
    }

    /// Create an error that indicates an internal bug in the runtime
    pub(crate) fn internal(message: impl ToString) -> Self {
        Self::Internal(message.to_string())
    }
}

/// An error that can occur while converting to/from [Value](crate::Value)
#[derive(Debug, Error)]
pub enum ValueError {
    /// TODO
    #[error("{0}")]
    Custom(String),

    /// Attempted to convert non-UTF-8 bytes to a string
    #[error("TODO")]
    InvalidUtf8(#[from] FromUtf8Error),

    /// Error converting [Number] to a specific number type. Could be a
    /// mismatch (float -> int or vice versa), or a value out of range
    #[error("Cannot convert number {number} to {expected}: {description}")]
    Number {
        expected: &'static str,
        number: Number,
        description: &'static str,
    },

    /// An operation required a specific type (or types), but received a value
    /// of an unsupported type. This commonly occurs during type downcasting,
    /// e.g. [Value] to [Number].
    #[error("Type error: expected {expected}, received {actual}")]
    Type {
        expected: ValueType,
        actual: ValueType,
    },
}

pub(crate) trait ResultExt<T, E> {
    /// Convert a `Result<T, E>` to a `Result<T, Spanned<E>>` by attaching a
    /// span to the error
    fn spanned_err(self, span: Span) -> Result<T, Spanned<RuntimeError>>;
}

impl<T, E: Into<RuntimeError>> ResultExt<T, E> for Result<T, E> {
    fn spanned_err(self, span: Span) -> Result<T, Spanned<RuntimeError>> {
        self.map_err(|error| error.into().into_spanned(span))
    }
}
