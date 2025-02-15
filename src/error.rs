use crate::value::ValueType;
use rslint_parser::ParserError;
use std::{
    fmt::{self, Display},
    io,
    string::FromUtf8Error,
};
use thiserror::Error;

pub type RuntimeResult<T> = Result<T, RuntimeError>;

/// TODO
#[derive(Debug, Error)]
pub enum Error {
    /// TODO add error message
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error(transparent)]
    Parse(#[from] ParseError),
    #[error(transparent)]
    Transform(#[from] TransformError),
    #[error(transparent)]
    Runtime(#[from] RuntimeError),
}

#[cfg(test)]
static_assertions::assert_impl_all!(Error: Send, Sync);

/// TODO
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

/// TODO
#[derive(Debug, Error)]
pub enum TransformError {
    /// Source code contains a syntax construct that isn't supported in our
    /// semantics
    #[error("Unsupported: {name}; {help}")]
    Unsupported {
        name: &'static str,
        help: &'static str,
    },
    /// TODO
    #[error("TODO")]
    Missing,
}

/// An error that occurred while executing a script or module
#[derive(Debug, Error)]
pub enum RuntimeError {
    /// TODO
    #[error("{name} is already exported")]
    AlreadyExported { name: String },

    /// Custom error type, for errors originating in user code
    #[error(transparent)]
    Custom(Box<dyn std::error::Error + Send + Sync>),

    /// Attempted to register two values of the same type as app data
    #[error("Multiple app data values of type `{type_name}` were registered")]
    DuplicateAppData { type_name: &'static str },

    /// Attempted to export from within a subscope
    #[error("Export only allowed in program root")]
    IllegalExport,

    /// Attempted to return while not in a function
    #[error("Cannot return while not in function")]
    IllegalReturn,

    /// Oopsies!
    #[error(
        "Internal error occurred in the PetitJS engine. \
        This is a bug; please report it! {0}"
    )]
    Internal(String),

    /// Attempted to convert non-UTF-8 bytes to a string
    #[error("TODO")]
    InvalidUtf8(#[from] FromUtf8Error),

    /// Second assignment to a `const` variable
    #[error("Assignment to immutable variable {name}")]
    ImmutableAssign { name: String },

    /// Reference to an identifier that isn't bound
    #[error("{name} is not defined")]
    Reference { name: String },

    /// An operation required a specific type (or types), but received a value
    /// of an unsupported type. This commonly occurs during type downcasting,
    /// e.g. [Value] to [Number].
    #[error("Type error: expected {expected}, received {actual}")]
    Type {
        expected: ValueType,
        actual: ValueType,
    },

    #[error("No app data of type `{type_name}` is registered")]
    UnknownAppData { type_name: &'static str },
}

impl RuntimeError {
    /// Wrap a custom error type
    pub fn custom(
        error: impl 'static + std::error::Error + Send + Sync,
    ) -> Self {
        Self::Custom(error.into())
    }

    pub(crate) fn internal(message: impl ToString) -> Self {
        Self::Internal(message.to_string())
    }
}
