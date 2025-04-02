use crate::{
    ast::source::{IntoSpanned, QualifiedSpan, Span, Spanned},
    compile::FunctionDefinitionId,
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

/// Any error that can occur within PetitScript
#[derive(Debug, Error)]
pub enum Error {
    /// Attempted to register two values of the same type as app data
    #[error("Multiple app data values of type `{type_name}` were registered")]
    DuplicateAppData { type_name: &'static str },

    /// Attempted to register a native module with an invalid name
    #[error(transparent)]
    InvalidModuleName(#[from] ModuleNameError),

    /// Error occurred while loading source code from a file or other I/O
    /// source
    #[error(transparent)]
    Io(#[from] io::Error),

    /// Error occurred while parsing source code. This indicates the source is
    /// not valid ECMAScript code
    #[error(transparent)]
    Parse(#[from] ParseError),

    /// An error that occurs while transforming a parsed program from
    /// ECMAScript to the PetitScript abstract syntax tree. This indicates the
    /// source is valid JavaScript syntax, but is illegal in PetitScript.
    #[error("Error transforming AST at {span}: {error}")]
    Transform {
        #[source]
        error: TransformError,
        /// The source location of the code that failed to transform
        span: QualifiedSpan,
    },

    /// An error that occurs while executing a PetitScript program. All runtime
    /// errors have an attached source span, indicating the point in the
    /// program where the error occurred.
    #[error("Error at {span}: {error}")]
    Runtime {
        #[source]
        error: RuntimeError,
        /// The source location of the code that failed
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
/// the PetitScript abstract syntax tree. This indicates the source is valid
/// JavaScript syntax, but is illegal in PetitScript.
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
    /// TODO improve error message - this is probably invalid syntax
    #[error(
        "Expected AST node of type {expected_type}, but it was not present"
    )]
    Missing {
        /// Name of the type of node that was expected
        expected_type: &'static str,
    },
}

/// Any error that can occur during the execution of a program
#[derive(Debug, Error)]
pub enum RuntimeError {
    /// TODO
    #[error("`{name}` is already exported")]
    AlreadyExported { name: String },

    /// Attempted to export from within a subscope
    #[error("Export only allowed in program root")]
    IllegalExport,

    /// Attempted to return while not in a function
    #[error("Cannot return while not in function")]
    IllegalReturn,

    /// Oopsies!
    /// TODO include bug report link here
    #[error(
        "Internal error occurred in the PetitScript engine. \
        This is a bug; please report it! {0}"
    )]
    Internal(String),

    /// Second assignment to a `const` variable
    #[error("Assignment to immutable variable `{name}`")]
    ImmutableAssign { name: String },

    /// Custom error type, for errors originating in user-provided native
    /// functions
    #[error(transparent)]
    Other(Box<dyn std::error::Error + Send + Sync>),

    /// Reference to an identifier that isn't bound
    #[error("`{name}` is not defined")]
    Reference { name: String },

    /// TODO
    #[error("Unknown user function with definition ID {0:?}")]
    UnknownUserFunction(FunctionDefinitionId),

    /// Error converting to/from PS values
    #[error(transparent)]
    Value(#[from] ValueError),

    /// A runtime error with an additional context message
    #[error("{message}")]
    WithContext {
        message: String,
        #[source]
        source: Box<Self>,
    },
}

impl RuntimeError {
    /// Wrap a custom error type
    pub fn other(
        error: impl 'static + std::error::Error + Send + Sync,
    ) -> Self {
        Self::Other(error.into())
    }

    /// Create an error that indicates an internal bug in the runtime
    pub(crate) fn internal(message: impl ToString) -> Self {
        Self::Internal(message.to_string())
    }

    /// Wrap this error to attach an additional context message to it
    pub fn context(self, context: impl ToString) -> Self {
        Self::WithContext {
            message: context.to_string(),
            source: Box::new(self),
        }
    }
}

/// An error that can occur while converting to/from [Value](crate::Value)
#[derive(Debug, Error)]
pub enum ValueError {
    /// A custom error message, e.g. for deserialization
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

    /// Wrapper for an external error type
    #[error(transparent)]
    Other(Box<dyn std::error::Error + Send + Sync>),

    /// An operation required a specific type (or types), but received a value
    /// of an unsupported type. This commonly occurs during type downcasting,
    /// e.g. [Value](crate::Value) to [Number].
    #[error("Type error: expected {expected}, received {actual}")]
    Type {
        expected: ValueType,
        actual: ValueType,
    },
}

impl ValueError {
    /// Wrap a custom error type
    pub fn other(
        error: impl 'static + std::error::Error + Send + Sync,
    ) -> Self {
        Self::Other(error.into())
    }
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

/// TODO
#[derive(Debug, Error)]
#[error(
    "Invalid module name: `{name}`; module names must not be empty, and can \
    only contain alphanumeric characters, hyphens (-) or underscores (_)"
)]
pub struct ModuleNameError {
    pub name: String,
}
