//! Sometimes bad things happen

use crate::{
    compile::SUPPORTED_EXTENSIONS,
    source::QualifiedSpan,
    value::{Number, ValueType},
    NativeModuleName,
};
use rslint_parser::ParserError;
use std::{
    error::Error as StdError,
    fmt::{self, Display},
    io,
    path::PathBuf,
    string::FromUtf8Error,
};
use winnow::error::ContextError;

/// Any error that can occur within PetitScript
#[derive(Debug)]
pub enum Error {
    /// Attempted to register two values of the same type as app data
    DuplicateAppData { type_name: &'static str },

    /// Attempted to import a module with an unrecognized file extension
    InvalidExtension { path: PathBuf },

    /// Attempted to register a native module with an invalid name
    InvalidModuleName(ModuleNameError),

    /// Error occurred while loading source code from a file
    Io {
        error: io::Error,
        path: Option<PathBuf>,
    },

    /// Error occurred while parsing source code. This indicates the source is
    /// not valid ECMAScript code
    Parse(ParseError),

    /// An error that occurs while transforming a parsed program from
    /// ECMAScript to the PetitScript abstract syntax tree. This indicates the
    /// source is valid JavaScript syntax, but is illegal in PetitScript.
    Transform {
        error: TransformError,
        /// The source location of the code that failed to transform
        span: QualifiedSpan,
    },

    /// An error that occurs while executing a PetitScript program. All runtime
    /// errors have an attached source span, indicating the point in the
    /// program where the error occurred.
    Runtime {
        error: RuntimeError,
        /// A trace showing every function call leading up to the error
        trace: StackTrace,
    },

    /// Requested app data of a type that has not been set
    UnknownAppData { type_name: &'static str },
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DuplicateAppData { type_name } => write!(
                f,
                "Multiple app data values of type `{type_name}` were registered"
            ),
            Self::InvalidModuleName(error) => write!(f, "{error}"),
            Self::InvalidExtension { path } => write!(
                f,
                "Unsupported extension for imported module at path {path:?}. \
                Supported extensions are: {SUPPORTED_EXTENSIONS:?}"
            ),
            Self::Io {
                error,
                path: Some(path),
            } => write!(f, "Error loading {path:?}: {error}"),
            Self::Io { error, path: None } => write!(f, "I/O error: {error}"),
            Self::Parse(error) => write!(f, "{error}"),
            Self::Transform { error, span } => {
                write!(f, "Error transforming AST at {span}: {error}")
            }
            Self::Runtime { error, trace } => {
                write!(f, "{trace}{error}")
            }
            Self::UnknownAppData { type_name } => {
                write!(f, "No app data of type `{type_name}` is registered")
            }
        }
    }
}

impl StdError for Error {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            Self::DuplicateAppData { .. } => None,
            Self::Io { error, .. } => Some(error),
            Self::InvalidExtension { .. } => None,
            Self::UnknownAppData { .. } => None,
            // For internal error types, there's no reason to add an additional
            // layer of source, so defer to the child
            Self::InvalidModuleName(error) => error.source(),
            Self::Parse(error) => error.source(),
            Self::Transform { error, .. } => error.source(),
            Self::Runtime { error, .. } => error.source(),
        }
    }
}

impl From<ModuleNameError> for Error {
    fn from(error: ModuleNameError) -> Self {
        Self::InvalidModuleName(error)
    }
}

impl From<ParseError> for Error {
    fn from(error: ParseError) -> Self {
        Self::Parse(error)
    }
}

impl From<TracedError> for Error {
    fn from(error: TracedError) -> Self {
        Self::Runtime {
            error: error.error,
            trace: error.trace,
        }
    }
}

#[cfg(test)]
static_assertions::assert_impl_all!(Error: Send, Sync);

/// Error occurred while parsing source code. This indicates the source is not
/// valid ECMAScript code
#[derive(Debug)]
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

impl StdError for ParseError {}

/// An error that occurs while transforming a parsed program from ECMAScript to
/// PetitScript
///
/// This indicates the source is valid ECMAScript syntax, but is illegal in
/// PetitScript.
#[derive(Debug)]
pub enum TransformError {
    /// A nested error that occurred while parsing an imported local module
    Import(Box<Error>),

    /// AST is missing a node that should be present
    Missing {
        /// Name of the type of node that was expected
        expected_type: &'static str,
    },

    /// Source code contains a syntax construct that isn't supported in our
    /// semantics
    Unsupported {
        name: &'static str,
        help: &'static str,
    },
}

impl Display for TransformError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Import(error) => write!(f, "{error}"),
            // TODO improve error message - this is probably invalid syntax
            Self::Missing { expected_type } => write!(f, "Expected AST node of type {expected_type}, but it was not present"),
            Self::Unsupported { name, help } => write!(f,"Unsupported: {name}; {help}"),
        }
    }
}

impl StdError for TransformError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            Self::Import(error) => Some(error),
            Self::Missing { .. } => None,
            Self::Unsupported { .. } => None,
        }
    }
}

/// Any error that can occur during the execution of a program
#[derive(Debug)]
pub enum RuntimeError {
    /// TODO
    AlreadyExported { name: String },

    /// Attempted to export from within a subscope
    IllegalExport,

    /// Attempted to return while not in a function
    IllegalReturn,

    /// Oopsies!
    /// TODO include bug report link here
    Internal(String),

    /// Second assignment to a `const` variable
    ImmutableAssign { name: String },

    /// Error in `JSON.parse`
    JsonParse { error: ContextError },

    /// Custom error type, for errors originating in user-provided native
    /// functions
    Other(Box<dyn StdError + Send + Sync>),

    /// Reference to an identifier that isn't bound
    Reference { name: String },

    /// Attempt to import a name that a module doesn't export
    UnknownImport { identifier: String },

    /// Attempt to import from a module that doesn't exist
    UnknownModule { name: NativeModuleName },

    /// Error converting to/from PS values
    Value(ValueError),

    /// A runtime error with an additional context message
    WithContext { message: String, error: Box<Self> },
}

impl RuntimeError {
    /// Wrap a custom error type
    pub fn other(error: impl 'static + StdError + Send + Sync) -> Self {
        Self::Other(error.into())
    }

    /// Create an error that indicates an internal bug in the runtime
    pub(crate) fn internal(message: impl Into<String>) -> Self {
        Self::Internal(message.into())
    }

    /// Wrap this error to attach an additional context message to it
    pub fn context(self, context: impl Into<String>) -> Self {
        Self::WithContext {
            message: context.into(),
            error: Box::new(self),
        }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::AlreadyExported { name } => {
                write!(f, "`{name}` is already exported")
            }
            Self::IllegalExport => {
                write!(f, "Export only allowed in program root")
            }
            Self::IllegalReturn => {
                write!(f, "Cannot return while not in function")
            }
            Self::Internal(message) => write!(
                f,
                "Internal error occurred in the PetitScript engine. \
                This is a bug; please report it! {message}"
            ),
            Self::ImmutableAssign { name } => {
                write!(f, "Assignment to immutable variable `{name}`")
            }
            Self::JsonParse { error } => write!(f, "TODO: {error}"),
            Self::Other(error) => write!(f, "{error}"),
            Self::Reference { name } => write!(f, "`{name}` is not defined"),
            Self::UnknownImport { identifier } => {
                write!(f, "Module does not export the name `{identifier}`")
            }
            Self::UnknownModule { name } => write!(
                f,
                "Unknown module `{name}`. Native modules must be registered \
                with the engine. If you meant to import from a local file, \
                try `./{name}`"
            ),
            Self::Value(error) => write!(f, "{error}"),
            Self::WithContext { message, .. } => write!(f, "{message}"),
        }
    }
}

impl StdError for RuntimeError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            Self::AlreadyExported { .. } => None,
            Self::IllegalExport => None,
            Self::IllegalReturn => None,
            Self::Internal(_) => None,
            Self::ImmutableAssign { .. } => None,
            Self::JsonParse { .. } => None,
            // This is a transparent wrapper. We're going to print the error
            // in our display, so don't print it again as the source
            Self::Other(error) => error.source(),
            Self::Reference { .. } => None,
            Self::UnknownImport { .. } => None,
            Self::UnknownModule { .. } => None,
            Self::Value(error) => Some(error),
            Self::WithContext { error, .. } => Some(error),
        }
    }
}

impl From<ValueError> for RuntimeError {
    fn from(error: ValueError) -> Self {
        Self::Value(error)
    }
}

/// A runtime error paired with its stack trace. This is only for internal use,
/// to pack the two together. It's much easier than including the stack trace
/// on every variant of [RuntimeError]. We unpack this into [Error::Runtime]
/// before returning it to the user, to remove an unnecessary layer of
/// indirection.
pub(crate) struct TracedError {
    pub error: RuntimeError,
    pub trace: StackTrace,
}

/// An error that can occur while converting to/from [Value](crate::Value)
#[derive(Debug)]
pub enum ValueError {
    /// A custom error message, e.g. for deserialization
    Custom(String),

    /// Attempted to deserialize into a [Function] from a data format other
    /// than [Value]. [Function] can only be serialized into/deserialized from
    /// [Function]; it cannot be represented in other serde data formats or
    /// transformed into another data type.
    FunctionSerde,

    /// Attempted to convert non-UTF-8 bytes to a string
    InvalidUtf8(FromUtf8Error),

    /// Error converting [Number] to a specific number type. Could be a
    /// mismatch (float -> int or vice versa), or a value out of range
    Number {
        expected: &'static str,
        number: Number,
        description: &'static str,
    },

    /// Wrapper for an external error type
    Other(Box<dyn StdError + Send + Sync>),

    /// An operation required a specific type (or types), but received a value
    /// of an unsupported type. This commonly occurs during type downcasting,
    /// e.g. [Value](crate::Value) to [Number].
    Type {
        expected: ValueType,
        actual: ValueType,
    },
}

impl ValueError {
    /// Wrap a custom error type
    pub fn other(error: impl 'static + StdError + Send + Sync) -> Self {
        Self::Other(error.into())
    }
}

impl Display for ValueError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Custom(message) => write!(f, "{message}"),
            Self::FunctionSerde => {
                write!(
                    f,
                    "Invalid function serialization/deserialization. \
                    PetitScript functions can only be serialized into and \
                    deserialized from petitscript::Value. Other data formats \
                    are not supported."
                )
            }
            Self::InvalidUtf8(error) => write!(f, "{error}"),
            Self::Number {
                expected,
                number,
                description,
            } => write!(
                f,
                "Cannot convert number {number} to {expected}: {description}"
            ),
            Self::Other(error) => write!(f, "{error}"),
            Self::Type { expected, actual } => {
                write!(f, "Type error: expected {expected}, received {actual}")
            }
        }
    }
}

impl StdError for ValueError {
    fn cause(&self) -> Option<&dyn StdError> {
        match self {
            Self::Custom(_) => None,
            Self::FunctionSerde { .. } => None,
            Self::InvalidUtf8(error) => Some(error),
            Self::Number { .. } => None,
            Self::Other(error) => Some(&**error),
            Self::Type { .. } => None,
        }
    }
}

impl From<FromUtf8Error> for ValueError {
    fn from(error: FromUtf8Error) -> Self {
        Self::InvalidUtf8(error)
    }
}

/// Error while parsing a string to
/// [Identifier](crate::ast::Identifier)
#[derive(Debug)]
pub struct IdentifierError {
    pub identifier: String,
}

impl Display for IdentifierError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { identifier } = self;
        write!(
            f,
            "Invalid identifier: `{identifier}`; identifiers must not be \
            empty, TODO update this"
        )
    }
}

impl StdError for IdentifierError {}

/// Error while parsing a string to [NativeModuleName]
#[derive(Debug)]
pub struct ModuleNameError {
    pub name: String,
}

impl Display for ModuleNameError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { name } = self;
        write!(
            f,
            "Invalid module name: `{name}`; module names must not be empty, \
            and can only contain alphanumeric characters, hyphens (-) or \
            underscores (_)"
        )
    }
}

impl StdError for ModuleNameError {}

/// A list of source locations representing the function call stack
///
/// This captures the call stack at a particular moment in time, typically when
/// an error occurs. The most recent call is _last_ on this stack.
#[derive(Debug, Default)]
pub struct StackTrace(Vec<StackTraceFrame>);

impl StackTrace {
    /// Get an iterator over the frames of this stack. The most recent call will
    /// be _last_.
    pub fn iter(&self) -> impl Iterator<Item = &StackTraceFrame> {
        self.0.iter()
    }
}

impl Display for StackTrace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
