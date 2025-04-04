use crate::{
    ast::{
        source::{IntoSpanned, QualifiedSpan, Span, Spanned},
        NativeModuleName,
    },
    compile::FunctionDefinitionId,
    value::ValueType,
    Number,
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

    /// Attempted to register a native module with an invalid name
    InvalidModuleName(ModuleNameError),

    /// Error occurred while loading source code from a file
    Io { error: io::Error, path: PathBuf },

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
        /// The source location of the code that failed
        span: QualifiedSpan,
    },

    /// Requested app data of a type that has not been set
    UnknownAppData { type_name: &'static str },
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DuplicateAppData { type_name } => write!(f,"Multiple app data values of type `{type_name}` were registered"),
            Self::InvalidModuleName(error) => write!(f,"{error}"),
            Self::Io { error, path } => write!(f,"Error loading {path:?}: {error}"),
            Self::Parse(error) => write!(f, "{error}"),
            Self::Transform { error, span } => {
                write!(f, "Error transforming AST at {span}: {error}")
            }
            Self::Runtime { error, span } => {
                write!(f, "Error at {span}: {error}")
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
            Self::InvalidModuleName(error) => Some(error),
            Self::Io { error, .. } => Some(error),
            Self::Parse(error) => Some(error),
            Self::Transform { error, .. } => Some(error),
            Self::Runtime { error, .. } => Some(error),
            Self::UnknownAppData { .. } => None,
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
/// the PetitScript abstract syntax tree. This indicates the source is valid
/// JavaScript syntax, but is illegal in PetitScript.
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

    /// Error in `JSO.parse`
    JsonParse { error: ContextError },

    /// Custom error type, for errors originating in user-provided native
    /// functions
    Other(Box<dyn StdError + Send + Sync>),

    /// Reference to an identifier that isn't bound
    Reference { name: String },

    /// Attempt to import from a module that doesn't exist
    UnknownModule { name: NativeModuleName },

    /// TODO
    UnknownUserFunction(FunctionDefinitionId),

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
    pub(crate) fn internal(message: impl ToString) -> Self {
        Self::Internal(message.to_string())
    }

    /// Wrap this error to attach an additional context message to it
    pub fn context(self, context: impl ToString) -> Self {
        Self::WithContext {
            message: context.to_string(),
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
            Self::UnknownModule { name } => write!(
                f,
                "Unknown module `{name}`. Native modules must be registered \
                with the engine. If you meant to import from a local file, \
                try `./{name}`"
            ),
            Self::UnknownUserFunction(id) => {
                write!(f, "Unknown user function with definition ID {id:?}")
            }
            Self::Value(error) => write!(f, "{error}"),
            Self::WithContext { message, .. } => write!(f, "{message}"),
        }
    }
}

impl StdError for RuntimeError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            RuntimeError::AlreadyExported { .. } => None,
            RuntimeError::IllegalExport => None,
            RuntimeError::IllegalReturn => None,
            RuntimeError::Internal(_) => None,
            RuntimeError::ImmutableAssign { .. } => None,
            RuntimeError::JsonParse { .. } => None,
            RuntimeError::Other(error) => Some(&**error),
            RuntimeError::Reference { .. } => None,
            RuntimeError::UnknownModule { .. } => None,
            RuntimeError::UnknownUserFunction(_) => None,
            RuntimeError::Value(error) => Some(error),
            RuntimeError::WithContext { error, .. } => Some(error),
        }
    }
}

impl From<ValueError> for RuntimeError {
    fn from(error: ValueError) -> Self {
        Self::Value(error)
    }
}

/// An error that can occur while converting to/from [Value](crate::Value)
#[derive(Debug)]
pub enum ValueError {
    /// A custom error message, e.g. for deserialization
    Custom(String),

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
