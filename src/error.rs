//! Sometimes bad things happen

use crate::{
    NativeModuleName, Source, Value,
    compile::SUPPORTED_EXTENSIONS,
    source::QualifiedSpan,
    value::{Number, ValueType},
};
use miette::{Diagnostic, LabeledSpan, Severity};
use std::{
    borrow::Borrow,
    error::Error as StdError,
    fmt::{self, Display},
    io,
    path::PathBuf,
    string::FromUtf8Error,
};
use winnow::error::ContextError;

// TODO consistent capitalization in errors. Probably should all be lower case

/// Any error that can occur within PetitScript
///
/// TODO explain sub-errors: ParseError and RuntimeError are returned when we
/// can be more specific.
/// - [ParseError] TODO
/// - [RuntimeError] TODO
#[derive(Debug, Diagnostic, thiserror::Error)]
pub enum Error {
    /// Attempted to register two values of the same type as app data
    ///
    /// See [Process::app_data](crate::Process::app_data).
    #[error("Multiple app data values of type `{type_name}` were registered")]
    DuplicateAppData { type_name: &'static str },

    /// Error(s) while parsing source code
    ///
    /// See [ParseErrors].
    #[diagnostic(transparent)]
    #[error(transparent)]
    Parse(ParseErrors),

    /// Error while executing a PetitScript program
    ///
    /// All runtime errors have an attached source span, indicating the point in
    /// the program where the error occurred.
    #[error("{trace}{error}")]
    Runtime {
        /// TODO
        #[source]
        error: RuntimeError,
        /// A trace showing every function call leading up to the error
        trace: StackTrace,
    },

    /// Requested app data of a type that has not been set
    ///
    /// See [Process::app_data](crate::Process::app_data).
    #[error("No app data of type `{type_name}` is registered")]
    UnknownAppData { type_name: &'static str },
}

impl From<ParseErrors> for Error {
    fn from(errors: ParseErrors) -> Self {
        Self::Parse(errors)
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

/// Container for one or more [ParseError]s during parsing
///
/// Parsing does not fail fast; it collects as many errors as it can, but will
/// fail if there is at least one.
///
/// This correponds to a single module's parse. Submodules will be included
/// recursively via [ParseErrorKind::Import].
#[derive(Debug, Diagnostic, thiserror::Error)]
#[error("{} parse error(s)", errors.len())]
pub struct ParseErrors {
    /// One or more parse errors
    #[related]
    pub errors: Vec<ParseError>,
    /// Module source code that failed to parse
    #[source_code]
    pub source_code: Source,
}

/// Allow `diagnostic_source()` through `Box<ParseErrors>`
///
/// Needed because [ParseErrorKind::Import] is recursive. Remove after
/// <https://github.com/zkat/miette/pull/367>.
impl Borrow<dyn miette::Diagnostic> for Box<ParseErrors> {
    fn borrow(&self) -> &(dyn miette::Diagnostic + 'static) {
        &**self
    }
}

/// Error occurred while parsing source code or transforming it to PetitScript
///
/// PetitScript uses a two-part parser:
/// - First, source code is parsed to an ECMAScript (JavaScript) abstract syntax
///   tree via [oxc_parser]
/// - Then, the ES AST is transformed to a PetitScript AST
///
/// The first step will fail if the source code is not valid syntax. The second
/// will fail if the code is not valid PetitScript. As PS is a small subset of
/// JS, there are many syntactic constructs in JS that are not valid PS.
#[derive(Debug, Diagnostic, thiserror::Error)]
#[error("parse")] // Use a short description because the cause is descriptive
pub struct ParseError {
    /// What bad thing happen
    #[diagnostic_source]
    #[source]
    pub error: ParseErrorKind,
    /// One or more spans related to the error
    ///
    /// In most cases this is just a single element pointing to the offending
    /// span.
    #[label(collection)]
    pub labels: Vec<LabeledSpan>,
}

/// The categories of errors that can occuring during parsing
///
/// This is contained by [ParseError], which carries the error info as well as
/// source context.
#[derive(Debug, Diagnostic, thiserror::Error)]
pub enum ParseErrorKind {
    /// A nested error while parsing an imported local module
    #[error("Importing '{module}'")]
    Import {
        /// Name/path of the module being imported
        module: String,
        /// Upstream error
        #[diagnostic_source]
        #[source]
        error: Box<ParseErrors>,
    },

    /// Attempted to import a module with an unrecognized file extension
    #[error(
        "Unsupported extension for imported module at path {path}. \
        Supported extensions are: {SUPPORTED_EXTENSIONS:?}",
        path = path.display()
    )]
    InvalidExtension { path: PathBuf },

    /// Attempted a local module import from a non-file source
    ///
    /// Non-file sources (i.e. sources defined in-memory) cannot import other
    /// files, as they do not exist on disk. A local import (e.g. `import ...
    /// from './other.js'`) will result in this error.
    #[error("Cannot import local module '{module}' from non-file source")]
    InvalidLocalImport {
        /// Name of the module being imported
        module: String,
    },

    /// Attempted to register a native module with an invalid name
    #[error(transparent)]
    InvalidModuleName(#[from] ModuleNameError),

    /// [oxc_parser] failed to parse the source
    ///
    /// This means the syntax isn't valid JavaScript, and therefore can't be
    /// valid PetitScript.
    ///
    /// This is built from [oxc_diagnostics::OxcDiagnostic]. `oxc` is used as
    /// the parser internally. Each error is remapped to this variant to
    /// prevent leaking the `oxc` type as part of PetitScript's external API.
    #[error("{message}")]
    Parse {
        // Omitting error code because I don't think it's useful
        message: String,
        #[help]
        help: Option<String>,
        note: Option<String>,
        severity: Severity,
    },

    /// I/O error while loading source code from a file
    ///
    /// See [SourceError].
    #[error(transparent)]
    Source(#[from] SourceError),

    /// AST node type wasn't handled
    ///
    /// This occurs when a node is downcast (using oxc's AST inheritance
    /// framework) but the downcast fails. It indicates a possible branch wasn't
    /// handled. **This is an internal error**.
    #[error("[INTERNAL ERROR] Unhandled AST node type: {node}")]
    Unhandled {
        /// Debug representation of the unhandled node
        node: String,
    },

    /// Source code contains a syntax construct that isn't supported in our
    /// semantics
    ///
    /// See [Unsupported].
    #[error("{0}")]
    Unsupported(Unsupported),
}

/// Error while loading source code from a file
#[derive(Debug, Diagnostic, thiserror::Error)]
#[error("Error reading source code from {}", self.path.display())]
pub struct SourceError {
    /// Path to the file being loaded
    pub path: PathBuf,
    /// Error that occurred
    #[source]
    pub error: io::Error,
}

/// Description of an unsupported feature
///
/// The vast majority of JS/TS features are not supported in PetitScript. This
/// enumeratures those various unsupported things so we can provide the best
/// feedback to the user possible.
///
/// This is *only* for syntax that is supported in most JS runtimes but not
/// ours. For syntax that's possible to construct but semantically invalid in
/// ECMAScript, do *not* use this type.
#[derive(Debug)]
pub enum Unsupported {
    /// `function()` (without identifier)
    AnonymousFunction,
    /// `[,,,]`
    ArrayElision,
    /// `async`/`await`
    Async,
    /// `import 'source'`
    BareImport,
    /// `~`, `&`, `|`, etc.
    Bitwise,
    /// `class` and friends
    Class,
    /// `**`
    ///
    /// Could support this one, I'm just being lazy.
    Exponent,
    /// `for ... in ...`
    ForIn,
    /// `const x = function() {}`
    ///
    /// This is different from [Self::AnonymousFunction] because function
    /// expressions may have an identifier, but are still illegal in an
    /// expression position.
    FunctionExpression,
    /// `function*`
    Generator,
    /// `import()`
    ImportExpression,
    /// `<Component></Component>`
    Jsx,
    /// `'label`, `break label`, `continue label`
    Label,
    /// `==` and `!=`
    LooseEquality,
    /// `let`, `var`, `delete`, and reassignment
    Mutation,
    /// `import * as local from 'source'`
    NamespaceImport,
    /// Non-UTF-8 string literal
    NonUtf,
    /// `{ f() {}, get g() {}, set g() {}  }`
    ObjectMethod,
    /// `a.#f`
    Private,
    /// ```
    /// tag`template`
    /// ```
    TaggedTemplate,
    /// `this`
    This,
    /// `using`
    Using,
    /// `void`
    Void,
    // TODO remove this!!
    Todo,
}

impl Display for Unsupported {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO single message for each variant, instead of 2
        let (name, help) = match self {
            Self::AnonymousFunction => (
                "anonymous `function`",
                "Functions declared with `function` must have a name. \
                Use `=>` for anonymous functions",
            ),
            Self::ArrayElision => {
                ("array elision", "Array elements cannot be elided")
            }
            Self::Async => ("`async`", "Async-await is not supported"),
            Self::BareImport => (
                "TODO",
                "Modules have no side-effects, \
                so there's no use for bare imports",
            ),
            Self::Bitwise => {
                ("bitwise", "Bitwise operations are not supported")
            }
            Self::Class => ("`class`", "Classes are not supported"),
            Self::Exponent => ("`**`", "Exponent operator is not supported"),
            Self::ForIn => ("`for...in`", "Use `for...of` instead"),
            Self::FunctionExpression => (
                "TODO",
                "`function` keyword not allowed in an expression position; \
                Use `=>` for function expressions",
            ),
            Self::Generator => ("generator", "Generators are not supported"),
            Self::ImportExpression => {
                ("`import()`", "Use the ES6-style `import` keyword instead")
            }
            Self::Jsx => ("JSX", "This isn't React..."),
            Self::Label => ("TODO", "TODO"),
            Self::LooseEquality => {
                ("loose equality", "use `===` or `!==` instead")
            }
            // TODO add link to docs explaining immutability
            Self::Mutation => (
                "`let`/`var`",
                "Mutable bindings are not supported; use `const` instead",
            ),
            Self::NamespaceImport => (
                "TODO",
                "List specific imports with `import { ... } from 'source'`",
            ),
            Self::NonUtf => (
                "non-UTF-8 string literal",
                "String literals must be valid UTF-8",
            ),
            Self::ObjectMethod => (
                "object method",
                "use a static field with an arrow function expression value",
            ),
            Self::Private => ("TODO", "TODO"),
            Self::TaggedTemplate => {
                ("tagged template", "Use untagged templates")
            }
            Self::This => ("`this`", "`this` is not supported"),
            Self::Using => ("`using`", "TODO"),
            Self::Void => ("`void`", "Void operator is not supported"),
            Self::Todo => ("TODO", "TODO"),
        };
        write!(f, "Unsupported: {name}; {help}")
    }
}

/// Any error that can occur during the execution of a program
#[derive(Debug, thiserror::Error)]
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
    ///
    /// TODO include bug report link here
    #[error(
        "Internal error occurred in the PetitScript engine. \
        This is a bug; please report it! {0}"
    )]
    Internal(String),

    /// Second assignment to a `const` variable
    #[error("Assignment to immutable variable `{name}`")]
    ImmutableAssign { name: String },

    /// Error in `JSON.parse`
    #[error("TODO: {error}")]
    JsonParse { error: ContextError },

    /// Custom error type, for errors originating in user-provided native
    /// functions
    #[error(transparent)]
    Other(Box<dyn StdError + Send + Sync>),

    /// Attempted to get a property of `null` or `undefined`
    ///
    /// TODO give this a more descriptive name
    #[error("Cannot read properties of `{value}` (property \"{property}\")")]
    Property { value: Value, property: Value },

    /// Reference to an identifier that isn't bound
    #[error("`{name}` is not defined")]
    Reference { name: String },

    /// Attempt to import a name that a module doesn't export
    #[error("Module does not export the name `{identifier}`")]
    UnknownImport { identifier: String },

    /// Attempt to import from a module that doesn't exist
    #[error(
        "Unknown module `{name}`. Native modules must be registered \
                with the engine. If you meant to import from a local file, \
                try `./{name}`"
    )]
    UnknownModule { name: NativeModuleName },

    /// Error converting to/from PS values
    #[error(transparent)]
    Value(#[from] ValueError),

    /// A runtime error with an additional context message
    #[error("{message}")]
    WithContext {
        message: String,
        #[source]
        error: Box<Self>,
    },
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
    #[must_use]
    pub fn context(self, context: impl Into<String>) -> Self {
        Self::WithContext {
            message: context.into(),
            error: Box::new(self),
        }
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
