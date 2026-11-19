//! Sometimes bad things happen

use crate::{
    NativeModuleName, Value,
    compile::SUPPORTED_EXTENSIONS,
    source::{SourceTable, Span},
    value::{Number, ValueType},
};
use annotate_snippets::{AnnotationKind, Element, Level, Snippet};
use std::{
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
/// This is a catch-all container for all error types. It's never returned
/// directly by any functions in the library. It's just a convenience for
/// downstream code that wants to group different PetitScript-related errors
/// together.
///
/// Each variant corresponds to another error type in this module, and this
/// implements [`From`] for each variant. The two most common variants are:
/// - [`Error::Parse`]: Error while parsing/compiling a program
/// - [`Error::Runtime`]: Error while executing a program
#[derive(Debug, thiserror::Error)]
pub enum Error {
    /// Attempted to register two values of the same type as app data
    ///
    /// See [`DuplicateAppData`].
    #[error(transparent)]
    DuplicateAppData(#[from] DuplicateAppData),

    /// Error(s) while parsing source code
    ///
    /// See [`ParseErrors`].
    #[error(transparent)]
    Parse(#[from] ParseErrors),

    /// Error while executing a PetitScript program
    ///
    /// See [`RuntimeError`].
    #[error(transparent)]
    Runtime(#[from] RuntimeError),

    /// Error loading a source
    ///
    /// See [`SourceError`].
    #[error(transparent)]
    Source(#[from] SourceError),

    /// User requested app data of a type that has not been set
    ///
    /// See [`UnknownAppData`].
    #[error(transparent)]
    UnknownAppData(#[from] UnknownAppData),

    /// Error in value conversion
    #[error(transparent)]
    Value(#[from] ValueError),
}

#[cfg(test)]
static_assertions::assert_impl_all!(Error: Send, Sync);

/// Container for one or more [`ParseError`]s during parsing
///
/// Parsing does not fail fast. It collects as many errors as it can, but will
/// fail if there is at least one.
///
/// This can hold errors from multiple modules. Each error has a `span` field
/// indicating the file and location of the offending code.
///
///
/// ## Display
///
/// The default [`Display`] implementation for this type just prints the
/// number of parse errors. This ensures the message is concise and fits on one
/// line. To get the full error chain, print in alternate mode:
///
/// ```notest
/// println!("{errors:#}");
/// ```
///
/// If you want to customize the display further, you can use
/// [`ParseErrors::to_report`]. Combine this with the [`annotate_snippets`]
/// crate to customize the report renderer.
#[derive(Debug, thiserror::Error)]
pub struct ParseErrors {
    /// One or more parse errors
    pub errors: Vec<ParseError>,
    /// Map of all sources
    ///
    /// This will include the invalid module(s), as well as all valid ones.
    ///
    /// TODO make this public?
    pub(crate) source_table: SourceTable,
}

impl ParseErrors {
    /// Generate an [`annotate_snippets`] report of these errors
    ///
    /// This report can be used to render the errors prettily. See
    /// [`annotate_snippets::Renderer`].
    ///
    /// ```
    /// # use petitscript::Engine;
    /// let errors = Engine::default().parse("f(").unwrap_err();
    /// let renderer = annotate_snippets::Renderer::styled();
    /// eprintln!("{}", renderer.render(&errors.to_report()));
    /// ```
    pub fn to_report(&self) -> Vec<annotate_snippets::Group<'_>> {
        self.errors
            .iter()
            .map(|error| {
                let span = error.span;
                let source = self.source_table.get(span.source_id);
                Level::ERROR.primary_title(error.to_string()).element(
                    Snippet::source(source.text())
                        .path(source.name())
                        .annotation(AnnotationKind::Primary.span(
                            (span.start_offset as usize)
                                ..(span.end_offset as usize),
                        )),
                )
            })
            .collect()
    }
}

impl Display for ParseErrors {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} parse error(s)", self.errors.len())?;

        // In alternate mode, print the children as well. This behavior is
        // maybe(?) non-standard, but making the error chain easy to access is
        // really important.
        if f.alternate() {
            writeln!(f)?;
            let renderer = annotate_snippets::Renderer::plain();
            writeln!(f, "{}", renderer.render(&self.to_report()))?;
        }

        Ok(())
    }
}

/// Error occurred while parsing source code or transforming it to PetitScript
///
/// PetitScript uses a two-part parser:
/// - First, source code is parsed to an ECMAScript (JavaScript) abstract syntax
///   tree via [`oxc_parser`]
/// - Then, the ES AST is transformed to a PetitScript AST
///
/// The first step will fail if the source code is not valid syntax. The second
/// will fail if the code is not valid PetitScript. As PS is a small subset of
/// JS, there are many syntactic constructs in JS that are not valid PS.
///
/// This is typically contained within [`ParseErrors`], which aggregates all the
/// errors for a single parsing run.
#[derive(Debug)]
pub struct ParseError {
    /// What bad thing happen
    pub error: ParseErrorKind,
    /// Location of the offending code
    pub span: Span,
}

// ===============
// Manual impls are needed here to flatten the error stack
// ===============
impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.error.fmt(f)
    }
}

impl StdError for ParseError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        self.error.source()
    }
}

/// The categories of errors that can occuring during parsing
///
/// This is contained by [`ParseError`], which carries the error info as well as
/// source context.
#[derive(Debug, thiserror::Error)]
pub enum ParseErrorKind {
    /// Imported an absolute path: `import ... from '/data/lib.js'`
    #[error("illegal import from absolute path '{}'", path.display())]
    ImportAbsolute { path: PathBuf },

    /// Attempted to import a module with an unrecognized file extension
    #[error(
        "unsupported extension for imported module at path {path}. \
        Supported extensions are: {SUPPORTED_EXTENSIONS:?}",
        path = path.display()
    )]
    ImportInvalidExtension { path: PathBuf },

    /// Attempted a local module import from a non-file source
    ///
    /// Non-file sources (i.e. sources defined in-memory) cannot import other
    /// files, as they do not exist on disk. A local import (e.g. `import ...
    /// from './other.js'`) will result in this error.
    #[error("illegal import '{module}' from non-file source")]
    ImportInvalidLocal {
        /// Name of the module being imported
        module: String,
    },

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
        help: Option<String>,
        note: Option<String>,
    },

    /// I/O error while loading source code from a file
    ///
    /// See [`SourceError`].
    #[error(transparent)]
    Source(#[from] SourceError),

    /// AST node type wasn't handled
    ///
    /// This occurs when a node is downcast (using oxc's AST inheritance
    /// framework) but the downcast fails. It indicates a possible branch wasn't
    /// handled. **This is an internal error**.
    #[error("[INTERNAL ERROR] unhandled AST node type: {node}")]
    Unhandled {
        /// Debug representation of the unhandled node
        node: String,
    },

    /// Source code contains a syntax construct that isn't supported in our
    /// semantics
    ///
    /// See [`Unsupported`].
    #[error("{0}")]
    Unsupported(Unsupported),
}

/// Error while loading source code from a file
#[derive(Debug, thiserror::Error)]
#[error("reading source code from {}", self.path.display())]
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
    /// >64-bit int literal
    BigIntLiteral,
    /// `~`, `&`, `|`, etc.
    Bitwise,
    /// `class` and friends
    Class,
    /// `debugger`
    Debugger,
    /// `try`, `catch`, and `throw`
    Exception,
    /// `**`
    ///
    /// Could support this one, I'm just being lazy.
    Exponent,
    /// `export { x } from 'source'`
    ExportFrom,
    /// `export { c }`
    ExportNamedDeclaration,
    /// `const x = function() {}`
    ///
    /// This is different from [`Unsupported::AnonymousFunction`] because
    /// function expressions may have an identifier, but are still illegal
    /// in an expression position.
    FunctionExpression,
    /// `function*`
    Generator,
    /// `import()`
    ImportExpression,
    /// `import { x as "anything" } from "..."`
    ImportStringIdentifier,
    /// `import.meta`
    ImportMeta,
    /// `in`
    In,
    /// `instanceof`
    ///
    /// This could be useful, but I'm omitting for now.
    Instanceof,
    /// `<Component></Component>`
    Jsx,
    /// `'label`
    Label,
    /// `for`, `for ... of`, `for ... in`, `while`, `do ... while`, `break`, and
    /// `continue`
    Loop,
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
    /// Regular expression literal
    RegExp,
    /// `a++, b++` in `let a = 1, b = 2; let result = (a++, b++);`
    Sequence,
    /// `switch`
    Switch,
    /// ```norust
    /// tag`template`
    /// ```
    TaggedTemplate,
    /// `this`
    This,
    /// `using`
    Using,
    /// Wonky V8-specific shit
    V8Intrinstic,
    /// `void`
    Void,
    /// `with`
    With,
}

impl Display for Unsupported {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (name, help) = match self {
            Self::AnonymousFunction => (
                "anonymous `function`",
                Some(
                    "Functions declared with `function` must have a name. \
                    Use `=>` for anonymous functions",
                ),
            ),
            Self::ArrayElision => {
                ("empty slots", Some("sparse arrays are not supported"))
            }
            Self::Async => ("`async`/`await`", None),
            Self::BareImport => {
                ("`import \"...\"`", Some("values must be imported by name"))
            }
            Self::Bitwise => ("bitwise operations", None),
            Self::BigIntLiteral => {
                ("big int literal", Some("integer literal out of range"))
            }
            Self::Class => ("`class`", None),
            Self::Debugger => ("`debugger`", None),
            Self::Exception => (
                "exceptions",
                Some(
                    "exceptions cannot be thrown or caught in PetitScript code",
                ),
            ),
            Self::ExportFrom => (
                "`export ... from ...`",
                Some(
                    "import the value and re-export it in separate statements",
                ),
            ),
            Self::Exponent => ("`**`", None),
            Self::ExportNamedDeclaration => (
                "`export {...}`",
                Some("export declarations directly: `export const c = ...`"),
            ),
            Self::FunctionExpression => (
                "`function()`",
                Some(
                    "`function` keyword not allowed in an expression position; \
                    Use `=>` for function expressions",
                ),
            ),
            Self::Generator => ("generator", None),
            Self::ImportExpression => (
                "`import()`",
                Some("use the ES6-style `import` keyword instead"),
            ),
            Self::ImportStringIdentifier => (
                "`import { x as \"...\"}`",
                Some("import renames must be identifiers"),
            ),
            Self::ImportMeta => ("`import.meta`", None),
            Self::In => ("`in`", Some("Use Object.hasOwn()")),
            Self::Instanceof => {
                ("`instanceof`", Some("use `typeof ... === ...`"))
            }
            Self::Jsx => ("JSX", Some("This isn't React...")),
            Self::Label => ("label", None),
            // TODO link to a page on recursion
            Self::Loop => ("loop", Some("use recursion instead")),
            Self::LooseEquality => {
                ("`==`/`!=`", Some("use `===` or `!==` instead"))
            }
            Self::Mutation => (
                // TODO add link to docs explaining immutability
                "`let`/`var`",
                Some("mutable bindings are not supported, use `const` instead"),
            ),
            Self::NamespaceImport => (
                "`import *`",
                Some(
                    "list specific imports with `import { ... } from 'source'`",
                ),
            ),
            Self::NonUtf => (
                "non-UTF-8 string literal",
                Some("string literals must be valid UTF-8"),
            ),
            Self::ObjectMethod => (
                "object method",
                Some(
                    "use a static field with an arrow function: \
                    `{ f: (...) => {...} }`",
                ),
            ),
            Self::Private => ("private field", None),
            Self::RegExp => ("regular expression", None),
            Self::Sequence => ("sequence", None),
            Self::Switch => ("`switch`", None),
            Self::TaggedTemplate => {
                ("tagged template", Some("use untagged templates"))
            }
            Self::This => ("`this`", None),
            Self::Using => ("`using`", None),
            Self::V8Intrinstic => ("V8 intrinsic", None),
            Self::Void => ("`void`", None),
            Self::With => ("`with`", None),
        };
        let help =
            help.unwrap_or("this feature is not supported in PetitScript");
        write!(f, "unsupported: {name}; {help}")
    }
}

/// A runtime error paired with its stack trace
///
/// Use [`RuntimeError::to_report`] to get a pretty version of the error and
/// trace.
#[derive(Debug)]
pub struct RuntimeError {
    /// What go wrong
    pub error: RuntimeErrorKind,
    /// Trace of function calls leading to the error
    pub trace: StackTrace,
    /// Map of all sources in the program
    ///
    /// TODO make this public?
    pub(crate) source_table: SourceTable,
}

// ===============
// Manual impls are needed here to flatten the error stack
// ===============
impl RuntimeError {
    /// Generate an [`annotate_snippets`] report of this error
    ///
    /// This report can be used to render the error and stack trace prettily.
    /// See [`annotate_snippets::Renderer`].
    ///
    /// ```
    /// # use petitscript::Engine;
    /// let engine = Engine::default();
    /// let process = engine.compile("f()").unwrap();
    /// let error = process.execute().unwrap_err();
    /// let renderer = annotate_snippets::Renderer::styled();
    /// eprintln!("{}", renderer.render(&[error.to_report()]));
    /// ```
    pub fn to_report(&self) -> annotate_snippets::Group<'_> {
        Level::ERROR
            .primary_title(self.error.to_string())
            .element(Level::NOTE.message("most recent call last"))
            .elements(self.trace.iter().map(|frame| {
                if let Some(span) = frame.span {
                    let source = self.source_table.get(span.source_id);
                    Element::from(
                        Snippet::source(source.text())
                            .path(source.name())
                            .annotation(AnnotationKind::Primary.span(
                                (span.start_offset as usize)
                                    ..(span.end_offset as usize),
                            )),
                    )
                } else {
                    Level::NOTE.message("???").into()
                }
            }))
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO include stack trace if f.alt()
        write!(f, "{}", self.error)
    }
}

impl StdError for RuntimeError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        self.error.source()
    }
}

/// The categories of errors that can occuring while running a program
///
/// This is contained by [`RuntimeError`], which carries the error along with
/// its stack trace.
#[derive(Debug, thiserror::Error)]
pub enum RuntimeErrorKind {
    /// Attempted to export the same name twice
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
    Internal(Box<dyn StdError + Send + Sync>),

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
    #[error("Cannot read properties of `{value}` (property \"{property}\")")]
    Property {
        /// Value being targeted (`null` or `undefined`)
        ///
        /// Boxed to reduce variant size.
        value: Box<Value>,
        /// Property being accessed (typically a string)
        ///
        /// Boxed to reduce variant size.
        property: Box<Value>,
    },

    /// Reference to an identifier that isn't bound
    #[error("`{name}` is not defined")]
    Reference { name: String },

    /// Attempt to import a name that a module doesn't export
    #[error(
        "Module does not provide an export with the name `{}`",
        identifier.as_deref().unwrap_or("default")
    )]
    UnknownImport {
        /// Imported identifier, or `None` for `default`
        identifier: Option<String>,
    },

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

impl RuntimeErrorKind {
    /// Wrap a custom error type
    pub fn other(error: impl 'static + StdError + Send + Sync) -> Self {
        Self::Other(error.into())
    }

    /// Create an error that indicates an internal bug in the runtime
    pub(crate) fn internal(
        message: impl Into<Box<dyn StdError + Send + Sync>>,
    ) -> Self {
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

/// A list of source locations representing the function call stack
///
/// This captures the call stack at a particular moment in time, typically when
/// an error occurs. The most recent call is _last_ on this stack.
#[derive(Debug, Default)]
pub struct StackTrace(
    /// Invariant: this list is never empty
    Vec<StackTraceFrame>,
);

impl StackTrace {
    /// Get the last (most recent) frame on the stack
    pub fn last(&self) -> &StackTraceFrame {
        self.0.last().expect("Stack trace cannot be empty")
    }

    /// Get an iterator over the frames of this stack
    ///
    /// The most recent call will be _last_.
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &StackTraceFrame> {
        self.0.iter()
    }
}

// impl Display for StackTrace {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         writeln!(f, "Error (most recent call last)")?;
//         for frame in &self.0 {
//             writeln!(f, "  {frame}")?;
//         }
//         Ok(())
//     }
// }

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
    /// Location from which this function was *invoked*
    ///
    /// `None` if the function was invoked from native code, or if the span
    /// couldn't be located. The latter indicates an internal bug.
    pub span: Option<Span>,
    /// Name of the invoked function
    pub function_name: String,
}

// TODO
// impl Display for StackTraceFrame {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "in {} at {}", self.function_name, self.span)
//     }
// }

/// An error that can occur while converting to/from [`Value`]
#[derive(Debug, thiserror::Error)]
pub enum ValueError {
    /// A custom error message for deserialization
    #[cfg(feature = "serde")]
    #[error("{0}")]
    Custom(String),

    /// Attempted to deserialize into a [`Function`](crate::value::Function)
    /// from a data format other than [`Value`]
    ///
    /// `Function` can only be serialized into/deserialized from `Function`; it
    /// cannot be represented in other serde data formats or transformed
    /// into another data type.
    #[cfg(feature = "serde")]
    #[error(
        "Invalid function serialization/deserialization. PetitScript functions \
        can only be serialized into and deserialized from petitscript::Value. \
        Other data formats are not supported."
    )]
    FunctionSerde,

    /// Attempted to convert non-UTF-8 bytes to a string
    #[error(transparent)]
    InvalidUtf8(#[from] FromUtf8Error),

    /// Error converting [`Number`] to a specific number type. Could be a
    /// mismatch (float -> int or vice versa), or a value out of range
    #[error("Cannot convert number {number} to {expected}: {description}")]
    Number {
        expected: &'static str,
        number: Number,
        description: &'static str,
    },

    /// Wrapper for an external error type
    #[error(transparent)]
    Other(Box<dyn StdError + Send + Sync>),

    /// An operation required a specific type (or types), but received a value
    /// of an unsupported type. This commonly occurs during type downcasting,
    /// e.g. [`Value`] to [`Number`].
    #[error("Type error: expected {expected}, received {actual}")]
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

/// Error while parsing a string to
/// [`Identifier`](crate::ast::Identifier)
#[derive(Debug, thiserror::Error)]
#[error(
    "invalid identifier `{identifier}`; must be non-empty, contain only UTF-8 \
    letters, `0-9`, `_`, or `$`, and not start with a digit"
)]
pub struct IdentifierError {
    pub identifier: String,
}

/// Error while parsing a string to [`NativeModuleName`]
#[derive(Debug, thiserror::Error)]
#[error(
    "invalid module name `{name}`; module names must not be empty and can \
    only contain alphanumeric characters, hyphens (-) or underscores (_)"
)]
pub struct ModuleNameError {
    pub name: String,
}

/// User requested app data of a type that has not been set
///
/// See [`Process::app_data`](crate::Process::app_data).
#[derive(Debug, thiserror::Error)]
#[error("no app data of type `{type_name}` is registered")]
pub struct UnknownAppData {
    pub type_name: &'static str,
}

/// Attempted to register two values of the same type as app data
///
/// See [`Process::app_data`](crate::Process::app_data).
#[derive(Debug, thiserror::Error)]
#[error("multiple app data values of type `{type_name}` were registered")]
pub struct DuplicateAppData {
    pub type_name: &'static str,
}
