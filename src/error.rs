use crate::value::ValueType;
use std::io;
use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error)]
pub enum Error {
    /// TODO
    #[error("Name {name} is already exported")]
    AlreadyExported { name: String },

    /// Attempted to return while not in a function
    #[error("Cannot return while not in function")]
    IllegalReturn,

    /// Second assignment to a `const` variable
    #[error("Assignment to immutable variable {name}")]
    ImmutableAssign { name: String },

    /// TODO
    #[error(transparent)]
    Io(#[from] io::Error),

    /// TODO
    #[error(transparent)]
    Parse(#[from] boa_parser::Error),

    /// Reference to an identifier that isn't bound
    #[error("{name} is not defined")]
    Reference { name: String },

    #[error("Type error: expected {expected}, received {actual}")]
    Type {
        expected: ValueType,
        actual: ValueType,
    },

    /// User tried to use an ES6 feature that we don't support
    #[error("Operation not supported: {name}; {help}")]
    Unsupported {
        name: &'static str,
        help: &'static str,
    },
}

impl Error {
    /// Helper for generating an [Unsupported](Error::Unsupported) error,
    /// wrapped in a `Result` for Added Convenience
    pub(crate) fn unsupported<T>(
        name: &'static str,
        help: &'static str,
    ) -> Result<T> {
        Err(Self::Unsupported { name, help })
    }
}
