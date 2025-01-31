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
    #[error("TODO")]
    Parse(),

    /// Reference to an identifier that isn't bound
    #[error("{name} is not defined")]
    Reference { name: String },

    #[error("Type error: expected {expected}, received {actual}")]
    Type {
        expected: ValueType,
        actual: ValueType,
    },
}
