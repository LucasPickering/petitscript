use std::io;
use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    Parse(#[from] boa_parser::Error),
    #[error(transparent)]
    Io(#[from] io::Error),

    /// Attempted to return while not in a function
    #[error("Cannot return while not in function")]
    IllegalReturn,

    /// Second assignment to a `const` variable
    #[error("Assignment to immutable variable {name}")]
    ImmutableAssign { name: String },

    /// Reference to an identifier that isn't bound
    #[error("{name} is not defined")]
    Reference { name: String },

    /// User tried to use an ES6 feature that we don't support
    #[error("Operation not supported: {name}; {help}")]
    Unsupported {
        name: &'static str,
        help: &'static str,
    },
}
