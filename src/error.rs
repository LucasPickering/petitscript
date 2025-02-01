use crate::value::ValueType;
use chumsky::error::Rich;
use std::{
    fmt::{self, Display},
    io,
};
use thiserror::Error;

/// TODO
#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    Load(#[from] LoadError),
    #[error(transparent)]
    Runtime(#[from] RuntimeError),
}

/// An error that occurred while loading/parsing source code
#[derive(Debug, Error)]
pub enum LoadError {
    /// TODO
    #[error(transparent)]
    Io(#[from] io::Error),

    /// TODO
    Parse {
        source_name: Option<String>,
        errors: Vec<Rich<'static, char>>,
    },
}

impl Display for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(_) => todo!(),
            Self::Parse {
                source_name,
                errors,
            } => {
                let name = source_name.as_deref().unwrap_or("source code");
                writeln!(f, "Error parsing {name}:")?;
                for error in errors {
                    writeln!(f, "  {error}")?;
                }
                Ok(())
            }
        }
    }
}

pub(crate) type RuntimeResult<T> = std::result::Result<T, RuntimeError>;

/// An error that occurred while executing a script or module
#[derive(Debug, Error)]
pub enum RuntimeError {
    /// TODO
    #[error("Name {name} is already exported")]
    AlreadyExported { name: String },

    /// Attempted to return while not in a function
    #[error("Cannot return while not in function")]
    IllegalReturn,

    /// Second assignment to a `const` variable
    #[error("Assignment to immutable variable {name}")]
    ImmutableAssign { name: String },

    /// Reference to an identifier that isn't bound
    #[error("{name} is not defined")]
    Reference { name: String },

    #[error("Type error: expected {expected}, received {actual}")]
    Type {
        expected: ValueType,
        actual: ValueType,
    },
}
