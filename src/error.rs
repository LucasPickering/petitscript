use std::io;
use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    Parse(#[from] boa_parser::Error),
    #[error(transparent)]
    Io(#[from] io::Error),
}
