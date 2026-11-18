/// Extension trait for [`Option`]
///
/// TODO move to util module
pub trait OptionExt<T> {
    /// Map an option with a fallible mapper
    fn try_map<O, E>(
        self,
        f: impl FnOnce(T) -> Result<O, E>,
    ) -> Result<Option<O>, E>;

    /// Return the option if it contains a value, otherwise call a fallible
    /// function `f` and return the result
    fn or_else_try<E>(
        self,
        f: impl FnOnce() -> Result<Option<T>, E>,
    ) -> Result<Option<T>, E>;
}

impl<T> OptionExt<T> for Option<T> {
    fn try_map<O, E>(
        self,
        f: impl FnOnce(T) -> Result<O, E>,
    ) -> Result<Option<O>, E> {
        self.map(f).transpose()
    }

    fn or_else_try<E>(
        self,
        f: impl FnOnce() -> Result<Option<T>, E>,
    ) -> Result<Option<T>, E> {
        match self {
            Some(v) => Ok(Some(v)),
            None => f(),
        }
    }
}
