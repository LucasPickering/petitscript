/// Extension trait for [`Option`]
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

/// Extension trait for [`Result`]
#[cfg(test)]
pub trait ResultExt<T, E> {
    /// Return the value if `Ok`, or panic if `Err`
    ///
    /// Uses the error's `Display` impl instead of `Debug` for printing.
    fn unwrap_display(self) -> T
    where
        E: std::fmt::Display;
}

#[cfg(test)]
impl<T, E> ResultExt<T, E> for Result<T, E> {
    fn unwrap_display(self) -> T
    where
        E: std::fmt::Display,
    {
        match self {
            Ok(value) => value,
            Err(error) => panic!("{error:#}"),
        }
    }
}
