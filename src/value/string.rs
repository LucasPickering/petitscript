use std::{
    fmt::{self, Display},
    ops::{Add, Deref},
    sync::Arc,
};

/// A reference-counted immutable string
#[derive(Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PetitString(Arc<str>);

impl Deref for PetitString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for PetitString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl From<char> for PetitString {
    fn from(value: char) -> Self {
        Self(value.to_string().into())
    }
}

impl From<&str> for PetitString {
    fn from(value: &str) -> Self {
        Self(value.into())
    }
}

impl From<String> for PetitString {
    fn from(value: String) -> Self {
        Self(value.into())
    }
}

impl From<PetitString> for String {
    fn from(string: PetitString) -> Self {
        // It'd be nice to be able to reuse the allocated string if we own the
        // last copy of the wrapping Arc, but I can't find a way to do that
        // since str is unsized, so we have to clone all the data
        string.0.deref().to_owned()
    }
}

impl Add<PetitString> for PetitString {
    type Output = Self;

    /// Perform string concatenation
    fn add(self, rhs: PetitString) -> Self::Output {
        (String::from(&*self.0) + &rhs).into()
    }
}
