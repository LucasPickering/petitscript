//! Names and identifiers. These are building blocks for the AST, but are in a
//! separate file to keep parsing logic from cluttering up the AST definitions

use crate::error::{IdentifierError, ModuleNameError};
use std::{borrow::Borrow, str::FromStr};

/// The name of a variable, function, or property. Identifiers must adhere to
/// these rules:
/// - Allowed characters are unicode letters, `0-9`, `$`, and `_`
/// - Must not start with a digit
///
/// <https://developer.mozilla.org/en-US/docs/Glossary/Identifier>
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
pub struct Identifier(String);

impl Identifier {
    /// Get a reference to the identifier's string value
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for Identifier {
    type Error = IdentifierError;

    fn try_from(identifier: String) -> Result<Self, Self::Error> {
        /// Is a char valid as the first in an identifier?
        fn is_valid_first(c: char) -> bool {
            c.is_alphabetic() || ['$', '_'].contains(&c)
        }

        /// Is a char valid anywhere in an identifier?
        fn is_valid(c: char) -> bool {
            is_valid_first(c) || c.is_ascii_digit()
        }

        // Identifiers are so short that there's no value in providing
        // contextual errors. It's easier to do manual parsing instead of using
        // winnow
        if !identifier.is_empty()
            && identifier.starts_with(is_valid_first)
            && identifier.chars().skip(1).all(is_valid)
        {
            Ok(Self(identifier))
        } else {
            Err(IdentifierError { identifier })
        }
    }
}

impl FromStr for Identifier {
    type Err = IdentifierError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.to_owned().try_into()
    }
}

/// The name of a native module registered with the PS engine. Native module
/// names must adhere to these rules:
/// - Allowed characters are `a-z`, `A-Z`, `0-9`, `_` and `-`
/// - Must start with a letter `a-z` or `A-Z`
/// - Must be at least one character long
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
pub struct NativeModuleName(String);

impl NativeModuleName {
    /// Get a reference to the module name's string value
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

// Enables map lookups with &str
impl Borrow<str> for NativeModuleName {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for NativeModuleName {
    type Error = ModuleNameError;

    fn try_from(name: String) -> Result<Self, Self::Error> {
        fn is_valid(c: char) -> bool {
            c.is_ascii_alphanumeric() || ['-', '_'].contains(&c)
        }

        if !name.is_empty() && name.chars().all(is_valid) {
            Ok(Self(name))
        } else {
            Err(ModuleNameError { name })
        }
    }
}

impl FromStr for NativeModuleName {
    type Err = ModuleNameError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.to_owned().try_into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    // TODO prop tests for this

    /// Test parsing valid identifiers. Parsing doesn't apply transformation, so
    /// the expected value is the same as the input
    #[test_case("_"; "underscore")]
    #[test_case("$"; "dollar")]
    #[test_case("a"; "letter")]
    #[test_case("a1"; "letter_digit")]
    #[test_case("ä"; "unicode")]
    fn parse_identifier(identifier: &str) {
        let parsed = identifier.parse::<Identifier>().unwrap();
        assert_eq!(parsed.as_str(), identifier);
    }

    /// Test parsing invalid identifiers
    #[test_case(""; "empty")]
    #[test_case("1a"; "digit_start")]
    #[test_case("."; "special_char")]
    fn parse_identifier_error(identifier: &str) {
        let result = identifier.parse::<Identifier>();
        assert!(result.is_err(), "Expected {identifier} to be invalid");
    }

    // TODO module name parse errors
}
