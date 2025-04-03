//! Runtime values

mod array;
#[cfg(feature = "bytes")]
mod buffer;
pub mod function;
mod macros;
mod number;
mod object;

pub use array::Array;
#[cfg(feature = "bytes")]
pub use buffer::Buffer;
pub use function::Function;
pub use number::Number;
pub use object::Object;

use crate::{
    error::{RuntimeError, ValueError},
    value::macros::{
        ensure_type, impl_value_conversions, impl_value_from,
        impl_value_numeric_binary_op,
    },
};
use indexmap::IndexMap;
use std::{
    cmp::Ordering,
    fmt::{self, Display},
    ops::{Add, Deref, Div, Mul, Neg, Not, Rem, Sub},
    path::PathBuf,
    sync::Arc,
};

/// TODO
#[derive(Clone, Debug, Default, PartialEq)]
pub enum Value {
    /// TODO
    ///
    /// This value serializes as the unit value: `()`
    #[default]
    Undefined,
    /// TODO
    ///
    /// This value serializes as `None`
    Null,
    /// `true` or `false`
    Boolean(bool),
    /// A float or integer
    Number(Number),
    /// A string of UTF-8 characters
    String(PetitString),
    /// An ordered list
    Array(Array),
    /// An ordered key-value mapping
    Object(Object),
    /// An immutable byte buffer
    #[cfg(feature = "bytes")]
    Buffer(Buffer),
    /// TODO
    Function(Function),
}

#[cfg(test)]
static_assertions::assert_impl_all!(Value: Send, Sync);

impl Value {
    /// Coerce this value to a boolean. Truthy values return true, falsy values
    /// return false.
    pub fn to_bool(&self) -> bool {
        match self {
            Self::Undefined | Self::Null => false,
            Self::Boolean(b) => *b,
            Self::Number(number) => number.to_bool(),
            Self::String(s) => !s.is_empty(),
            Self::Array(_) | Self::Object(_) | Self::Function(_) => true,
            #[cfg(feature = "bytes")]
            Self::Buffer(_) => true,
        }
    }

    /// Coernce this value to a number.
    /// | Type        | Value   | Coercion |
    /// | ----------- | ------- | -------- |
    /// | `undefined` |         | `NaN`    |
    /// | `null`      |         | `0`      |
    /// | `boolean`   | `false` | `0`      |
    /// | `boolean`   | `true`  | `1`      |
    /// | `number`    |         | Itself   |
    /// | `string`    |         | `None`   |
    /// | `array`     |         | `None`   |
    /// | `object`    |         | `None`   |
    /// | `function`  |         | `None`   |
    pub fn to_number(&self) -> Option<Number> {
        match self {
            Self::Undefined => Some(f64::NAN.into()),
            Self::Null | Self::Boolean(false) => Some(0.into()),
            Self::Boolean(true) => Some(1.into()),
            Self::Number(number) => Some(*number),
            Self::String(_)
            | Self::Array(_)
            | Self::Object(_)
            | Self::Function(_) => None,
            #[cfg(feature = "bytes")]
            Self::Buffer(_) => None,
        }
    }

    /// If this value is a string, get the inner string. Otherwise return a type
    /// error.
    pub fn try_into_string(self) -> Result<PetitString, ValueError> {
        if let Self::String(string) = self {
            Ok(string)
        } else {
            Err(ValueError::Type {
                expected: ValueType::Array,
                actual: self.type_(),
            })
        }
    }

    /// If this value is a byte buffer, get the inner bytes. Otherwise return a
    /// type error.
    #[cfg(feature = "bytes")]
    pub fn try_into_buffer(self) -> Result<Buffer, ValueError> {
        if let Self::Buffer(buffer) = self {
            Ok(buffer)
        } else {
            Err(ValueError::Type {
                expected: ValueType::Buffer,
                actual: self.type_(),
            })
        }
    }

    /// If this value is an array, get the inner array. Otherwise return a type
    /// error.
    pub fn try_into_array(self) -> Result<Array, ValueError> {
        if let Self::Array(array) = self {
            Ok(array)
        } else {
            Err(ValueError::Type {
                expected: ValueType::Array,
                actual: self.type_(),
            })
        }
    }

    /// If this value is an object, get the inner object. Otherwise return a
    /// type error.
    pub fn try_into_object(self) -> Result<Object, ValueError> {
        if let Self::Object(object) = self {
            Ok(object)
        } else {
            Err(ValueError::Type {
                expected: ValueType::Object,
                actual: self.type_(),
            })
        }
    }

    /// If this value is a function, get the inner function. Otherwise return a
    /// type error.
    pub fn try_into_function(self) -> Result<Function, ValueError> {
        if let Self::Function(function) = self {
            Ok(function)
        } else {
            Err(ValueError::Type {
                expected: ValueType::Function,
                actual: self.type_(),
            })
        }
    }

    /// Get the type of this value
    pub fn type_(&self) -> ValueType {
        match self {
            Self::Undefined => ValueType::Undefined,
            Self::Null => ValueType::Null,
            Self::Boolean(_) => ValueType::Boolean,
            Self::Number(_) => ValueType::Number,
            Self::String(_) => ValueType::String,
            Self::Array(_) => ValueType::Array,
            Self::Object(_) => ValueType::Object,
            #[cfg(feature = "bytes")]
            Self::Buffer(_) => ValueType::Buffer,
            Self::Function(_) => ValueType::Function,
        }
    }

    /// TODO
    pub fn get(&self, key: &Self) -> Result<Value, RuntimeError> {
        match (self, key) {
            (Self::Array(_), Self::Number(_)) => todo!(),
            // TODO support number keys here?
            (Self::Object(object), Self::String(key)) => {
                Ok(object.get(key).clone())
            }
            (Self::Undefined | Self::Null, _) => todo!(),
            _ => Ok(Value::Undefined),
        }
    }

    /// Boolean AND operator (&&)
    pub fn and(&self, _: &Self) -> bool {
        todo!()
    }

    /// Boolean OR operator (||)
    pub fn or(&self, _: &Self) -> bool {
        todo!()
    }

    /// Apply nullish coalescing (??)
    /// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Nullish_coalescing>
    pub fn coalesce(self, other: Self) -> Self {
        match self {
            Self::Undefined | Self::Null => other,
            _ => self,
        }
    }

    /// Convert this value into an arbitrary type, using the type's [FromPs]
    /// implementation
    pub fn into_todo<T: FromPs>(self) -> Result<T, ValueError> {
        T::from_ps(self)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO pretty printing param
        match self {
            Self::Undefined => write!(f, "undefined"),
            Self::Null => write!(f, "null"),
            Self::Boolean(b) => write!(f, "{b}"),
            Self::Number(number) => write!(f, "{number}"),
            Self::String(string) => write!(f, "{string}"),
            Self::Array(array) => write!(f, "{array}"),
            Self::Object(object) => write!(f, "{object}"),
            #[cfg(feature = "bytes")]
            Self::Buffer(buffer) => write!(f, "{buffer}"),
            Self::Function(function) => write!(f, "{function}"),
        }
    }
}

impl Not for Value {
    type Output = Self;

    /// Apply boolean negation to this value. The value will be cast to a
    /// boolean according to [Self::to_bool], then negated.
    fn not(self) -> Self::Output {
        (!self.to_bool()).into()
    }
}

impl Neg for Value {
    type Output = Self;

    /// Apply arithmetic negation to this value. The value will be cast to a
    /// number according to [Self::to_number], then negated. If the value cannot
    /// be cast to a number, the result is `NaN`.
    fn neg(self) -> Self::Output {
        if let Some(number) = self.to_number() {
            Self::Number(-number)
        } else {
            Self::Number(Number::NAN)
        }
    }
}

impl Add for Value {
    type Output = Self;

    /// TODO link to MDN docs or something on this
    fn add(self, rhs: Self) -> Self::Output {
        // Add is unique from the other mathematical operations. For types that
        // can't be converted to a number, we'll do string concatenation
        match (self, rhs) {
            (Self::Number(n1), Self::Number(n2)) => (n1 + n2).into(),
            _ => todo!(),
        }
    }
}

impl PartialOrd for Value {
    /// Compare two values. This is a best attempt at following the semantics
    /// defined [for the comparison operators in MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Less_than).
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::Undefined, Value::Undefined)
            | (Value::Null, Value::Null) => Some(Ordering::Equal),
            (Value::Boolean(b1), Value::Boolean(b2)) => b1.partial_cmp(b2),
            (Value::Number(n1), Value::Number(n2)) => n1.partial_cmp(n2),
            (Value::String(s1), Value::String(s2)) => s1.partial_cmp(s2),
            _ => match (self.to_number(), other.to_number()) {
                // Try to cast both to numbers
                (Some(n1), Some(n2)) => n1.partial_cmp(&n2),
                // Last resort: cast both to strings
                _ => self.to_string().partial_cmp(&other.to_string()),
            },
        }
    }
}

impl_value_numeric_binary_op!(Sub, sub, -);
impl_value_numeric_binary_op!(Mul, mul, *);
impl_value_numeric_binary_op!(Div, div, /);
impl_value_numeric_binary_op!(Rem, rem, %);

// Two-way conversions: `From<T> for Value` and `FromPs for T``
impl_value_conversions!(bool, Boolean);
impl_value_conversions!(Number, Number);
impl_value_conversions!(i8, Number, to_ps: infallible, from_ps: fallible);
impl_value_conversions!(u8, Number, to_ps: infallible, from_ps: fallible);
impl_value_conversions!(i16, Number, to_ps: infallible, from_ps: fallible);
impl_value_conversions!(u16, Number, to_ps: infallible, from_ps: fallible);
impl_value_conversions!(i32, Number, to_ps: infallible, from_ps: fallible);
impl_value_conversions!(u32, Number, to_ps: infallible, from_ps: fallible);
impl_value_conversions!(i64, Number, to_ps: infallible, from_ps: fallible);
impl_value_conversions!(u64, Number, to_ps: fallible, from_ps: fallible);
impl_value_conversions!(i128, Number, to_ps: fallible, from_ps: fallible);
impl_value_conversions!(u128, Number, to_ps: fallible, from_ps: fallible);
impl_value_conversions!(f32, Number, to_ps: infallible, from_ps: fallible);
impl_value_conversions!(f64, Number, to_ps: infallible, from_ps: fallible);
impl_value_conversions!(String, String);
impl_value_conversions!(Array, Array);
impl_value_conversions!(Object, Object);
impl_value_conversions!(IndexMap<String, Value>, Object);
impl_value_conversions!(Function, Function);

// One-way conversions: `From<T> for Value`
impl_value_from!(&str, String);
impl_value_from!(char, String);
impl_value_from!(Vec<Value>, Array);
impl_value_from!(IndexMap<&str, Value>, Object);

/// Possible types for a value
#[derive(Copy, Clone, Debug)]
pub enum ValueType {
    Undefined,
    Null,
    Boolean,
    Number,
    String,
    Array,
    Object,
    Buffer,
    Function,
}

impl Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Undefined => write!(f, "undefined"),
            Self::Null => write!(f, "null"),
            Self::Boolean => write!(f, "boolean"),
            Self::Number => write!(f, "number"),
            Self::String => write!(f, "string"),
            Self::Array => write!(f, "array"),
            Self::Object => write!(f, "object"),
            Self::Buffer => write!(f, "buffer"),
            Self::Function => write!(f, "function"),
        }
    }
}

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

/// Values exported from a module. This is the output of loading a module.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Exports {
    /// Default exported value
    pub default: Option<Value>,
    /// Named exported values
    pub named: IndexMap<String, Value>,
}

impl Exports {
    /// Export a module as named exports. This allows it to be imported like so:
    ///
    /// ```notrust
    /// import { add } from "module";
    ///
    /// add(1, 2);
    /// ```
    pub fn named<K, V>(exports: impl IntoIterator<Item = (K, V)>) -> Self
    where
        K: ToString,
        V: Into<Value>,
    {
        let named = exports
            .into_iter()
            .map(|(name, value)| (name.to_string(), value.into()))
            .collect();
        Self {
            named,
            default: None,
        }
    }

    /// Export a module as both named exports and a default object of those
    /// names. This allows a module to be used with both named and default
    /// imports, like so:
    ///
    /// ```notrust
    /// import { add } from "module";
    /// import module from "module";
    ///
    /// add(1, 2);
    /// module.add(1, 2);
    /// ```
    pub fn named_and_default<K, V>(
        exports: impl IntoIterator<Item = (K, V)>,
    ) -> Self
    where
        K: ToString,
        V: Into<Value>,
    {
        let mut default = Object::new();
        let mut named = IndexMap::new();
        for (name, value) in exports {
            let name = name.to_string();
            let value: Value = value.into();
            named.insert(name.clone(), value.clone());
            // Since we're the only owner, this will insert without cloning
            default = default.insert(name, value);
        }
        Self {
            named,
            default: Some(default.into()),
        }
    }
}

impl Display for Exports {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(default) = &self.default {
            writeln!(f, "(default): {default}")?;
        }
        for (name, value) in &self.named {
            writeln!(f, "{name}: {value}")?;
        }
        Ok(())
    }
}

/// Trait for converting values into [Value]
pub trait IntoPs {
    fn into_ps(self) -> Result<Value, ValueError>;
}

impl<T: Into<Value>> IntoPs for T {
    fn into_ps(self) -> Result<Value, ValueError> {
        Ok(self.into())
    }
}

impl IntoPs for () {
    fn into_ps(self) -> Result<Value, ValueError> {
        Ok(Value::Undefined)
    }
}

impl IntoPs for PathBuf {
    fn into_ps(self) -> Result<Value, ValueError> {
        // Attempt to convert the path to UTF-8
        let string =
            String::from_utf8(self.into_os_string().into_encoded_bytes())?;
        Ok(string.into())
    }
}

/// Trait for converting values from [Value]
pub trait FromPs: Sized {
    fn from_ps(value: Value) -> Result<Self, ValueError>;
}

impl<T: From<Value>> FromPs for T {
    fn from_ps(value: Value) -> Result<Self, ValueError> {
        Ok(value.into())
    }
}

impl FromPs for PathBuf {
    fn from_ps(value: Value) -> Result<Self, ValueError> {
        let string: String = value.into_todo()?;
        Ok(string.into())
    }
}

impl<T: FromPs> FromPs for Vec<T> {
    fn from_ps(value: Value) -> Result<Self, ValueError> {
        let array = ensure_type!(value, Array);
        array.into_iter().map(T::from_ps).collect()
    }
}
