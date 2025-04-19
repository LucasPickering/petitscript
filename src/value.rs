//! Runtime values

mod array;
#[cfg(feature = "buffer")]
mod buffer;
pub mod function;
mod macros;
mod number;
mod object;
mod string;

pub use array::Array;
#[cfg(feature = "buffer")]
pub use buffer::Buffer;
pub use function::Function;
pub use number::Number;
pub use object::Object;
pub use string::PetitString;

use crate::{
    error::{RuntimeError, ValueError},
    json,
    scope::Scope,
    value::macros::{
        ensure_type, impl_value_conversions, impl_value_from,
        impl_value_numeric_binary_op,
    },
};
use indexmap::IndexMap;
use std::{
    cmp::Ordering,
    fmt::{self, Display},
    ops::{Add, Div, Mul, Neg, Not, Rem, Sub},
    path::PathBuf,
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
    #[cfg(feature = "buffer")]
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
            #[cfg(feature = "buffer")]
            Self::Buffer(_) => true,
        }
    }

    /// Coernce this value to a number.
    /// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number#number_coercion
    pub fn to_number(&self) -> Option<Number> {
        match self {
            Self::Undefined => Some(Number::NAN),
            Self::Null => Some(0.into()),
            Self::Boolean(false) => Some(0.into()),
            Self::Boolean(true) => Some(1.into()),
            Self::Number(number) => Some(*number),
            // TODO try parsing string as number
            Self::String(_)
            | Self::Array(_)
            | Self::Object(_)
            | Self::Function(_) => None,
            #[cfg(feature = "buffer")]
            Self::Buffer(_) => None,
        }
    }

    /// Coerce this value to a string. This intentionally shadows
    /// [ToString::to_string](std::fmt::ToString::to_string) to prevent
    /// accidentally using display rules for coercion. The [Display] impl is
    /// intended just for debug printing in Rust. This coercion implementation
    /// aims to match JS's semantics for string coercion. If you specifically
    /// want the display implementation, use `format!({string})` instead.
    ///
    /// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String
    pub fn to_string(&self) -> PetitString {
        match self {
            Self::Undefined => "undefined".into(),
            Self::Null => "null".into(),
            Self::Boolean(false) => "false".into(),
            Self::Boolean(true) => "true".into(),
            Self::Number(number) => format!("{number}").into(),
            Self::String(string) => string.clone(),
            Self::Array(array) => {
                let mut buf = String::new();
                for (i, element) in array.iter().enumerate() {
                    if i > 0 {
                        buf.push(',');
                    }
                    buf.push_str(&element.to_string());
                }
                buf.into()
            }
            // lol. This is stupid but it matches JS
            Self::Object(_) => "[Object object]".into(),
            #[cfg(feature = "buffer")]
            Self::Buffer(_) => "TODO".into(),
            Self::Function(_) => todo!(),
        }
    }

    /// Generate a JSON string representing this value
    pub fn to_json(&self) -> String {
        let mut s = String::new();
        json::write_json(&mut s, self);
        s
    }

    /// If this value is a string, get the inner string. Otherwise return a type
    /// error. This will not perform any type coercion.
    pub fn try_into_string(self) -> Result<PetitString, ValueError> {
        if let Self::String(string) = self {
            Ok(string)
        } else {
            Err(ValueError::Type {
                expected: ValueType::String,
                actual: self.type_(),
            })
        }
    }

    /// If this value is a string, return a reference to the inner `str`. If
    /// not, return `None`
    pub fn as_str(&self) -> Option<&str> {
        if let Self::String(string) = self {
            Some(string)
        } else {
            None
        }
    }

    /// If this value is a byte buffer, get the inner bytes. Otherwise return a
    /// type error.
    #[cfg(feature = "buffer")]
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
            #[cfg(feature = "buffer")]
            Self::Buffer(_) => ValueType::Buffer,
            Self::Function(_) => ValueType::Function,
        }
    }

    /// Get a property from this value. If the property is not present on the
    /// value, check its prototype as well. Return `undefined` if the property
    /// isn't present. If this value is `null` or `undefined`, return an error.
    pub(crate) fn get(
        &self,
        key: &Self,
        scope: &Scope,
    ) -> Result<Value, RuntimeError> {
        let value: Option<Value> = match (self, key) {
            (Self::Array(array), Self::Number(Number::Int(i))) => {
                // If the index is positive and in the array, get the value
                if let Ok(i) = usize::try_from(*i) {
                    array.get(i).cloned()
                } else {
                    None
                }
            }
            // TODO support number keys here?
            (Self::Object(object), Self::String(key)) => {
                Some(object.get(key).clone())
            }
            (Self::Undefined | Self::Null, _) => todo!("error"),
            _ => None,
        };

        Ok(value
            // If it wasn't directly on the value, check the prototype
            .or_else(|| {
                // If the key isn't a string, we know it won't be in the
                // prototype so we can bail out
                let name = key.as_str()?;
                let definition_id = scope.get_prototype(self.type_(), name)?;
                Some(
                    Function::bound(definition_id, self.clone(), name.into())
                        .into(),
                )
            })
            .unwrap_or_default())
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
            #[cfg(feature = "buffer")]
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

    /// Add two values together. The semantics on this replicate those of JS
    /// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Addition
    fn add(self, rhs: Self) -> Self::Output {
        // Add is unique from the other mathematical operations. If either side
        // is a string, do string concat.
        match (self, rhs) {
            // If either side is a string, do string concatenation
            // TODO to_string() isn't the right string coercion. It doesn't
            // handle 1-element arrays correctly
            (Self::String(s), other) => (s + other.to_string()).into(),
            (other, Self::String(s)) => (other.to_string() + s).into(),
            // Attempt to convert both to numbers and add them. Otherwise fall
            // back to string concat again
            (a, b) => match (a.to_number(), b.to_number()) {
                (Some(a), Some(b)) => (a + b).into(),
                _ => (a.to_string() + b.to_string()).into(),
            },
        }
    }
}

impl Add<PetitString> for Value {
    type Output = Self;

    /// Perform string concatenation
    fn add(self, rhs: PetitString) -> Self::Output {
        (self.to_string() + rhs).into()
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
impl_value_conversions!(PetitString, String);
impl_value_conversions!(Array, Array);
impl_value_conversions!(Object, Object);
impl_value_conversions!(IndexMap<String, Value>, Object);
impl_value_conversions!(Function, Function);

// One-way conversions: `From<T> for Value`
impl_value_from!(&str, String);
impl_value_from!(char, String);

// These generic conversions can't be done easily with macros
impl<const N: usize, T: Into<Value>> From<[T; N]> for Value {
    fn from(value: [T; N]) -> Self {
        Value::Array(value.into())
    }
}

impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(value: Vec<T>) -> Self {
        Value::Array(value.into())
    }
}

impl<const N: usize, T: Into<Value>> From<[(&str, T); N]> for Value {
    fn from(value: [(&str, T); N]) -> Self {
        Self::Object(value.into())
    }
}

impl<T: Into<Value>> From<IndexMap<&str, T>> for Value {
    fn from(map: IndexMap<&str, T>) -> Self {
        Self::Object(map.into())
    }
}

/// Possible types for a value
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum ValueType {
    Undefined,
    Null,
    Boolean,
    Number,
    String,
    Array,
    Object,
    Function,
    #[cfg(feature = "buffer")]
    Buffer,
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
            Self::Function => write!(f, "function"),
            #[cfg(feature = "buffer")]
            Self::Buffer => write!(f, "buffer"),
        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value::{Null, Undefined};
    use test_case::test_case;

    /// Test boolean coercion
    #[test_case(Undefined, false; "undefined")]
    #[test_case(Null, false; "null")]
    #[test_case(false, false; "bool_false")]
    #[test_case(true, true; "bool_true")]
    #[test_case(0, false; "int_false")]
    #[test_case(-3, true; "int_true")]
    #[test_case(0.0, false; "float_false")]
    #[test_case(-3.0, true; "float_true")]
    // Number has more extensive test cases for this
    #[test_case("", false; "string_empty")]
    #[test_case("test", true; "string")]
    #[test_case(Array::default(), true; "array_empty")]
    #[test_case([1,2], true; "array")]
    #[test_case(Object::default(), true; "object_empty")]
    #[test_case([("a", 1)], true; "object")]
    #[test_case(Buffer::default(), true; "buffer_empty")]
    #[test_case(Buffer::from([1, 2]), true; "buffer")]
    // TODO test function
    fn to_bool(value: impl Into<Value>, expected: bool) {
        assert_eq!(value.into().to_bool(), expected);
    }

    /// Test number coercion
    #[test_case(Undefined, Some(Number::NAN); "undefined")]
    #[test_case(Null, Some(0); "null")]
    #[test_case(false, Some(0); "bool_false")]
    #[test_case(true, Some(1); "bool_true")]
    #[test_case(-3, Some(-3); "int")]
    #[test_case(17.8, Some(17.8); "float")]
    #[test_case("3", Some(3); "string_int_positive")]
    #[test_case("-3", Some(-3); "string_int_negative")]
    #[test_case("-3.5", Some(-3.5); "string_float")]
    #[test_case("test", None::<Number>; "string")]
    #[test_case(Array::default(), None::<Number>; "array_empty")]
    #[test_case([1], None::<Number>; "array_1")]
    #[test_case([1, 2], None::<Number>; "array_2")]
    #[test_case(Object::default(), None::<Number>; "object_empty")]
    #[test_case([("a", 1)], None::<Number>; "object")]
    #[test_case(Buffer::default(), None::<Number>; "buffer_empty")]
    #[test_case(Buffer::from([1, 2]), None::<Number>; "buffer")]
    // TODO test function
    fn to_number(value: impl Into<Value>, expected: Option<impl Into<Number>>) {
        assert_eq!(value.into().to_number(), expected.map(|n| n.into()));
    }

    /// Test string coercion (NOT string displaying)
    #[test_case(Undefined, "undefined"; "undefined")]
    #[test_case(Null, "null"; "null")]
    #[test_case(false, "false"; "bool_false")]
    #[test_case(true, "true"; "bool_true")]
    #[test_case(-3, "-3"; "int")]
    #[test_case(17.8, "17.8"; "float")]
    #[test_case("test", "test"; "string")]
    #[test_case([1, 2], "1,2"; "array")]
    #[test_case([("a", 1)], "[Object object]"; "object")]
    #[test_case(Buffer::from([1, 2]), "TODO"; "buffer")]
    // TODO test function
    fn to_string(value: impl Into<Value>, expected: &str) {
        assert_eq!(&*value.into().to_string(), expected);
    }

    /// Test value equality
    #[test_case(Undefined, Undefined; "undefined")]
    #[test_case(Null, Null; "null")]
    #[test_case(false, false; "bool_false")]
    #[test_case(true, true; "bool_true")]
    #[test_case(3, 3; "int")]
    #[test_case(3.5, 3.5; "float")]
    #[test_case(3, 3.0; "int float")]
    #[test_case("", ""; "string_empty")]
    #[test_case("hello", "hello"; "string")]
    #[test_case([1, 2], [1, 2]; "array")]
    #[test_case([("a", 1)], [("a", 1)]; "object")]
    #[test_case(Buffer::from([1u8, 2u8]), [1u8, 2u8].as_slice(); "buffer")]
    // TODO test functions
    fn equal(a: impl Into<Value>, b: impl Into<Value>) {
        assert_eq!(a.into(), b.into());
    }

    /// Test value inequality
    #[test_case(Undefined, Null; "undefined_null")]
    #[test_case(Null, "null"; "null_string")]
    #[test_case(true, "true"; "bool_string")]
    #[test_case(Number::NAN, Number::NAN; "nan")]
    #[test_case("hello", "HELLO"; "string")]
    #[test_case(1, "1"; "number string")]
    #[test_case([1], [1, 2]; "array")]
    #[test_case([("a", 1)], [("a", 2)]; "object")]
    #[test_case(Buffer::from([1, 2]), Buffer::from([1, 3]); "buffer")]
    // TODO test functions
    fn unequal(a: impl Into<Value>, b: impl Into<Value>) {
        assert_ne!(a.into(), b.into());
    }

    /// Test addition operations that return a number. These should all be
    /// commutative (`a+ b == b + a`).
    #[test_case(Undefined, Undefined, Number::NAN; "undefined undefined")]
    #[test_case(Null, Null, 0; "null null")]
    #[test_case(Undefined, Null, Number::NAN; "undefined null")]
    #[test_case(Undefined, true, Number::NAN; "undefined bool")]
    #[test_case(Undefined, 3, Number::NAN; "undefined int")]
    #[test_case(Undefined, 3.5, Number::NAN; "undefined float")]
    #[test_case(false, true, 1; "bool bool")]
    #[test_case(false, 6.5, 7.5; "bool float")]
    #[test_case(false, 6, 7; "bool int")]
    #[test_case(-6, 13, 7; "int int")]
    #[test_case(-5, 12.5, 7.5; "int float")]
    #[test_case(3.2, 170.6, 173.8; "float float")]
    #[test_case(3, Number::NAN, Number::NAN; "nan")]
    fn add_numeric(
        a: impl Into<Value>,
        b: impl Into<Value>,
        expected: impl Into<Number>,
    ) {
        let a = a.into();
        let b = b.into();
        let expected = Value::from(expected.into());
        assert_eq!(a.clone() + b.clone(), expected);
        assert_eq!(b + a, expected);
    }

    /// Test addition operations that return a string. Since string
    /// concatenation is not commutative, this takes two expected values:
    /// `a + b` and `b + a`
    #[test_case(Undefined, "s", "undefineds", "sundefined"; "undefined string")]
    #[test_case(Null, "s", "nulls", "snull"; "null string")]
    #[test_case(true, "s", "trues", "strue"; "boolean string")]
    #[test_case(3, "1", "31", "13"; "number string")]
    #[test_case("s1", "s2", "s1s2", "s2s1"; "string string")]
    #[test_case([1], [1, 2], "11,2", "1,21"; "array array")]
    #[test_case([1], "s", "1s", "s1"; "array string")]
    #[test_case([1, 2, 3], "s", "1,2,3s", "s1,2,3"; "long_array string")]
    #[test_case(
        [("a", 1), ("b", 2)], "s", "[Object object]s", "s[Object object]";
        "object string"
    )]
    #[test_case(Buffer::from([1, 2]), "s", "TODOs", "sTODO"; "buffer string")]
    // TODO test functions
    fn add_string(
        a: impl Into<Value>,
        b: impl Into<Value>,
        expected1: &str,
        expected2: &str,
    ) {
        let a = a.into();
        let b = b.into();
        assert_eq!(a.clone() + b.clone(), expected1.into());
        assert_eq!(b + a, expected2.into());
    }
}
