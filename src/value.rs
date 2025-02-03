//! Runtime values

mod array;
mod function;
mod number;
mod object;

pub use array::Array;
pub use function::{Function, NativeFunction};
pub use number::Number;
pub use object::Object;

use crate::{error::RuntimeResult, RuntimeError};
use std::{
    collections::HashMap,
    fmt::{self, Display},
    ops::{Add, Deref, Div, Mul, Rem, Sub},
    sync::Arc,
};

/// TODO
#[derive(Clone, Debug, Default)]
pub enum Value {
    #[default]
    Undefined,
    Null,
    Boolean(bool),
    Number(Number),
    String(JsString),
    Array(Array),
    Object(Object),
    Function(Function),
    Native(NativeFunction),
}

impl Value {
    /// Coerce this value to a boolean. Truthy values return true, falsy values
    /// return false.
    pub fn to_bool(&self) -> bool {
        match self {
            Self::Undefined | Self::Null => false,
            Self::Boolean(b) => *b,
            Self::Number(number) => number.to_bool(),
            Self::String(s) => !s.is_empty(),
            Self::Array(_)
            | Self::Object(_)
            | Self::Function(_)
            | Self::Native(_) => true,
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
            | Self::Function(_)
            | Self::Native(_) => None,
        }
    }

    /// If this value is an array, get the inner array. Otherwise return a type
    /// error.
    pub fn try_into_array(self) -> RuntimeResult<Array> {
        if let Self::Array(array) = self {
            Ok(array)
        } else {
            Err(RuntimeError::Type {
                expected: ValueType::Array,
                actual: self.type_(),
            })
        }
    }

    /// If this value is an object, get the inner object. Otherwise return a
    /// type error.
    pub fn try_into_object(self) -> RuntimeResult<Object> {
        if let Self::Object(object) = self {
            Ok(object)
        } else {
            Err(RuntimeError::Type {
                expected: ValueType::Object,
                actual: self.type_(),
            })
        }
    }

    /// If this value is a function, get the inner function. Otherwise return a
    /// type error.
    pub fn try_into_function(self) -> RuntimeResult<Function> {
        if let Self::Function(function) = self {
            Ok(function)
        } else {
            Err(RuntimeError::Type {
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
            Self::Function(_) | Self::Native(_) => ValueType::Function,
        }
    }

    /// TODO
    pub fn get(&self, key: &Self) -> RuntimeResult<Value> {
        match (self, key) {
            (Self::Array(_), Self::Number(_)) => todo!(),
            // TODO support number keys here?
            (Self::Object(object), Self::String(key)) => Ok(object.get(key)),
            _ => todo!("error"),
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
    /// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Nullish_coalescing
    pub fn coalesce(self, other: Self) -> Self {
        match self {
            Self::Undefined | Self::Null => other,
            _ => self,
        }
    }

    /// TODO better name?
    #[cfg(feature = "serde")]
    pub fn deserialize<T: serde::de::DeserializeOwned>(
        &self,
    ) -> RuntimeResult<T> {
        todo!()
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
            Self::Function(function) => write!(f, "{function}"),
            Self::Native(function) => write!(f, "{function}"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Array(l0), Self::Array(r0)) => l0 == r0,
            (Self::Object(l0), Self::Object(r0)) => l0 == r0,
            // Functions are never equal
            (Self::Function(_), Self::Function(_))
            | (Self::Native(_), Self::Native(_)) => false,
            _ => false,
        }
    }
}

impl Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        // Add is unique from the other mathematical operations. For types that
        // can't be converted to a number, we'll do string concatenation
        match (self, rhs) {
            (Self::Number(n1), Self::Number(n2)) => (n1 + n2).into(),
            _ => todo!(),
        }
    }
}

/// Implement a numeric binary operator for [Value]
macro_rules! impl_numeric_binary_op {
    ($trait:ident, $func:ident, $op:tt) => {
        impl $trait for Value {
            type Output = Self;

            fn $func(self, rhs: Self) -> Self::Output {
                match (self.to_number(), rhs.to_number()) {
                    (Some(lhs), Some(rhs)) => (lhs $op rhs).into(),
                    _ => Number::NAN.into(),
                }
            }
        }
    };
}

impl_numeric_binary_op!(Sub, sub, -);
impl_numeric_binary_op!(Mul, mul, *);
impl_numeric_binary_op!(Div, div, /);
impl_numeric_binary_op!(Rem, rem, %);

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Self::Boolean(b)
    }
}

impl From<Number> for Value {
    fn from(value: Number) -> Self {
        Self::Number(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Self::String(value.into())
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::String(value.into())
    }
}

impl From<Array> for Value {
    fn from(array: Array) -> Self {
        Self::Array(array)
    }
}

impl From<Object> for Value {
    fn from(object: Object) -> Self {
        Self::Object(object)
    }
}

impl From<Function> for Value {
    fn from(function: Function) -> Self {
        Self::Function(function)
    }
}

impl From<NativeFunction> for Value {
    fn from(function: NativeFunction) -> Self {
        Self::Native(function)
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for Value {
    fn serialize<S>(&self, _: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        todo!()
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for Value {
    fn deserialize<D>(_: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        todo!()
    }
}

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
            Self::Function => write!(f, "function"),
        }
    }
}

/// A reference-counted immutable string
#[derive(Clone, Debug, Default, PartialEq)]
pub struct JsString(Arc<str>);

impl Deref for JsString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for JsString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<&str> for JsString {
    fn from(value: &str) -> Self {
        Self(value.into())
    }
}

impl From<String> for JsString {
    fn from(value: String) -> Self {
        Self(value.into())
    }
}

/// Values exported from a module. This is the output of loading a module.
#[derive(Debug, Default)]
pub struct Exports {
    /// Default exported value
    pub default: Option<Value>,
    /// Named exported values
    pub named: HashMap<String, Value>,
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
pub trait IntoJs {
    fn into_js(self) -> RuntimeResult<Value>;
}

impl<T: Into<Value>> IntoJs for T {
    fn into_js(self) -> RuntimeResult<Value> {
        Ok(self.into())
    }
}

/// Trait for converting values from [Value]
pub trait FromJs: Sized {
    fn from_js(value: Value) -> RuntimeResult<Self>;
}

impl<T: From<Value>> FromJs for T {
    fn from_js(value: Value) -> RuntimeResult<Self> {
        Ok(value.into())
    }
}

/// TODO
pub trait FromJsArguments: Sized {
    fn from_js_arguments(values: Vec<Value>) -> RuntimeResult<Self>;
}

impl FromJsArguments for Vec<Value> {
    fn from_js_arguments(values: Vec<Value>) -> RuntimeResult<Self> {
        Ok(values)
    }
}
