//! Runtime values

mod array;
mod function;
mod object;

pub use array::Array;
pub use function::{Function, NativeFunction, NativeFunctionTrait};
pub use object::Object;

use crate::{Error, Result};
use std::{
    fmt::{self, Display},
    ops::{Add, Deref},
    rc::Rc,
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
    pub fn try_into_array(self) -> Result<Array> {
        if let Self::Array(array) = self {
            Ok(array)
        } else {
            Err(Error::Type {
                expected: ValueType::Array,
                actual: self.type_(),
            })
        }
    }

    /// If this value is an object, get the inner object. Otherwise return a
    /// type error.
    pub fn try_into_object(self) -> Result<Object> {
        if let Self::Object(object) = self {
            Ok(object)
        } else {
            Err(Error::Type {
                expected: ValueType::Object,
                actual: self.type_(),
            })
        }
    }

    /// If this value is a function, get the inner function. Otherwise return a
    /// type error.
    pub fn try_into_function(self) -> Result<Function> {
        if let Self::Function(function) = self {
            Ok(function)
        } else {
            Err(Error::Type {
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
    pub fn get(&self, key: &Self) -> Result<Value> {
        match (self, key) {
            (Self::Array(array), Self::Number(index)) => todo!(),
            // TODO support number keys here?
            (Self::Object(object), Self::String(key)) => Ok(object.get(key)),
            _ => todo!("error"),
        }
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

impl<F: NativeFunctionTrait> From<F> for Value {
    fn from(function: F) -> Self {
        Self::Native(function.into())
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

/// TODO
#[derive(Copy, Clone, Debug)]
pub enum Number {
    Int(i64),
    Float(f64),
}

impl Number {
    /// TODO
    pub fn to_bool(self) -> bool {
        match self {
            Number::Int(i) => i != 0,
            Number::Float(f) => f == 0.0,
        }
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Number::Int(i) => write!(f, "{i}"),
            Number::Float(n) => write!(f, "{n}"),
        }
    }
}

impl From<i64> for Number {
    fn from(value: i64) -> Self {
        Self::Int(value)
    }
}

impl From<f64> for Number {
    fn from(value: f64) -> Self {
        Self::Float(value)
    }
}

impl Add for Number {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(lhs), Number::Int(rhs)) => {
                lhs.wrapping_add(rhs).into()
            }
            // TODO handle overflow or w/e
            (Number::Int(lhs), Number::Float(rhs)) => {
                ((lhs as f64) + rhs).into()
            }
            (Number::Float(lhs), Number::Int(rhs)) => {
                (lhs + (rhs as f64)).into()
            }
            (Number::Float(lhs), Number::Float(rhs)) => (lhs + rhs).into(),
        }
    }
}

/// A reference-counted immutable string
#[derive(Clone, Debug, Default)]
pub struct JsString(Rc<str>);

impl Deref for JsString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for JsString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.0)
    }
}

impl From<&str> for JsString {
    fn from(value: &str) -> Self {
        Self(value.into())
    }
}
