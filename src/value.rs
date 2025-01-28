//! Runtime values

use crate::{runtime::scope::Scope, Error, Result};
use boa_ast::{
    function::{FormalParameterList, FunctionBody},
    StatementListItem,
};
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
            Self::Array(_) => true,
            Self::Object(_) => true,
            Self::Function(_) => true,
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
            Self::Function(_) => ValueType::Function,
        }
    }

    /// TODO
    pub fn get(&self, key: &Self) -> Result<&Value> {
        match (self, key) {
            (Self::Array(array), Self::Number(index)) => todo!(),
            // TODO support number keys here?
            (Self::Object(object), Self::String(key)) => todo!(),
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

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO pretty printing param
        match self {
            Value::Undefined => write!(f, "undefined"),
            Value::Null => write!(f, "null"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Number(number) => write!(f, "{number}"),
            Value::String(string) => write!(f, "{string}"),
            Value::Array(array) => write!(f, "{array}"),
            Value::Object(object) => write!(f, "{object}"),
            Value::Function(function) => write!(f, "{function}",),
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

/// TODO
#[derive(Clone, Debug, Default)]
pub struct Array(Rc<Vec<Value>>);

impl Array {
    /// TODO
    pub fn insert(self, value: Value) -> Self {
        self.with_inner(|vec| vec.push(value))
    }

    /// TODO
    /// TODO better name?
    pub fn insert_all(self, other: Self) -> Self {
        // If we're the sole owner of the other object, we can move the items
        // out. Otherwise we have to clone them over
        match Rc::try_unwrap(other.0) {
            // If this object is empty, and we now own the other one, just point
            // to its buffer and avoid all copies. This optimizes for a common
            // pattern {...obj1, field: "value"}, to avoid repeated allocations
            Ok(other) if self.0.is_empty() => {
                self.with_inner(|vec| *vec = other)
            }
            // We own the other one, so we can move each inner item into our
            // buffer without cloning
            Ok(other) => self.with_inner(|vec| vec.extend(other)),
            // Other object is shared (uncommon case) - we need to clone all its
            // contents
            Err(other) => {
                self.with_inner(|vec| vec.extend(other.iter().cloned()))
            }
        }
    }

    /// TODO
    fn with_inner(mut self, f: impl FnOnce(&mut Vec<Value>)) -> Self {
        // TODO explain
        if let Some(vec) = Rc::get_mut(&mut self.0) {
            f(vec);
            self
        } else {
            let mut vec = self.0.deref().clone();
            f(&mut vec);
            Self(vec.into())
        }
    }
}

impl From<Vec<Value>> for Array {
    fn from(value: Vec<Value>) -> Self {
        Self(value.into())
    }
}

impl Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for (i, element) in self.0.iter().enumerate() {
            if i > 0 {
                // TODO pretty printing
                write!(f, ", ")?;
            }
            write!(f, "{element}")?;
        }
        write!(f, "]")?;
        Ok(())
    }
}

/// TODO
/// TODO disallow duplication - maybe we need our own indexmap?
#[derive(Clone, Debug, Default)]
pub struct Object(Rc<Vec<(String, Value)>>);

impl Object {
    /// TODO
    pub fn insert(self, name: String, value: Value) -> Self {
        self.with_inner(|vec| vec.push((name, value)))
    }

    /// TODO
    /// TODO better name?
    pub fn insert_all(self, other: Self) -> Self {
        // If we're the sole owner of the other object, we can move the items
        // out. Otherwise we have to clone them over
        match Rc::try_unwrap(other.0) {
            // If this object is empty, and we now own the other one, just point
            // to its buffer and avoid all copies. This optimizes for a common
            // pattern {...obj1, field: "value"}, to avoid repeated allocations
            Ok(other) if self.0.is_empty() => {
                self.with_inner(|vec| *vec = other)
            }
            // We own the other one, so we can move each inner item into our
            // buffer without cloning
            Ok(other) => self.with_inner(|vec| vec.extend(other)),
            // Other object is shared (uncommon case) - we need to clone all its
            // contents
            Err(other) => {
                self.with_inner(|vec| vec.extend(other.iter().cloned()))
            }
        }
    }

    /// TODO
    fn with_inner(mut self, f: impl FnOnce(&mut Vec<(String, Value)>)) -> Self {
        // TODO explain
        if let Some(vec) = Rc::get_mut(&mut self.0) {
            f(vec);
            self
        } else {
            let mut vec = self.0.deref().clone();
            f(&mut vec);
            Self(vec.into())
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        for (i, (key, value)) in self.0.iter().enumerate() {
            if i > 0 {
                // TODO pretty printing
                write!(f, ", ")?;
            }
            write!(f, "{key}: {value}")?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

/// TODO
#[derive(Clone, Debug)]
pub struct Function(Rc<FunctionInner>);

impl Function {
    pub fn new(
        name: Option<String>,
        parameters: FormalParameterList,
        body: FunctionBody,
        scope: Scope,
    ) -> Self {
        let inner = FunctionInner {
            name,
            parameters,
            body,
            scope,
        };
        Self(Rc::new(inner))
    }

    /// TODO
    pub fn name(&self) -> Option<&str> {
        self.0.name.as_deref()
    }

    /// TODO
    pub(super) fn scope(&self) -> &Scope {
        &self.0.scope
    }

    /// Get the body's list of executable statements
    pub(super) fn body(&self) -> &[StatementListItem] {
        self.0.body.statements()
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[Function: {}]", self.name().unwrap_or("(anonymous)"))
    }
}

#[derive(Debug)]
struct FunctionInner {
    name: Option<String>,
    parameters: FormalParameterList,
    body: FunctionBody,
    /// Captured variables. This is defined at function definition, and will be
    /// exposed to all calls of the function
    scope: Scope,
}
