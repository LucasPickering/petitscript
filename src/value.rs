//! Runtime values

use crate::{runtime::scope::Scope, Result};
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
    String(Rc<str>),
    Array(Array),
    Object(Object),
    Function(Function),
}

impl Value {
    /// TODO
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

    /// TODO
    pub fn to_number(&self) -> Option<Number> {
        match self {
            Value::Undefined => todo!(),
            Value::Null | Value::Boolean(false) => Some(0.into()),
            Value::Boolean(true) => Some(1.into()),
            Value::Number(number) => Some(*number),
            Value::String(_)
            | Value::Array(_)
            | Value::Object(_)
            | Value::Function(_) => None,
        }
    }

    /// TODO
    pub fn try_into_array(self) -> Result<Array> {
        if let Self::Array(array) = self {
            Ok(array)
        } else {
            todo!("error")
        }
    }

    /// TODO
    pub fn try_into_object(self) -> Result<Object> {
        if let Self::Object(object) = self {
            Ok(object)
        } else {
            todo!("error")
        }
    }

    /// TODO
    pub fn try_into_function(self) -> Result<Function> {
        if let Self::Function(function) = self {
            Ok(function)
        } else {
            todo!("error")
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
        match self {
            Value::Undefined => write!(f, "undefined"),
            Value::Null => write!(f, "null"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Number(number) => write!(f, "{number}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Array(vec) => write!(f, "[todo]"),
            Value::Object(map) => write!(f, "{{todo}}"),
            Value::Function(function) => write!(
                f,
                "[Function: {}]",
                function.name().unwrap_or("(anonymous)")
            ),
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

#[derive(Debug)]
struct FunctionInner {
    name: Option<String>,
    parameters: FormalParameterList,
    body: FunctionBody,
    /// Captured variables. This is defined at function definition, and will be
    /// exposed to all calls of the function
    scope: Scope,
}
