//! Runtime values

use crate::Result;
use boa_ast::function::{FormalParameterList, FunctionBody};
use std::{
    fmt::{self, Display},
    ops::Add,
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
    Array(Rc<[Value]>),
    Object(Rc<[(String, Value)]>),
    Function(Rc<Function>),
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
        Value::Number(value)
    }
}

/// TODO remove this - we should be able to optimize so we don't need it
impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::String(value.into())
    }
}

impl From<Function> for Value {
    fn from(value: Function) -> Self {
        Value::Function(value.into())
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
                "[Function: {}",
                function.name.as_deref().unwrap_or("(anonymous)")
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
#[derive(Debug)]
pub struct Function {
    pub name: Option<String>,
    pub parameters: FormalParameterList,
    pub body: FunctionBody,
}
