//! Runtime values

use crate::Result;
use boa_ast::function::{FormalParameterList, FunctionBody};
use indexmap::IndexMap;
use std::{
    fmt::{self, Display},
    ops::{Add, Deref},
    rc::Rc,
};

/// TODO
/// TODO is clone the best option?
#[derive(Clone, Debug, Default)]
pub struct Value(Rc<ValueKind>);

impl From<ValueKind> for Value {
    fn from(value: ValueKind) -> Self {
        Self(Rc::new(value))
    }
}

impl From<Number> for Value {
    fn from(value: Number) -> Self {
        Self(Rc::new(ValueKind::Number(value)))
    }
}

/// TODO remove this - we should be able to optimize so we don't need it
impl From<&str> for Value {
    fn from(value: &str) -> Self {
        ValueKind::String(value.to_owned()).into()
    }
}

impl Deref for Value {
    type Target = ValueKind;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", *self.0)
    }
}

/// TODO
#[derive(Debug, Default)]
pub enum ValueKind {
    #[default]
    Undefined,
    Null,
    Boolean(bool),
    Number(Number),
    String(String),
    Array(Rc<[Value]>),
    Object(Rc<[(String, Value)]>),
    Function(Function),
}

impl ValueKind {
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
            ValueKind::Undefined => todo!(),
            ValueKind::Null | ValueKind::Boolean(false) => Some(0.into()),
            ValueKind::Boolean(true) => Some(1.into()),
            ValueKind::Number(number) => Some(*number),
            ValueKind::String(_)
            | ValueKind::Array(_)
            | ValueKind::Object(_)
            | ValueKind::Function(_) => None,
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

impl Display for ValueKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueKind::Undefined => write!(f, "undefined"),
            ValueKind::Null => write!(f, "null"),
            ValueKind::Boolean(b) => write!(f, "{b}"),
            ValueKind::Number(number) => write!(f, "{number}"),
            ValueKind::String(s) => write!(f, "{s}"),
            ValueKind::Array(vec) => todo!(),
            // lol
            ValueKind::Object(index_map) => write!(f, "[object Object]"),
            ValueKind::Function(function) => todo!(),
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
        todo!()
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
