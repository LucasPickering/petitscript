//! Runtime values

use crate::Result;
use boa_ast::function::{FormalParameterList, FunctionBody};
use indexmap::IndexMap;
use std::{ops::Deref, rc::Rc};

/// TODO
/// TODO is clone the best option?
#[derive(Clone, Debug)]
pub struct Value(Rc<ValueKind>);

impl From<ValueKind> for Value {
    fn from(value: ValueKind) -> Self {
        Self(Rc::new(value))
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

/// TODO
#[derive(Debug)]
pub enum ValueKind {
    Null,
    Undefined,
    Boolean(bool),
    Number(Number),
    String(String),
    Array(Vec<Value>),
    Object(IndexMap<String, Value>),
    Function(Function),
}

impl ValueKind {
    /// TODO
    pub fn to_bool(&self) -> bool {
        match self {
            Self::Null | Self::Undefined => false,
            Self::Boolean(b) => *b,
            Self::Number(number) => number.to_bool(),
            Self::String(s) => !s.is_empty(),
            Self::Array(_) => true,
            Self::Object(_) => true,
            Self::Function(_) => true,
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

/// TODO
#[derive(Debug)]
pub struct Function {
    pub name: Option<String>,
    pub parameters: FormalParameterList,
    pub body: FunctionBody,
}
