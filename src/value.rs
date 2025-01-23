//! Runtime values

use crate::Result;
use boa_ast::function::{FormalParameterList, FunctionBody};
use indexmap::IndexMap;

/// TODO
#[derive(Debug)]
pub enum Value {
    Null,
    Undefined,
    Boolean(bool),
    Number(Number),
    String(String),
    Array(Vec<Self>),
    Object(IndexMap<String, Self>),
    Function(Function),
}

impl Value {
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
    pub fn get(&self, key: &Self) -> Result<&Self> {
        match (self, key) {
            (Self::Array(array), Self::Number(index)) => todo!(),
            // TODO support number keys here?
            (Self::Object(object), Self::String(key)) => todo!(),
            _ => todo!("error"),
        }
    }
}

/// TODO
#[derive(Debug)]
pub enum Number {
    Int(i64),
    Float(f64),
}

impl Number {
    /// TODO
    pub fn to_bool(&self) -> bool {
        match self {
            Number::Int(i) => *i != 0,
            Number::Float(f) => todo!(),
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
