//! Runtime values

use crate::Result;
use boa_ast::function::{FormalParameterList, FunctionBody};
use id_arena::Arena;
use indexmap::IndexMap;

type ValueRef = id_arena::Id<ValueKind>;

/// TODO
#[derive(Debug)]
pub struct Value(ValueInner);

impl Value {
    /// TODO
    pub fn resolve<'a>(
        &'a self,
        arena: &'a Arena<ValueKind>,
    ) -> Result<&ValueKind> {
        match &self.0 {
            ValueInner::Owned(value) => Ok(value),
            ValueInner::Borrowed(id) => {
                arena.get(*id).ok_or_else(|| todo!("error"))
            }
        }
    }
}

impl From<ValueKind> for Value {
    fn from(value: ValueKind) -> Self {
        Self(ValueInner::Owned(value))
    }
}

impl From<ValueRef> for Value {
    fn from(value: ValueRef) -> Self {
        Self(ValueInner::Borrowed(value))
    }
}

#[derive(Debug)]
enum ValueInner {
    Owned(ValueKind),
    Borrowed(ValueRef),
}

/// TODO
#[derive(Debug)]
pub enum ValueKind {
    Null,
    Undefined,
    Boolean(bool),
    Number(Number),
    String(String),
    Array(Vec<ValueRef>),
    Object(IndexMap<String, ValueRef>),
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
