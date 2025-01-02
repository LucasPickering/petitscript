use indexmap::IndexMap;

/// TODO
pub enum Value {
    Null,
    Undefined,
    Boolean(bool),
    Number(Number),
    String(String),
    Array(Vec<Self>),
    Object(IndexMap<Self, Self>),
    Function(/* TODO */),
}

/// TODO
pub enum Number {
    Int(i64),
    Float(f64),
}
