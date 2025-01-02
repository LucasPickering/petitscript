use crate::value::Value;
use indexmap::IndexMap;

/// TODO
pub struct Script {
    globals: IndexMap<String, Value>,
}
