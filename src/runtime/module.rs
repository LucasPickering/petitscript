//! ES6-esque modules

use crate::value::Value;
use std::{
    collections::HashMap,
    fmt::{self, Display},
};

/// TODO
#[derive(Debug, Default)]
pub struct Module {
    /// Default exported value
    pub default: Option<Value>,
    /// Named exported values
    pub named: HashMap<String, Value>,
}

impl Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(default) = &self.default {
            writeln!(f, "(default): {default}")?;
        }
        for (name, value) in &self.named {
            writeln!(f, "{name}: {value}")?;
        }
        Ok(())
    }
}
