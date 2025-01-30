use crate::value::Value;
use std::{
    fmt::{self, Display},
    ops::Deref as _,
    sync::Arc,
};

/// TODO
#[derive(Clone, Debug, Default, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Array(Arc<Vec<Value>>);

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
        match Arc::try_unwrap(other.0) {
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
        if let Some(vec) = Arc::get_mut(&mut self.0) {
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
