use crate::value::Value;
use std::{
    fmt::{self, Display},
    ops::Deref,
    sync::Arc,
};

/// TODO
///
/// Operations on this array will use optimistic mutation, meaning they will
/// mutate the current array in place if there are no other references to it,
/// and only clone the contents if they're referenced in multiple places. This
/// makes this method very fast for most use cases.
#[derive(Clone, Debug, Default, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
pub struct Array(Arc<Vec<Value>>);

impl Array {
    /// Append a single value to the end of the array.
    pub fn push(self, value: Value) -> Self {
        self.with_inner(|vec| vec.push(value))
    }

    /// Insert all elements from another array into this one.
    pub fn concat(self, other: Self) -> Self {
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
        if let Some(vec) = Arc::get_mut(&mut self.0) {
            // If we're the only owner of the arc, we can mutate in place
            f(vec);
            self
        } else {
            // The arc is aliased; we have to clone the contents before mutating
            let mut vec = self.0.deref().clone();
            f(&mut vec);
            Self(vec.into())
        }
    }
}

impl Deref for Array {
    type Target = [Value];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Into<Value>> From<Vec<T>> for Array {
    fn from(value: Vec<T>) -> Self {
        value.into_iter().map(T::into).collect()
    }
}

impl From<Array> for Vec<Value> {
    fn from(array: Array) -> Self {
        Arc::unwrap_or_clone(array.0)
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

impl FromIterator<Value> for Array {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        Self(Arc::new(iter.into_iter().collect()))
    }
}

impl IntoIterator for Array {
    type Item = Value;
    type IntoIter = <Vec<Value> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        Arc::unwrap_or_clone(self.0).into_iter()
    }
}
