use crate::{
    value::{FromPetitArgs, Function, IntoPetitResult, Value},
    Process,
};
use indexmap::IndexMap;
use std::{
    fmt::{self, Display},
    ops::Deref as _,
    sync::Arc,
};

/// A reference-counted immutable key-value store
#[derive(Clone, Debug, Default, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
pub struct Object(Arc<IndexMap<String, Value>>);

impl Object {
    /// Create a new empty object
    pub fn new() -> Self {
        Self::default()
    }

    /// Get a value from the object by key, or undefined if not present
    pub fn get(&self, key: &str) -> &Value {
        self.0
            .iter()
            .find_map(|(k, v)| if k == key { Some(v) } else { None })
            .unwrap_or(&Value::Undefined)
    }

    /// Get the number of entries in this object
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Check if this object has any entries
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// TODO
    pub fn insert(
        self,
        name: impl Into<String>,
        value: impl Into<Value>,
    ) -> Self {
        self.with_inner(|map| {
            map.insert(name.into(), value.into());
        })
    }

    /// Insert a native function into this object
    pub fn insert_fn<F, Args, Out>(
        self,
        name: impl Into<String>,
        function: F,
    ) -> Self
    where
        F: 'static + Fn(&Process, Args) -> Out + Send + Sync,
        Args: FromPetitArgs,
        Out: IntoPetitResult,
    {
        // Use the naem as the and also attach it to the function for printing
        let name = name.into();
        self.insert(name.clone(), Value::from(Function::native(name, function)))
    }

    /// Return an iterator over the key-value pairs of this object, in order of
    /// insertion
    pub fn iter(&self) -> impl Iterator<Item = (&String, &Value)> {
        self.0.iter()
    }

    /// Get an owned iterator over this object's keys. This will clone the keys
    /// only if this object has multiple active references
    pub fn into_keys(self) -> impl Iterator<Item = String> {
        Arc::unwrap_or_clone(self.0).into_keys()
    }

    /// Get an owned iterator over this object's values. This will clone the
    /// values only if this object has multiple active references
    pub fn into_values(self) -> impl Iterator<Item = Value> {
        Arc::unwrap_or_clone(self.0).into_values()
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
            Ok(other) => self.with_inner(|map| map.extend(other)),
            // Other object is shared (uncommon case) - we need to clone all its
            // contents
            Err(other) => self.with_inner(|map| {
                map.extend(other.iter().map(|(k, v)| (k.clone(), v.clone())))
            }),
        }
    }

    /// TODO
    fn with_inner(
        mut self,
        f: impl FnOnce(&mut IndexMap<String, Value>),
    ) -> Self {
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

impl Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        for (i, (key, value)) in self.0.iter().enumerate() {
            if i > 0 {
                // TODO pretty printing
                write!(f, ", ")?;
            }
            write!(f, "{key}: {value}")?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl<const N: usize, T: Into<Value>> From<[(&str, T); N]> for Object {
    fn from(value: [(&str, T); N]) -> Self {
        Self(Arc::new(
            value
                .into_iter()
                .map(|(k, v)| (k.to_owned(), v.into()))
                .collect(),
        ))
    }
}

impl<T: Into<Value>> From<IndexMap<&str, T>> for Object {
    fn from(map: IndexMap<&str, T>) -> Self {
        Self(Arc::new(
            map.into_iter()
                .map(|(k, v)| (k.to_owned(), v.into()))
                .collect(),
        ))
    }
}

impl<T: Into<Value>> From<IndexMap<String, T>> for Object {
    fn from(map: IndexMap<String, T>) -> Self {
        Self(Arc::new(
            map.into_iter().map(|(k, v)| (k, v.into())).collect(),
        ))
    }
}

impl From<Object> for IndexMap<String, Value> {
    fn from(object: Object) -> Self {
        Arc::unwrap_or_clone(object.0)
    }
}

impl FromIterator<(String, Value)> for Object {
    fn from_iter<T: IntoIterator<Item = (String, Value)>>(iter: T) -> Self {
        Self(Arc::new(iter.into_iter().collect()))
    }
}

impl IntoIterator for Object {
    type Item = (String, Value);
    type IntoIter = <IndexMap<String, Value> as IntoIterator>::IntoIter;

    /// Get an iterator over this object's `(key, value)` pairs. This will clone
    /// the pairs only if this has multiple active references
    fn into_iter(self) -> Self::IntoIter {
        Arc::unwrap_or_clone(self.0).into_iter()
    }
}
