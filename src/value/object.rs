use crate::value::Value;
use indexmap::IndexMap;
use std::{
    fmt::{self, Display},
    ops::Deref as _,
    sync::Arc,
};

/// TODO
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Object(Arc<IndexMap<String, Value>>);

impl Object {
    /// Get a value from the object by key, or undefined if not present
    pub fn get(&self, key: &str) -> Value {
        self.0
            .iter()
            .find_map(|(k, v)| if k == key { Some(v) } else { None })
            .cloned()
            .unwrap_or_default()
    }

    /// TODO
    pub fn insert(self, name: impl Into<String>, value: Value) -> Self {
        self.with_inner(|map| {
            map.insert(name.into(), value);
        })
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

impl From<IndexMap<String, Value>> for Object {
    fn from(map: IndexMap<String, Value>) -> Self {
        Self(map.into())
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

#[cfg(feature = "serde")]
impl serde::Serialize for Object {
    fn serialize<S>(&self, _: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        todo!()
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for Object {
    fn deserialize<D>(_: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        todo!()
    }
}
