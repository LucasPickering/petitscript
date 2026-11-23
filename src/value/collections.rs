//! Arrays and objects

use crate::{
    World,
    value::{FromPetitArgs, Function, IntoPetitResult, Value},
};
use indexmap::IndexMap;
use std::{
    fmt::{self, Display},
    ops::Deref,
    sync::Arc,
};

/// A reference-counted immutable ordered collection of values.
///
/// Because arrays are immutable, any operation "modifying" it actually
/// consumes the array and returns a new one. However, the container uses
/// optimistic mutation. If no other references to the consumed array exist,
/// the object will be mutated in place and returned. This makes modifying
/// functions very fast for most use cases.
#[derive(Clone, Debug, Default, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
pub struct Array(Arc<Vec<Value>>);

impl Array {
    /// Create a new empty array
    pub fn new() -> Self {
        Self::default()
    }

    /// Append a single value to the end of the array
    ///
    /// ```
    /// # use petitscript::value::Array;
    /// assert_eq!(Array::new().push(1), [1].into());
    /// ```
    #[must_use]
    pub fn push(self, value: impl Into<Value>) -> Self {
        self.with_mut(|vec| vec.push(value.into()))
    }

    /// Insert all elements from another array into this one
    ///
    /// ```
    /// # use petitscript::value::Array;
    /// assert_eq!(Array::new().push(1).concat([2, 3].into()), [1, 2, 3].into());
    /// ```
    #[must_use]
    pub fn concat(self, other: Self) -> Self {
        // If we're the sole owner of the other object, we can move the items
        // out. Otherwise we have to clone them over
        match Arc::try_unwrap(other.0) {
            // If this object is empty, and we now own the other one, just point
            // to its buffer and avoid all copies. This optimizes for a common
            // pattern {...obj1, field: "value"}, to avoid repeated allocations
            Ok(other) if self.0.is_empty() => self.with_mut(|vec| *vec = other),
            // We own the other one, so we can move each inner item into our
            // buffer without cloning
            Ok(other) => self.with_mut(|vec| vec.extend(other)),
            // Other object is shared (uncommon case) - we need to clone all its
            // contents
            Err(other) => {
                self.with_mut(|vec| vec.extend(other.iter().cloned()))
            }
        }
    }

    /// Perform an optimistic mutation
    fn with_mut(self, f: impl FnOnce(&mut Vec<Value>)) -> Self {
        Self(with_mut(self.0, f))
    }
}

impl Deref for Array {
    type Target = [Value];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<const N: usize, T: Into<Value>> From<[T; N]> for Array {
    fn from(value: [T; N]) -> Self {
        value.into_iter().map(T::into).collect()
    }
}

impl<T: Into<Value>> From<Vec<T>> for Array {
    fn from(value: Vec<T>) -> Self {
        value.into_iter().map(T::into).collect()
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

impl<V: Into<Value>> FromIterator<V> for Array {
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        Self(Arc::new(iter.into_iter().map(V::into).collect()))
    }
}

impl IntoIterator for Array {
    type Item = Value;
    type IntoIter = <Vec<Value> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        Arc::unwrap_or_clone(self.0).into_iter()
    }
}

/// A reference-counted immutable key-value store
///
/// Object keys must be strings.
///
/// Because objects are immutable, any operation "modifying" it actually
/// consumes the object and returns a new one. However, the container uses
/// optimistic mutation. If no other references to the consumed object exist,
/// the object will be mutated in place and returned. This makes modifying
/// functions very fast for most use cases.
#[derive(Clone, Debug, Default, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
pub struct Object(Arc<IndexMap<String, Value>>);

impl Object {
    /// Create a new empty object
    pub fn new() -> Self {
        Self::default()
    }

    /// Get a value from the object by key, or `None` if not present
    pub fn get(&self, key: &str) -> Option<&Value> {
        self.0.iter().find_map(|(k, v)| (k == key).then_some(v))
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

    /// Add a new property to this object, returning a new object
    ///
    /// ```
    /// # use petitscript::value::Object;
    /// assert_eq!(Object::new().insert("a", 1), Object::from_iter([("a", 1)]));
    /// ```
    #[must_use]
    pub fn insert(
        self,
        name: impl Into<String>,
        value: impl Into<Value>,
    ) -> Self {
        self.with_mut(|map| {
            map.insert(name.into(), value.into());
        })
    }

    /// Insert a native function into this object
    ///
    /// This is a convenience for [`Self::insert`] when the value is a native
    /// function. This handles the conversion from a Rust function to
    /// [`Function`].
    #[must_use]
    pub fn insert_fn<F, Args, Out>(
        self,
        name: impl Into<String>,
        function: F,
    ) -> Self
    where
        F: 'static + Fn(&World, Args) -> Out + Send + Sync,
        Args: FromPetitArgs,
        Out: IntoPetitResult,
    {
        // Use the name as the key and also attach it to the function
        let name = name.into();
        self.insert(name.clone(), Value::from(Function::native(name, function)))
    }

    /// Insert all properties from another object into this one
    ///
    /// ```
    /// # use petitscript::value::Object;
    /// assert_eq!(
    ///     Object::from_iter([("a", 1)]).insert_all(Object::from_iter([("b", 2)])),
    ///     Object::from_iter([("a", 1), ("b", 2)])
    /// );
    /// ```
    #[must_use]
    pub fn insert_all(self, other: Self) -> Self {
        // If we're the sole owner of the other object, we can move the items
        // out. Otherwise we have to clone them over
        match Arc::try_unwrap(other.0) {
            // If this object is empty, and we now own the other one, just point
            // to its buffer and avoid all copies. This optimizes for a common
            // pattern {...obj1, field: "value"}, to avoid repeated allocations
            Ok(other) if self.0.is_empty() => self.with_mut(|vec| *vec = other),
            // We own the other one, so we can move each inner item into our
            // buffer without cloning
            Ok(other) => self.with_mut(|map| map.extend(other)),
            // Other object is shared (uncommon case) - we need to clone all its
            // contents
            Err(other) => self.with_mut(|map| {
                map.extend(other.iter().map(|(k, v)| (k.clone(), v.clone())));
            }),
        }
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

    /// Perform an optimistic mutation
    fn with_mut(self, f: impl FnOnce(&mut IndexMap<String, Value>)) -> Self {
        Self(with_mut(self.0, f))
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

impl<K, V> FromIterator<(K, V)> for Object
where
    K: Into<String>,
    V: Into<Value>,
{
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Self(Arc::new(
            iter.into_iter()
                .map(|(k, v)| (k.into(), v.into()))
                .collect(),
        ))
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

/// Perform an optimistic mutation on a value in an `Arc`
///
/// If this is the only reference to the Arc, it will be mutated in place and
/// returned. Otherwise, the inner value will be cloned, mutated, and wrapped
/// in a new `Arc`.
fn with_mut<T: Clone>(mut arc: Arc<T>, f: impl FnOnce(&mut T)) -> Arc<T> {
    if let Some(value) = Arc::get_mut(&mut arc) {
        // If we're the only owner of the arc, we can mutate in place
        f(value);
        arc
    } else {
        // The arc is aliased; we have to clone the contents before mutating
        let mut value = (*arc).clone();
        f(&mut value);
        Arc::new(value)
    }
}
