//! Macros for implementing traits on various types

/// Implement a numeric binary operator for `Value`
macro_rules! impl_value_numeric_binary_op {
    ($trait:ident, $func:ident, $op:tt) => {
        /// Apply this arithmetic operation between two values. Cast both values
        /// to numbers with [Self::to_number]; if both are successful ly cast,
        /// apply the operation between them. If not, return `NaN`.
        impl $trait for Value {
            type Output = Self;

            fn $func(self, rhs: Self) -> Self::Output {
                match (self.to_number(), rhs.to_number()) {
                    (Some(lhs), Some(rhs)) => (lhs $op rhs).into(),
                    _ => Number::NAN.into(),
                }
            }
        }
    };
}

/// Implement `From<T>` for `Value`, where `T` is convertible to a particular
/// `Value` variant.
macro_rules! impl_value_from {
    ($type:ty, $variant:ident) => {
        impl From<$type> for $crate::Value {
            fn from(value: $type) -> Self {
                Self::$variant(value.into())
            }
        }
    };
}

/// Implement `IntoPs` for a type `T`, where `T` is fallibly convertibale to a
/// a particular variant. This is only useful for fallible conversions;
/// infallible converts are already covered by a blanket implement of `IntoPs`
macro_rules! impl_into_ps {
    ($type:ty, $variant:ident) => {
        impl IntoPs for $type {
            fn into_ps(self) -> Result<Value, ValueError> {
                Ok(Value::$variant(self.try_into()?))
            }
        }
    };
}

/// TODO
macro_rules! ensure_type {
    ($value:expr, $variant:ident) => {
        if let $crate::Value::$variant(value) = $value {
            value
        } else {
            return Err($crate::error::ValueError::Type {
                expected: $crate::ValueType::$variant,
                actual: $value.type_(),
            });
        }
    };
}

/// Implement `FromPs` for `T`, where `T` is convertible from the value in one
/// variant of `Value`.
macro_rules! impl_from_ps {
    ($type:ty, $variant:ident, infallible) => {
        impl $crate::FromPs for $type {
            fn from_ps(
                value: $crate::Value,
            ) -> Result<Self, $crate::error::ValueError> {
                let value =
                    $crate::value::macros::ensure_type!(value, $variant);
                Ok(value.into())
            }
        }
    };
    ($type:ty, $variant:ident, fallible) => {
        impl $crate::FromPs for $type {
            fn from_ps(
                value: $crate::Value,
            ) -> Result<Self, $crate::error::ValueError> {
                // Fallible in two ways: wrong type, or value conversion fails
                let value =
                    $crate::value::macros::ensure_type!(value, $variant);
                value.try_into()
            }
        }
    };
}

/// Implement two complementary traits for a type `T`, where `T` is convertible
/// to a particular variant (e.g. the `String` type and the `String` variant).
/// - `From<T> for Value` (infallible) OR `TryFrom<T> for Value` (fallible)
/// - `FromPs for T` - Ensure the value is of the expected variant, then use
///   either an infallible (`From`) or fallible (`TryFrom`) conversion to
///   convert the variant of `Value` into `T`.
///
/// Note: from_ps being infallible just means the _value_ conversion is
/// infallible. FromPs can always fail if the value has the wrong type.
/// Fallibility here just refers to whether we should use `From` or `TryFrom`
/// to convert the contained value, after narrowing the type of the `Value
macro_rules! impl_value_conversions {
    ($type:ty, $variant:ident) => {
        $crate::value::macros::impl_value_from!($type, $variant);
        $crate::value::macros::impl_from_ps!($type, $variant, infallible);
    };
    ($type:ty, $variant:ident, to_ps: infallible, from_ps: infallible) => {
        $crate::value::macros::impl_value_from!($type, $variant);
        $crate::value::macros::impl_from_ps!($type, $variant, infallible);
    };
    ($type:ty, $variant:ident, to_ps: infallible, from_ps: fallible) => {
        $crate::value::macros::impl_value_from!($type, $variant);
        $crate::value::macros::impl_from_ps!($type, $variant, fallible);
    };
    // Note: there are no cases of fallible/infallible, so it's not supported
    ($type:ty, $variant:ident, to_ps: fallible, from_ps: fallible) => {
        $crate::value::macros::impl_into_ps!($type, $variant);
        $crate::value::macros::impl_from_ps!($type, $variant, fallible);
    };
}

pub(crate) use ensure_type;
pub(crate) use impl_from_ps;
pub(crate) use impl_into_ps;
pub(crate) use impl_value_conversions;
pub(crate) use impl_value_from;
pub(crate) use impl_value_numeric_binary_op;
