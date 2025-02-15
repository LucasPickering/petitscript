//! Macros for implementing traits on various types

/// Implement a numeric binary operator for [Value]
macro_rules! impl_value_numeric_binary_op {
    ($trait:ident, $func:ident, $op:tt) => {
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

/// Implement two complementary traits for a type `T`, where `T` is convertible
/// to a particular variant (e.g. the `String` type and the `String` variant):
/// - `From<T>` for `Value` - Convert infallibly to `T`'s natural variant
/// - `FromJs` for `T` - Convert from `T`'s variant to `T`, returning a type
///   error if the value is not of the expected variant
macro_rules! impl_conversions {
    ($type:ty, $variant:ident) => {
        // In most cases, the variant on Value matches the one on ValueType, so
        // this case eliminates the need for the duplication
        impl_conversions!($type, $variant, $variant);
    };
    ($type:ty, $variant:ident, $type_variant:ident) => {
        impl From<$type> for $crate::Value {
            fn from(value: $type) -> Self {
                Self::$variant(value.into())
            }
        }

        impl $crate::FromJs for $type {
            fn from_js(value: $crate::Value) -> $crate::RuntimeResult<Self> {
                if let $crate::Value::$variant(value) = value {
                    Ok(value.into())
                } else {
                    Err($crate::RuntimeError::Type {
                        expected: $crate::ValueType::$type_variant,
                        actual: value.type_(),
                    })
                }
            }
        }
    };
}

pub(crate) use impl_conversions;
pub(crate) use impl_value_numeric_binary_op;
