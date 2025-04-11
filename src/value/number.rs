use crate::error::ValueError;
use std::{
    cmp::Ordering,
    f64,
    fmt::{self, Display},
    ops::{Add, Div, Mul, Neg, Rem, Sub},
};

/// TODO
#[derive(Copy, Clone, Debug)]
pub enum Number {
    Int(i64),
    Float(f64),
    // TODO support bigints
}

impl Number {
    /// Not a Number
    pub const NAN: Self = Self::Float(f64::NAN);

    /// TODO
    pub fn to_bool(self) -> bool {
        match self {
            Number::Int(i) => i != 0,
            Number::Float(f) => f != 0.0,
        }
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Number::Int(i) => write!(f, "{i}"),
            Number::Float(n) => write!(f, "{n}"),
        }
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::Float(f), Self::Int(i)) | (Self::Int(i), Self::Float(f)) => {
                (*i as f64) == *f
            }
        }
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        match (self, rhs) {
            (Number::Int(i1), Number::Int(i2)) => i1.partial_cmp(i2),
            (Number::Float(f1), Number::Float(f2)) => f1.partial_cmp(f2),
            (Number::Int(_), Number::Float(_)) => todo!(),
            (Number::Float(_), Number::Int(_)) => todo!(),
        }
    }
}

impl Neg for Number {
    type Output = Self;

    /// Apply arithmetic negation to this number
    fn neg(self) -> Self::Output {
        match self {
            Self::Int(i) => Self::Int(-i),
            Self::Float(f) => Self::Float(-f),
        }
    }
}

/// Implement a numeric binary operator for `Number`
macro_rules! impl_numeric_binary_op {
    ($trait:ident, $func:ident, $op:tt) => {
        /// Apply this arithmetic operation between two numbers. If both numbers
        /// are ints, keep them as ints. If either is a float, cast both to
        /// floats and apply the operation.
        impl $trait for Number {
            type Output = Self;

            fn $func(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Number::Int(lhs), Number::Int(rhs)) => {
                        // TODO handle overflow/underflow
                        (lhs $op rhs).into()
                    }
                    (Number::Int(lhs), Number::Float(rhs)) => {
                        ((lhs as f64) $op rhs).into()
                    }
                    (Number::Float(lhs), Number::Int(rhs)) => {
                        (lhs $op (rhs as f64)).into()
                    }
                    (Number::Float(lhs), Number::Float(rhs)) => {
                        (lhs $op rhs).into()
                    }
                }
            }
        }
    };
}

/// Implement `From<T> for Number`, for infallible conversions
macro_rules! impl_from {
    ($type:ty, $variant:ident) => {
        impl From<$type> for $crate::Number {
            fn from(value: $type) -> Self {
                Self::$variant(value.into())
            }
        }
    };
}

/// Implement `TryFrom<T> for Number`, for number types that may be out of range
macro_rules! impl_try_from {
    ($type:ty, $variant:ident) => {
        impl TryFrom<$type> for Number {
            type Error = ValueError;

            fn try_from(value: $type) -> Result<Self, Self::Error> {
                Ok(Self::$variant(
                    value.try_into().expect("TODO convert error"),
                ))
            }
        }
    };
}

/// Implement `TryFrom<Number> for T`. This is implemented for all numeric
/// types, because converting from `Number` is always fallible. The `Number`
/// could be an int and the output a float, or vice versa.
macro_rules! impl_try_from_number {
    ($type:ty, int) => {
        impl TryFrom<Number> for $type {
            type Error = ValueError;

            fn try_from(number: Number) -> Result<Self, Self::Error> {
                match number {
                    Number::Int(i) => {
                        // This cast may not actually be fallible, but this
                        // allows us to use the same code for all int types
                        i.try_into().map_err(|_| ValueError::Number {
                            expected: std::any::type_name::<$type>(),
                            number,
                            description: "Value out of range",
                        })
                    }
                    Number::Float(_) => Err(ValueError::Number {
                        expected: std::any::type_name::<$type>(),
                        number,
                        description: "Expected int",
                    }),
                }
            }
        }
    };
    ($type:ty, float) => {
        impl TryFrom<Number> for $type {
            type Error = ValueError;

            fn try_from(number: Number) -> Result<Self, Self::Error> {
                match number {
                    Number::Int(_) => Err(ValueError::Number {
                        expected: std::any::type_name::<$type>(),
                        number,
                        description: "Expected float",
                    }),
                    // Rust doesn't provide any fallible conversions for
                    // f64->f32, so we just have to cast and round
                    Number::Float(f) => Ok(f as $type),
                }
            }
        }
    };
}

impl_numeric_binary_op!(Add, add, +);
impl_numeric_binary_op!(Sub, sub, -);
impl_numeric_binary_op!(Mul, mul, *);
impl_numeric_binary_op!(Div, div, /);
impl_numeric_binary_op!(Rem, rem, %);

impl_from!(i8, Int);
impl_from!(u8, Int);
impl_from!(i16, Int);
impl_from!(u16, Int);
impl_from!(i32, Int);
impl_from!(u32, Int);
impl_from!(i64, Int);
impl_from!(f32, Float);
impl_from!(f64, Float);

impl_try_from!(u64, Int);
impl_try_from!(i128, Int);
impl_try_from!(u128, Int);

impl_try_from_number!(u8, int);
impl_try_from_number!(i8, int);
impl_try_from_number!(u16, int);
impl_try_from_number!(i16, int);
impl_try_from_number!(u32, int);
impl_try_from_number!(i32, int);
impl_try_from_number!(u64, int);
impl_try_from_number!(i64, int);
impl_try_from_number!(u128, int);
impl_try_from_number!(i128, int);
impl_try_from_number!(f32, float);
impl_try_from_number!(f64, float);

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    #[test_case(3i64, 7i64, 10i64; "int int")]
    #[test_case(3i64, 7.0f64, 10.0f64; "int float")]
    #[test_case(3.0f64, 7i64, 10.0f64; "float int")]
    #[test_case(3.0f64, 7.0f64, 10.0f64; "float float")]
    fn add(
        a: impl Into<Number>,
        b: impl Into<Number>,
        expected: impl Into<Number>,
    ) {
        assert_eq!(a.into() + b.into(), expected.into());
    }
}
