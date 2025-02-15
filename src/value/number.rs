use std::{
    f64,
    fmt::{self, Display},
    ops::{Add, Div, Mul, Rem, Sub},
};

/// TODO
#[derive(Copy, Clone, Debug)]
pub enum Number {
    Int(i64),
    Float(f64),
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
            // TODO handle int-float equality
            _ => false,
        }
    }
}

impl Add for Number {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(lhs), Number::Int(rhs)) => {
                lhs.wrapping_add(rhs).into()
            }
            // TODO handle overflow or w/e
            (Number::Int(lhs), Number::Float(rhs)) => {
                ((lhs as f64) + rhs).into()
            }
            (Number::Float(lhs), Number::Int(rhs)) => {
                (lhs + (rhs as f64)).into()
            }
            (Number::Float(lhs), Number::Float(rhs)) => (lhs + rhs).into(),
        }
    }
}

impl Sub for Number {
    type Output = Self;

    fn sub(self, _: Self) -> Self::Output {
        todo!()
    }
}

impl Mul for Number {
    type Output = Self;

    fn mul(self, _: Self) -> Self::Output {
        todo!()
    }
}

impl Div for Number {
    type Output = Self;

    fn div(self, _: Self) -> Self::Output {
        todo!()
    }
}

impl Rem for Number {
    type Output = Self;

    fn rem(self, _: Self) -> Self::Output {
        todo!()
    }
}

impl From<i8> for Number {
    fn from(value: i8) -> Self {
        Self::Int(value.into())
    }
}

impl From<u8> for Number {
    fn from(value: u8) -> Self {
        Self::Int(value.into())
    }
}

impl From<i16> for Number {
    fn from(value: i16) -> Self {
        Self::Int(value.into())
    }
}

impl From<u16> for Number {
    fn from(value: u16) -> Self {
        Self::Int(value.into())
    }
}

impl From<i32> for Number {
    fn from(value: i32) -> Self {
        Self::Int(value.into())
    }
}

impl From<u32> for Number {
    fn from(value: u32) -> Self {
        Self::Int(value.into())
    }
}

impl From<i64> for Number {
    fn from(value: i64) -> Self {
        Self::Int(value)
    }
}

impl From<u64> for Number {
    fn from(value: u64) -> Self {
        todo!()
    }
}

impl From<f32> for Number {
    fn from(value: f32) -> Self {
        Self::Float(value.into())
    }
}

impl From<f64> for Number {
    fn from(value: f64) -> Self {
        Self::Float(value)
    }
}
