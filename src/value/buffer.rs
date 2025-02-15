use crate::value::macros::{impl_value_conversions, impl_value_from};
use bytes::Bytes;
use std::{
    fmt::{self, Display},
    ops::Deref,
};

/// A reference-counted immutable byte buffer. This type _cannot_ be constructed
/// from PetitJS code; it must originate in native code. This uses
/// [bytes::Bytes] for the internal storage type, because it enables fast and
/// efficient sharing of bytes, and is commonly used throughout the Rust
/// ecosystem.
#[derive(Clone, Debug)]
pub struct Buffer(Bytes);

impl Deref for Buffer {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for Buffer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Print up to the first 8 bytes
        const MAX: usize = 8;
        write!(f, "[")?;
        for (i, byte) in self.0[..MAX].iter().enumerate() {
            write!(f, "{byte:x}")?;
            if i > 0 {
                write!(f, ", ")?;
            }
        }
        if self.len() > MAX {
            write!(f, "..]")?;
        }
        Ok(())
    }
}

impl From<&[u8]> for Buffer {
    fn from(bytes: &[u8]) -> Self {
        Self(bytes.to_owned().into())
    }
}

impl From<Vec<u8>> for Buffer {
    fn from(bytes: Vec<u8>) -> Self {
        Self(bytes.into())
    }
}

impl From<Bytes> for Buffer {
    fn from(bytes: bytes::Bytes) -> Self {
        Self(bytes)
    }
}

impl From<Buffer> for Bytes {
    fn from(buffer: Buffer) -> Self {
        buffer.0
    }
}

impl From<Buffer> for Vec<u8> {
    fn from(buffer: Buffer) -> Self {
        buffer.0.into()
    }
}

// Conversions are stashed here instead of the parent module so we don't need
// a feature gate on each one
impl_value_conversions!(Buffer, Buffer);
impl_value_conversions!(Bytes, Buffer);
impl_value_conversions!(Vec<u8>, Buffer);
// One-way conversion, because we can't convert back to a ref
impl_value_from!(&[u8], Buffer);
