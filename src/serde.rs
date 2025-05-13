//! Serialization and deserialization via the [serde] crate
//!
//! This module is designed to imitate a Serde data format crate, and thus
//! follows [the conventions laid out by Serde](https://serde.rs/conventions.html),
//! with the exception of the `Error` and `Result` types: we use [ValueError]
//! instead and no `Result` alias is provided.

mod de;
mod impls;
mod ser;

pub use de::Deserializer;
pub use ser::Serializer;

use crate::{error::ValueError, value::Function, Value};
use serde::{Deserialize, Serialize};
use std::{cell::RefCell, collections::BTreeMap};

/// Serialize an instance of type `T` into a PetitScript value
pub fn to_value<T: Serialize>(data: &T) -> Result<Value, ValueError> {
    data.serialize(Serializer)
}

/// Deserialize an instance of type `T` from a PetitScript value
pub fn from_value<'de, T: Deserialize<'de>>(
    value: Value,
) -> Result<T, ValueError> {
    T::deserialize(Deserializer::new(value))
}

/// A tool for out-of-band serialization and deserialization of functions.
/// Functions are challenging to serialize and serve little value as raw data.
/// The only meaningful use case to ser/de functions is going between [Value]
/// and another type `T` that contains a `Function` internally. So we only need
/// to support pass-through ser/de, in which a `Function` is mapped right back
/// to a `Function`. Rather than trying to shove the function through serde's
/// data model and mapping it back out, it's much easier to store the function
/// externally (using TLS) and write a simple identifier. We'll extract the ID
/// on the other side and give it to the pool to redeem it for the original
/// function.
///
/// This pattern allows user functions to avoid serializing their entire
/// definition (which could be large), and enables inline storage of native
/// functions, as there is no way to serialize native code (or pointers to it)
/// without `unsafe`.
///
/// This works by create a newtype struct in the data model with a unique name,
/// such that it's very unlikely for a user to accidentally create another
/// struct with the same name. The struct contains a single u64, which uniquely
/// identifies a corresponding function in the pool.
///
/// This of course comes with the limitation that both ends of the ser/de
/// pipeline must run in the same thread, which prohibits any persistence of
/// functions.
///
/// There are two paths supported by this:
/// - Serialization: `T` contains a Function and wants to serialize to a
///   `Value`. `T::serialize`` will insert the function into the pool, and our
///   `Serializer` will pull it back out and create a `Value` from it.
/// - Deserialization: Our `Deserializer` encounters a Function and inserts it
///   into the pool. If `T` wants a Function in that position, it will pull it
///   back out from the pool.
///
/// https://lucumr.pocoo.org/2021/11/14/abusing-serde/
struct FunctionPool {
    next_id: u64,
    pool: BTreeMap<u64, Function>,
}

impl FunctionPool {
    /// Unique identifier for the newtype struct that we create in the serde
    /// data model. Pick a string that's extremely unlikely to collide with
    /// anything a user would actually write.
    ///
    /// This string **does not actually get serialized** in most data formats,
    /// as struct names are typically thrown away and the wrapped value is
    /// serialized alone. The deserializer has to know that the value in that
    /// position is intended to be a function pool ID. It knows this because
    /// the Deserialize impl requests a newtype struct of this name. So the name
    /// is used during serialization and deserialization time, but doesn't
    /// actually appear in the serialized output.
    const STRUCT_NAME: &'static str = "$petitscript::Function";

    thread_local! {
        static POOL: RefCell<FunctionPool> = const {
            RefCell::new(FunctionPool::new())
        };
        // TODO
        // static IS_ACTIVE: AtomicBool = false;
    }

    const fn new() -> Self {
        Self {
            next_id: 0,
            pool: BTreeMap::new(),
        }
    }

    /// Add a function to the pool and return a new unique ID for it
    fn insert(function: Function) -> u64 {
        Self::POOL.with_borrow_mut(|pool| {
            let id = pool.next_id;
            // It's exceptionally unlikely we'll ever wrap around these IDs, but
            // if we do it's even more unlikely that the the original IDs are
            // still in the pool. Zero-cost preventative maintenance
            pool.next_id = pool.next_id.wrapping_add(1);
            pool.pool.insert(id, function);
            id
        })
    }

    /// Take a function from the pool corresponding to the given ID. Return an
    /// error if the function isn't in the pool. This typically indicates a user
    /// tried to deserialize from a format other than [Value] into a [Function],
    /// which is not supported.
    fn take(id: u64) -> Result<Function, ValueError> {
        Self::POOL
            .with_borrow_mut(|pool| pool.pool.remove(&id))
            .ok_or(ValueError::FunctionSerde)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{
            Expression, FunctionBody, FunctionDefinition, FunctionParameter,
        },
        value::{
            function::{BoundFunction, Function},
            Array, Object,
        },
    };
    use indexmap::indexmap;
    use serde::de::{DeserializeOwned, Visitor};
    use std::fmt::{self, Debug};
    use test_case::test_case;

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct UnitStruct;

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct NewtypeStruct(i64);

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct TupleStruct(i64, bool);

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct FieldStruct {
        s: String,
        b: bool,
        i: i64,
        f: f64,
        unit_enum: UnitEnum,
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    enum UnitEnum {
        /// "A"
        A,
        /// "B"
        B,
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    enum TaggedEnum {
        /// {Unit: undefined}
        Unit,
        /// {Newtype: 3}
        Newtype(i64),
        /// {Tuple: [3, true]}
        Tuple(i64, bool),
        /// {Struct: {b: true}}
        Struct { b: bool },
    }

    /// A custom struct containing a function. This tests nested ser/de of
    /// Function, which has special logic
    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct FunctionStruct {
        f: Function,
    }

    /// Custom byte buffer to testing buffer ser/de. The only byte struct that
    /// serde supports out of the box is &[u8], which doesn't work for us
    /// because our Deserializer doesn't support borrowed deserialization.
    /// bytes::Bytes doesn't support serde either.
    #[derive(Debug, PartialEq)]
    struct Bytes(Vec<u8>);

    impl Serialize for Bytes {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            serializer.serialize_bytes(&self.0)
        }
    }

    impl<'de> Deserialize<'de> for Bytes {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'de>,
        {
            struct BytesVisitor;

            impl<'de> Visitor<'de> for BytesVisitor {
                type Value = Bytes;

                fn expecting(
                    &self,
                    formatter: &mut fmt::Formatter,
                ) -> fmt::Result {
                    write!(formatter, "bytes")
                }

                fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<Self::Value, E>
                where
                    E: serde::de::Error,
                {
                    Ok(Bytes(v))
                }
            }

            deserializer.deserialize_byte_buf(BytesVisitor)
        }
    }

    /// Serializing a `Value` to a `Value`, and deserializing back, should yield
    /// the same value each time. This is for the case when someone is
    /// serializing from or deserializing into a struct that stores a `Value`
    /// somewhere within.
    #[test_case(Value::Undefined; "undefined")]
    #[test_case(Value::Null; "null")]
    #[test_case(true; "bool_true")]
    #[test_case(false; "bool_false")]
    #[test_case(32; "int")]
    #[test_case(5.67; "float")]
    #[test_case(Value::String("hello".into()); "string")]
    #[test_case(
        vec![Value::Null, Value::Undefined, 32.into(), false.into()]; "array"
    )]
    #[test_case(indexmap! {
        "null".to_owned() => Value::Null,
        "undefined".to_owned() => Value::Undefined,
        "b".to_owned() => 32.into(),
        "c".to_owned() => indexmap! {"d".to_owned() => true.into()}.into()
    }; "object")]
    #[test_case(
        Function::user(
            FunctionDefinition::new(
                [FunctionParameter::identifier("x")],
                FunctionBody::expression(Expression::reference("x")),
            )
                .with_name("func")
                .with_captures(["cap"])
                .into(),
            indexmap! {"cap".to_owned() => 32.into()},
        );
        "function_user"
    )]
    #[test_case(
        // This will do pointer equality to ensure it's the same fn
        Function::native("func".into(), |_, _: Value| 0);
        "function_native"
    )]
    #[test_case(
        Function::bound(
            "func".into(),
            // This will do pointer equality to ensure it's the same fn
            BoundFunction::new(|_, _: Value, _: Value| 0),
            [1, 2, 3].into(),
        );
        "function_bound"
    )]
    fn value_identity(value: impl Into<Value>) {
        let value = value.into();
        let deserialized: Value = from_value(value.clone())
            .unwrap_or_else(|error| panic!("Deserialization failed: {error}"));
        assert_eq!(deserialized, value, "Deserialization did not match");
        let serialized = to_value(&value)
            .unwrap_or_else(|error| panic!("Serialization failed: {error}"));
        assert_eq!(serialized, value, "Serialization did not match");
    }

    /// Serialize/deserialize directly into/from a Function, without the Value
    /// wrapper. This takes different code paths through Serializer/Deserializer
    #[test_case(
        Function::user(
            FunctionDefinition::new(
                [FunctionParameter::identifier("x")],
                FunctionBody::expression(Expression::reference("x")),
            )
                .with_name("func")
                .with_captures(["cap"])
                .into(),
            indexmap! {"cap".to_owned() => 32.into()},
        );
        "user"
    )]
    #[test_case(
        // This will do pointer equality to ensure it's the same fn
        Function::native("func".into(), |_, _: Value| 0);
        "native"
    )]
    #[test_case(
        Function::bound(
            "func".into(),
            // This will do pointer equality to ensure it's the same fn
            BoundFunction::new(|_, _: Value, _: Value| 0),
            [1, 2, 3].into(),
        );
        "bound"
    )]
    fn function_identity(function: Function) {
        let deserialized: Function = from_value(Value::Function(
            function.clone(),
        ))
        .unwrap_or_else(|error| panic!("Deserialization failed: {error}"));
        assert_eq!(deserialized, function, "Deserialization did not match");
        let serialized = to_value(&function)
            .unwrap_or_else(|error| panic!("Serialization failed: {error}"));
        assert_eq!(
            serialized,
            Value::Function(function),
            "Serialization did not match"
        );
    }

    /// Serialize/deserialize between Value and some external types
    #[test_case(true, true.into(); "bool")]
    #[test_case(3, 3.into(); "int")]
    #[test_case(3.5, 3.5.into(); "float")]
    #[test_case("hi".to_owned(), "hi".into(); "string")]
    #[test_case([1, 2, 3], [1, 2, 3].into(); "array")]
    #[test_case(
        indexmap! {"a".to_owned() => 2, "b".to_owned() => 4},
        Object::new().insert("a", 2).insert("b", 4).into();
        "object indexmap"
    )]
    #[cfg_attr(
        feature = "buffer",
        test_case(
           Bytes(vec![0u8, 1u8, 2u8]),
            Value::Buffer([0u8, 1u8, 2u8].as_slice().into());
            "buffer"
        ),
    )]
    #[test_case(None::<&str>, Value::Null; "option none")]
    #[test_case(Some("hi".to_owned()), "hi".into(); "option some")]
    #[test_case(UnitStruct, Value::Undefined; "unit_struct")]
    #[test_case(NewtypeStruct(3), 3.into(); "newtype_struct")]
    #[test_case(
        TupleStruct(3, true),
        Array::new().push(3).push(true).into();
        "tuple_struct"
    )]
    #[test_case(
        FieldStruct {
            s:
            "hi".into(),
            b: true,
            i: 67,
            f: 123.4,
            unit_enum: UnitEnum::A,
        },
        Object::new()
            .insert("s", "hi")
            .insert("b", true)
            .insert("i", 67)
            .insert("f", 123.4)
            .insert("unit_enum", "A")
            .into();
        "field_struct"
    )]
    #[test_case(UnitEnum::A, "A".into(); "unit_enum")]
    #[test_case(TaggedEnum::Unit, "Unit".into(); "tagged_enum unit")]
    #[test_case(
        TaggedEnum::Newtype(3),
        Object::new().insert("Newtype", 3).into();
        "tagged_enum newtype"
    )]
    #[test_case(
        TaggedEnum::Tuple(3, true),
        Object::new()
            .insert("Tuple", Array::new().push(3).push(true))
            .into();
        "tagged_enum tuple"
    )]
    #[test_case(
        TaggedEnum::Struct { b: true },
        Object::new()
            .insert("Struct", Object::new().insert("b", true))
            .into();
        "tagged_enum struct"
    )]
    #[test_case(
        FunctionStruct {
            f: Function::user(
                FunctionDefinition::new([], FunctionBody::expression(3.into()))
                    .with_name("f")
                    .into(),
                Default::default(),
            ),
        },
        Object::new()
            .insert(
                "f",
                // This works because user functions use structural equality
                Function::user(
                    FunctionDefinition::new(
                        [],
                        FunctionBody::expression(3.into()),
                    )
                    .with_name("f")
                    .into(),
                    Default::default(),
                ),
            )
            .into();
        "function_struct"
    )]
    fn external_types_round_trip<'de, T>(external: T, value: Value)
    where
        T: Debug + PartialEq + Serialize + Deserialize<'de>,
    {
        let deserialized: T = from_value(value.clone())
            .unwrap_or_else(|error| panic!("Deserialization failed: {error}"));
        assert_eq!(deserialized, external, "Deserialization did not match");

        let serialized = to_value(&external)
            .unwrap_or_else(|error| panic!("Serialization failed: {error}"));
        assert_eq!(serialized, value, "Serialization did not match");
    }

    /// Test deserialization only of certain external values. There are some
    /// values that we expect to be able to deserialize from, but not serialize
    /// back to. E.g. unit values can deserialize from `null` OR `undefined`,
    /// but only serialize back to the latter
    #[test_case(
        // By default unit variants are serialized as just "Variant", but we
        // will also accept {Variant: undefined}
        TaggedEnum::Unit,
        Object::new().insert("Unit", Value::Undefined).into();
        "tagged_enum unit undefined"
    )]
    fn external_types_deserialize<T>(external: T, value: Value)
    where
        T: Debug + PartialEq + DeserializeOwned,
    {
        let deserialized: T = from_value(value.clone())
            .unwrap_or_else(|error| panic!("Deserialization failed: {error}"));
        assert_eq!(deserialized, external, "Deserialization did not match");
    }
}
