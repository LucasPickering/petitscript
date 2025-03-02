//! Custom implementations of `Serialize` and `Deserialize`. This is in its own
//! module so the implementations don't clutter the main value modules.

use crate::{compile::FunctionId, function::Function, scope::Bindings};
use serde::{
    de::{self, MapAccess, Visitor},
    ser::SerializeStruct,
    Deserialize, Serialize,
};
use std::fmt;

impl Function {
    /// TODO
    pub(super) const SERDE_NAME: &'static str = "__Function";
    pub(super) const FIELD_ID: &'static str = "id";
    pub(super) const FIELD_NAME: &'static str = "name";
    pub(super) const FIELD_CAPTURES: &'static str = "captures";
    pub(super) const ALL_FIELDS: &'static [&'static str] =
        &[Self::FIELD_ID, Self::FIELD_NAME, Self::FIELD_CAPTURES];
}

// TODO compare these to the derived impls

impl Serialize for Function {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // TODO explain
        let mut strct = serializer.serialize_struct(Self::SERDE_NAME, 3)?;
        strct.serialize_field(Self::FIELD_ID, &self.id())?;
        strct.serialize_field(Self::FIELD_NAME, &self.name())?;
        strct.serialize_field(Self::FIELD_CAPTURES, self.captures())?;
        strct.end()
    }
}

impl<'de> Deserialize<'de> for Function {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct FunctionVisitor {
            id: Field<FunctionId>,
            name: Field<Option<String>>,
            captures: Field<Bindings>,
        }

        impl<'v> Visitor<'v> for FunctionVisitor {
            type Value = Function;

            fn expecting(&self, f: &mut fmt::Formatter) -> std::fmt::Result {
                write!(f, "struct with the fields {:?}", Function::ALL_FIELDS)
            }

            fn visit_map<A>(
                mut self,
                mut map: A,
            ) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'v>,
            {
                while let Some(key) = map.next_key()? {
                    match key {
                        Function::FIELD_ID => self.id.set(&mut map)?,
                        Function::FIELD_NAME => self.name.set(&mut map)?,
                        Function::FIELD_CAPTURES => {
                            self.captures.set(&mut map)?
                        }
                        _ => {
                            return Err(de::Error::unknown_field(
                                key,
                                Function::ALL_FIELDS,
                            ));
                        }
                    }
                }
                // If any of the fields weren't specified, we'll fail here
                Ok(Function::new(
                    self.id.get()?,
                    self.name.get()?,
                    self.captures.get()?,
                ))
            }
        }

        // Functions can only be deserialized directly from function values,
        // since they need to be bound to an active process. Tell the
        // deserializer we'll only accept an exact struct match
        deserializer.deserialize_struct(
            Self::SERDE_NAME,
            Self::ALL_FIELDS,
            FunctionVisitor {
                id: Field::new(Function::FIELD_ID),
                name: Field::new(Function::FIELD_NAME),
                captures: Field::new(Function::FIELD_CAPTURES),
            },
        )
    }
}

/// TODO
struct Field<T> {
    name: &'static str,
    value: Option<T>,
}

impl<T> Field<T> {
    fn new(name: &'static str) -> Self {
        Self { name, value: None }
    }

    fn set<'de, A>(&mut self, map: &mut A) -> Result<(), A::Error>
    where
        T: Deserialize<'de>,
        A: MapAccess<'de>,
    {
        if self.value.is_some() {
            return Err(de::Error::duplicate_field(Function::FIELD_ID));
        }
        self.value = Some(map.next_value()?);
        Ok(())
    }

    fn get<E: de::Error>(self) -> Result<T, E> {
        self.value
            .ok_or_else(|| de::Error::missing_field(self.name))
    }
}
