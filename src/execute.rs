//! Code execution and evaluation

mod eval;
mod exec;
mod scope;
mod state;

pub use scope::{GlobalEnvironment, Prototype};

#[cfg(feature = "serde")]
use crate::error::ValueError;
use crate::{
    Engine, Source,
    compile::Program,
    error::{DuplicateAppData, RuntimeError, RuntimeErrorKind, UnknownAppData},
    execute::{
        exec::Execute,
        state::{Fiber, Location},
    },
    name::NativeModuleName,
    value::{FromPetitArgs, IntoPetitResult, Value, function::Function},
};
use indexmap::IndexMap;
use std::{
    any::{self, Any, TypeId},
    collections::{HashMap, hash_map::Entry},
    fmt::{self, Display},
    sync::Arc,
};

/// Instantiated [`Program`] that can be executed
///
/// TODO
#[derive(Clone, Debug)]
pub struct World {
    /// Engine to execute under
    ///
    /// This provides globals and native modules.
    engine: Engine,
    /// Program we'll be executing
    program: Arc<Program>,
    /// Arbitrary user-defined data attached to this world
    app_data: AppData,
}

#[cfg(test)]
static_assertions::assert_impl_all!(World: Send, Sync);

impl World {
    /// TODO
    pub(crate) fn new(engine: Engine, program: Program) -> Self {
        Self {
            engine,
            program: program.into(),
            app_data: AppData::default(),
        }
    }

    /// Get an iterator over all sources in the program
    ///
    /// The first will be the root source passed to the compiler, and subsequent
    /// items will be other imported modules. This will exclude native modules,
    /// as they have no source code.
    pub fn sources(&self) -> impl Iterator<Item = &Source> {
        self.program.sources().iter()
    }

    /// Get user-defined app data attached to the world
    ///
    /// App data is stored and fetched by type, meaning **there can only be one
    /// piece of app data per type**. See [set_app_data](Self::set_app_data) for
    /// examples.
    ///
    /// # Errors
    ///
    /// Return [`UnknownAppData`] if there is no app data of type `T`.
    pub fn app_data<T: Any + Send + Sync>(&self) -> Result<&T, UnknownAppData> {
        self.app_data.get()
    }

    /// Attach arbitrary data of a statically known type to this world
    ///
    /// This data will be accessible to all subsequent executions of this
    /// world. This makes it easy to pass data to native functions.
    ///
    /// App data is keyed by its type, meaning **only one instance of any given
    /// type can be stored.**
    ///
    /// TODO example code
    ///
    /// # Errors
    ///
    /// Return [`DuplicateAppData`] if there is already app data of type
    /// `T` registered.
    pub fn set_app_data<T: Any + Send + Sync>(
        &mut self,
        data: T,
    ) -> Result<(), DuplicateAppData> {
        self.app_data.set(data)
    }

    /// Execute the loaded program and return its exported values
    ///
    /// # Errors
    ///
    /// TODO
    pub fn run(&self) -> Result<Exports, RuntimeError> {
        let mut fiber = self.create_fiber();
        self.program.module().exec(&mut fiber)?;
        // Exporting is available here because we're in the root scope
        Ok(fiber.into_exports().unwrap())
    }

    /// Call a function that originated from this world
    ///
    /// TODO what happens if you pass it a function from another world?
    ///
    /// # Errors
    ///
    /// TODO
    pub fn call(
        &self,
        function: &Function,
        arguments: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // Exporting is NOT allowed here, because we're not in the root scope
        let mut fiber =
            Fiber::new(Arc::clone(&self.engine.globals), self, false);
        function.call(&mut fiber, Location::Native, arguments)
    }

    /// Create a new fiber to execute a module
    fn create_fiber(&self) -> Fiber<'_> {
        Fiber::new(self.engine.globals.clone(), self, true)
    }

    /// Look up a native module by name
    fn get_native_module(
        &self,
        name: &NativeModuleName,
    ) -> Result<&Exports, RuntimeErrorKind> {
        self.engine.modules.get(name).ok_or_else(|| {
            RuntimeErrorKind::UnknownModule { name: name.clone() }
        })
    }
}

/// Values exported from a module
///
/// This is the output of executing a world.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Exports {
    /// Default exported value
    pub default: Option<Value>,
    /// Named exported values
    pub named: IndexMap<String, Value>,
}

impl Exports {
    /// Create a set of exports with an optional default and a set of named
    /// values
    pub fn new(
        default: Option<impl Into<Value>>,
        named: impl IntoIterator<Item = (impl Into<String>, impl Into<Value>)>,
    ) -> Self {
        let named = named
            .into_iter()
            .map(|(name, value)| (name.into(), value.into()))
            .collect();
        Self {
            default: default.map(Into::into),
            named,
        }
    }

    /// Export a some values as named exports. This allows the exports to be
    /// imported as a native module like so:
    ///
    /// ```notrust
    /// import { add } from "module";
    ///
    /// add(1, 2);
    /// ```
    pub fn named<K, V>(exports: impl IntoIterator<Item = (K, V)>) -> Self
    where
        K: Into<String>,
        V: Into<Value>,
    {
        Self::new(None::<Value>, exports)
    }

    /// TODO
    #[cfg(feature = "serde")]
    pub fn get_deserialize<'de, T>(&self, name: &str) -> Result<T, ValueError>
    where
        T: serde::Deserialize<'de>,
    {
        let value = self.named.get(name).expect("TODO error or return None?");
        crate::serde::from_value(value.clone())
    }

    /// Create a native function and export it with the given name
    ///
    /// The given function must take two arguments: `(world, args)`. `world`
    /// is the [`World`] executing the function, and `args` is a list of
    /// arguments passed by the caller.
    ///
    /// `args` must implement [`FromPetitArgs`], which is implemented for tuples
    /// of individual values implementing
    /// [`FromPetit`](crate::value::FromPetit).
    ///
    /// The return value must implement [`IntoPetitResult`]. This can be any
    /// value that implements [`IntoPetit`](crate::value::IntoPetit), or a
    /// `Result` where `T` implements [`IntoPetit`](crate::value::IntoPetit)
    /// and `E` implements `Into` for [`RuntimeError`].
    ///
    /// ```
    /// use petitscript::{Exports, Value, error::ValueError, value::Number};
    ///
    /// let mut module = Exports::default();
    /// // All functions take two arguments: world and args. Use tuples to
    /// // unpack the args
    /// module.export_fn("args0", |_, ()| Value::Null);
    /// module.export_fn("args1", |_, (arg,): (Value,)| arg);
    /// module.export_fn("args2", |_, (arg0, arg1)| -> [Value; 2] { [arg0, arg1] });
    ///
    /// // You can restrict the arg and return type:
    /// module.export_fn("add", |_, (a, b): (Number, Number)| a + b);
    ///
    /// // You can return errors:
    /// module.export_fn(
    ///     "fallible",
    ///     |_, (input,): (Number,)| -> Result<Number, ValueError> {
    ///         if input < 10.into() {
    ///             Ok(input)
    ///         } else {
    ///             Err(ValueError::Other("error!".into()))
    ///         }
    ///     },
    /// );
    /// ```
    pub fn export_fn<F, Args, Out>(
        &mut self,
        name: impl Into<String>,
        function: F,
    ) where
        F: 'static + Fn(&World, Args) -> Out + Send + Sync,
        Args: FromPetitArgs,
        Out: IntoPetitResult,
    {
        // Use the naem as the and also attach it to the function for printing
        let name = name.into();
        self.named
            .insert(name.clone(), Function::native(name, function).into());
    }
}

impl Display for Exports {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(default) = &self.default {
            writeln!(f, "(default): {default}")?;
        }
        for (name, value) in &self.named {
            writeln!(f, "{name}: {value}")?;
        }
        Ok(())
    }
}

/// Arbitrary data that a user can attach to a world
///
/// Multiple pieces of data can be attached, but data is retrieved by its type,
/// meaning **only one entry can existing for any type**.
#[derive(Clone, Debug, Default)]
struct AppData(
    /// Inner arc is necessary so app data can be shared when a world is cloned
    HashMap<TypeId, Arc<dyn Any + Send + Sync>>,
);

impl AppData {
    fn get<T: Any + Send + Sync>(&self) -> Result<&T, UnknownAppData> {
        let data =
            self.0
                .get(&TypeId::of::<T>())
                .ok_or_else(|| UnknownAppData {
                    type_name: any::type_name::<T>(),
                })?;
        // Downcast can never fail because the key and value are derived from
        // the same original type T
        Ok(data.downcast_ref().expect("Incorrect type for app data"))
    }

    fn set<T: Any + Send + Sync>(
        &mut self,
        data: T,
    ) -> Result<(), DuplicateAppData> {
        match self.0.entry(TypeId::of::<T>()) {
            Entry::Occupied(_) => Err(DuplicateAppData {
                type_name: any::type_name::<T>(),
            }),
            Entry::Vacant(entry) => {
                entry.insert(Arc::new(data));
                Ok(())
            }
        }
    }
}
