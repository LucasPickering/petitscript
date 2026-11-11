//! Code execution and evaluation

mod eval;
mod exec;
mod scope;
mod state;

pub use scope::{GlobalEnvironment, Prototype};

#[cfg(feature = "serde")]
use crate::error::ValueError;
use crate::{
    Error, Source,
    compile::Program,
    error::RuntimeError,
    execute::{
        exec::Execute,
        state::{CallSite, ThreadState},
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

/// TODO rename this - a process implies it's running, which is not true. It
/// only runs on demand.
#[derive(Clone, Debug)]
pub struct Process {
    /// The program we'll be executing
    program: Arc<Program>,
    /// A clone of the module registry from the engine
    ///
    /// This clone should be relatively cheap because all the values within
    /// each export are reference counted. Cloning will snapshot the modules at
    /// the time of the process being spawned, allowing the engine to register
    /// new modules as needed without interfering with running modules.
    ///
    /// TODO hang onto the Engine instead? it's immutable
    modules: IndexMap<NativeModuleName, Exports>,
    /// Global values available to the program, i.e. the stdlib
    globals: Arc<GlobalEnvironment>,
    /// Arbitrary user-defined data attached to this process
    app_data: AppData,
}

#[cfg(test)]
static_assertions::assert_impl_all!(Process: Send, Sync);

impl Process {
    /// TODO
    pub(super) fn new(
        modules: IndexMap<NativeModuleName, Exports>,
        globals: Arc<GlobalEnvironment>,
        program: Program,
    ) -> Self {
        Self {
            program: program.into(),
            modules,
            globals,
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

    /// Get user-defined app data attached to the process
    ///
    /// App data is stored and fetched by type, meaning **there can only be one
    /// piece of app data per type**. Return [Error::UnknownAppData] if there is
    /// no app data of type `T`. See [set_app_data](Self::set_app_data).
    pub fn app_data<T: Any + Send + Sync>(&self) -> Result<&T, Error> {
        self.app_data.get()
    }

    /// Attach arbitrary data of a statically known type to this process. This
    /// data will be accessible to all subsequent executions of this process.
    /// This makes it easy to pass data to native functions.
    ///
    /// App data is keyed by its type, meaning **only one instance of any given
    /// type can be stored.**
    ///
    /// ## Errors
    ///
    /// Return [Error::DuplicateAppData] if there is already app data of type
    /// `T` registered.
    pub fn set_app_data<T: Any + Send + Sync>(
        &mut self,
        data: T,
    ) -> Result<(), Error> {
        self.app_data.set(data)
    }

    /// Execute the loaded program and return its exported values
    pub fn execute(&self) -> Result<Exports, Error> {
        let mut thread_state = self.create_thread();
        self.program.module().exec(&mut thread_state)?;
        // Exporting is available here because we're in the root scope
        Ok(thread_state.into_exports().unwrap())
    }

    /// Call a function that originated from this process
    ///
    /// TODO what happens if you pass it a function from another process?
    pub fn call(
        &self,
        function: &Function,
        arguments: Vec<Value>,
    ) -> Result<Value, Error> {
        // Exporting is NOT allowed here, because we're not in the root scope
        let mut thread_state =
            ThreadState::new(Arc::clone(&self.globals), self, false);
        function
            .call(&mut thread_state, CallSite::Native, arguments)
            .map_err(Error::from)
    }

    /// Create a new thread to evaluate a module
    fn create_thread(&self) -> ThreadState<'_> {
        ThreadState::new(self.globals.clone(), self, true)
    }

    /// Look up a native module by name
    fn native_module(
        &self,
        name: &NativeModuleName,
    ) -> Result<&Exports, RuntimeError> {
        self.modules
            .get(name)
            .ok_or_else(|| RuntimeError::UnknownModule { name: name.clone() })
    }
}

/// Values exported from a module
///
/// This is the output of executing a process.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Exports {
    /// Default exported value
    pub default: Option<Value>,
    /// Named exported values
    pub named: IndexMap<String, Value>,
}

impl Exports {
    /// Create a new empty set of exports
    pub fn new() -> Self {
        Self::default()
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
        let named = exports
            .into_iter()
            .map(|(name, value)| (name.into(), value.into()))
            .collect();
        Self {
            named,
            default: None,
        }
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
    /// The given function must take two arguments: `(process, args)`. `process`
    /// is the [Process] executing the function, and `args` is a list of
    /// arguments passed by the caller.
    ///
    /// `args` must implement [FromPetitArgs], which is implemented for tuples
    /// of individual values implementing [FromPetit](crate::value::FromPetit).
    ///
    /// The return value must implement [IntoPetitResult]. This can be any value
    /// that implements [IntoPetit](crate::value::IntoPetit), or a `Result`
    /// where `T` implements [IntoPetit](crate::value::IntoPetit) and `E`
    /// implements `Into` for [RuntimeError].
    ///
    /// ```
    /// use petitscript::{Exports, Value, error::ValueError, value::Number};
    ///
    /// let mut module = Exports::default();
    /// // All functions take two arguments: process and args. Use tuples to
    /// // unpack the args
    /// module.export_fn("args0", |_, ()| Value::Null);
    /// module.export_fn("args1", |_, (arg,): (Value,)| arg);
    /// module.export_fn(
    ///     "args2",
    ///     |_, (arg0, arg1)| -> [Value; 2] { [arg0, arg1] },
    /// );
    ///
    /// // You can restrict the arg and return type:
    /// module.export_fn("add", |_, (a, b): (Number, Number)| a + b);
    ///
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
    ///     }
    /// );
    /// ```
    pub fn export_fn<F, Args, Out>(
        &mut self,
        name: impl Into<String>,
        function: F,
    ) where
        F: 'static + Fn(&Process, Args) -> Out + Send + Sync,
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

/// Arbitrary data that a user can attach to a process. Multiple pieces of data
/// can be attached, but data is retrieved by its type, meaning **only one entry
/// can existing for any type**.
#[derive(Clone, Debug, Default)]
struct AppData(
    /// Inner arc is necessary so app data can be shared when a process is
    /// cloned
    HashMap<TypeId, Arc<dyn Any + Send + Sync>>,
);

impl AppData {
    fn get<T: Any + Send + Sync>(&self) -> Result<&T, Error> {
        // TODO return a ref from this instead once we eliminate the outer arc
        let data = self.0.get(&TypeId::of::<T>()).ok_or_else(|| {
            Error::UnknownAppData {
                type_name: any::type_name::<T>(),
            }
        })?;
        // Downcast can never fail because the key and value are derived from
        // the same original type T
        Ok(data.downcast_ref().expect("Incorrect type for app data"))
    }

    fn set<T: Any + Send + Sync>(&mut self, data: T) -> Result<(), Error> {
        match self.0.entry(TypeId::of::<T>()) {
            Entry::Occupied(_) => Err(Error::DuplicateAppData {
                type_name: any::type_name::<T>(),
            }),
            Entry::Vacant(entry) => {
                entry.insert(Arc::new(data));
                Ok(())
            }
        }
    }
}
