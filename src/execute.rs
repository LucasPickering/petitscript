//! Code execution and evaluation

mod eval;
mod exec;
mod scope;
mod state;

pub use scope::{GlobalEnvironment, Prototype};

use crate::{
    ast::NativeModuleName,
    compile::Program,
    error::RuntimeError,
    execute::{
        exec::Execute,
        state::{CallSite, ThreadState},
    },
    value::{
        function::{Function, FunctionInner},
        Object, Value,
    },
    Error, NativeFunctionTable,
};
use indexmap::IndexMap;
use std::{
    any::{self, Any, TypeId},
    collections::{hash_map::Entry, HashMap},
    fmt::{self, Display},
    sync::{atomic::AtomicU32, Arc},
};

/// TODO
#[derive(Clone, Debug)]
pub struct Process {
    /// TODO
    id: ProcessId,
    /// The program we'll be executing
    program: Arc<Program>,
    /// A clone of the module registry from the engine. This clone should be
    /// relatively cheap because all the values within each export are
    /// reference counted. Cloning snapshots the modules at the time of the
    /// process being spawned, allowing the engine to register new modules as
    /// needed without interfering with running modules.
    modules: IndexMap<NativeModuleName, Exports>,
    /// Intern pool of native function definitions. This is read-only; native
    /// functions can only be registered in the engine. Once a process is
    /// spawned, new native functions cannot be added to it. Anything added to
    /// the engine after spawning will not be reflected here.
    native_functions: NativeFunctionTable,
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
        native_functions: NativeFunctionTable,
        globals: Arc<GlobalEnvironment>,
        program: Program,
    ) -> Self {
        // Within a single OS process, each process ID will be unique
        static NEXT_ID: AtomicU32 = AtomicU32::new(0);
        let id = ProcessId(
            NEXT_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
        );

        Self {
            id,
            program: program.into(),
            modules,
            native_functions,
            globals,
            app_data: AppData::default(),
        }
    }

    fn id(&self) -> ProcessId {
        self.id
    }

    /// Get a piece of app data attached to the process, downcasted to a static
    /// type. Return an error if there is no app data of the requested type. See
    /// [Self::set_app_data] to attach app data to this process.
    pub fn app_data<T: Any + Send + Sync>(&self) -> Result<&T, Error> {
        self.app_data.get()
    }

    /// Attach arbitrary data of a statically known type to this process. This
    /// data will be accessible to all subsequent executions of this process.
    /// This makes it easy to pass data to native functions.
    ///
    /// App data is keyed by its type, meaning **only one instance of any given
    /// type can be stored.**
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
    pub fn call(
        &self,
        function: &Function,
        arguments: &[Value],
    ) -> Result<Value, Error> {
        // If this is a user function, make sure it belongs to this process.
        // Native functions don't capture any values so they don't need this
        if let FunctionInner::User { id, .. } = &*function.0 {
            if id.process_id != self.id {
                todo!("error process ID mismatch")
            }
        }

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

/// TODO
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct ProcessId(pub u32);

/// Values exported from a module. This is the output of loading a module.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Exports {
    /// Default exported value
    pub default: Option<Value>,
    /// Named exported values
    pub named: IndexMap<String, Value>,
}

impl Exports {
    /// Export a module as named exports. This allows it to be imported like so:
    ///
    /// ```notrust
    /// import { add } from "module";
    ///
    /// add(1, 2);
    /// ```
    pub fn named<K, V>(exports: impl IntoIterator<Item = (K, V)>) -> Self
    where
        K: ToString,
        V: Into<Value>,
    {
        let named = exports
            .into_iter()
            .map(|(name, value)| (name.to_string(), value.into()))
            .collect();
        Self {
            named,
            default: None,
        }
    }

    /// Export a module as both named exports and a default object of those
    /// names. This allows a module to be used with both named and default
    /// imports, like so:
    ///
    /// ```notrust
    /// import { add } from "module";
    /// import module from "module";
    ///
    /// add(1, 2);
    /// module.add(1, 2);
    /// ```
    pub fn named_and_default<K, V>(
        exports: impl IntoIterator<Item = (K, V)>,
    ) -> Self
    where
        K: ToString,
        V: Into<Value>,
    {
        let mut default = Object::new();
        let mut named = IndexMap::new();
        for (name, value) in exports {
            let name = name.to_string();
            let value: Value = value.into();
            named.insert(name.clone(), value.clone());
            // Since we're the only owner, this will insert without cloning
            default = default.insert(name, value);
        }
        Self {
            named,
            default: Some(default.into()),
        }
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
