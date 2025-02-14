//! Code execution and evaluation

mod eval;
mod exec;
mod state;

use crate::{
    ast::Program,
    error::RuntimeResult,
    execute::{exec::Execute, state::ThreadState},
    scope::Scope,
    value::{Exports, Value},
    Function, RuntimeError,
};
use std::{
    any::{self, Any, TypeId},
    collections::{hash_map::Entry, HashMap},
    sync::Arc,
};

/// TODO
/// TODO rename
#[derive(Debug)]
pub struct Process {
    /// The program we'll be executing
    program: Program,
    /// Global values available to the program, such as the stdlib
    globals: Scope,
    /// Arbitrary user-defined data attached to this process
    app_data: AppData,
}

impl Process {
    /// TODO
    pub(super) fn new(globals: Scope, program: Program) -> Self {
        Self {
            program,
            globals,
            app_data: AppData::default(),
        }
    }

    /// Get a piece of app data attached to the process, downcasted to a static
    /// type. Return an error if there is no app data of the requested type.
    pub fn app_data<T: Any + Send + Sync>(&self) -> RuntimeResult<&T> {
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
    ) -> RuntimeResult<()> {
        self.app_data.set(data)
    }

    /// Execute the loaded program and return its exported values
    pub async fn execute(&self) -> RuntimeResult<Exports> {
        // Exporting is available here because we're in the root scope
        let mut thread_state =
            ThreadState::new(self.globals.clone(), self, true);
        self.program.statements.exec(&mut thread_state).await?;
        Ok(thread_state.into_exports().unwrap())
    }

    /// Call a function that originated from this process
    pub async fn call(
        &self,
        function: &Function,
        args: &[Value],
    ) -> RuntimeResult<Value> {
        // Exporting is NOT allowed here, because we're not in the root scope
        let mut thread_state =
            ThreadState::new(self.globals.clone(), self, false);
        function.call(&mut thread_state, args).await
    }
}

/// Arbitrary data that a user can attach to a process. Multiple pieces of data
/// can be attached, but data is retrieved by its type, meaning **only one entry
/// can existing for any type**.
#[derive(Clone, Debug, Default)]
pub struct AppData(
    /// Inner arc is necessary so app data can be shared when a process is
    /// cloned
    HashMap<TypeId, Arc<dyn Any + Send + Sync>>,
);

impl AppData {
    fn get<T: Any + Send + Sync>(&self) -> RuntimeResult<&T> {
        // TODO return a ref from this instead once we eliminate the outer arc
        let data = self.0.get(&TypeId::of::<T>()).ok_or_else(|| {
            RuntimeError::UnknownAppData {
                type_name: any::type_name::<T>(),
            }
        })?;
        // Downcast can never fail because the key and value are derived from
        // the same original type T
        Ok(data.downcast_ref().expect("Incorrect type for app data"))
    }

    fn set<T: Any + Send + Sync>(&mut self, data: T) -> RuntimeResult<()> {
        match self.0.entry(TypeId::of::<T>()) {
            Entry::Occupied(_) => Err(RuntimeError::DuplicateAppData {
                type_name: any::type_name::<T>(),
            }),
            Entry::Vacant(entry) => {
                entry.insert(Arc::new(data));
                Ok(())
            }
        }
    }
}
