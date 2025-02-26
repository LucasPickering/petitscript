//! Code execution and evaluation

mod eval;
mod exec;
mod state;

use crate::{
    compile::Program,
    execute::{exec::Execute, state::ThreadState},
    function::Function,
    scope::Scope,
    value::{Exports, Value},
    Error,
};
use std::{
    any::{self, Any, TypeId},
    collections::{hash_map::Entry, HashMap},
    sync::Arc,
};

/// TODO
#[derive(Clone, Debug)]
pub struct Process {
    /// The program we'll be executing
    program: Arc<Program>,
    /// Global values available to the program, such as the stdlib and native
    /// functions provided by the user
    globals: Scope,
    /// Arbitrary user-defined data attached to this process
    app_data: AppData,
}

impl Process {
    /// TODO
    pub(super) fn new(globals: Scope, program: Program) -> Self {
        Self {
            program: program.into(),
            globals,
            app_data: AppData::default(),
        }
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
        // Exporting is available here because we're in the root scope
        let mut thread_state =
            ThreadState::new(self.globals.clone(), self, true);
        self.program
            .ast()
            .exec(&mut thread_state)
            .map_err(|error| thread_state.qualify_error(error))?;
        Ok(thread_state.into_exports().unwrap())
    }

    /// Call a function that originated from this process
    pub fn call(
        &self,
        function: &Function,
        args: &[Value],
    ) -> Result<Value, Error> {
        if function.id().program_id() != self.program.id() {
            todo!("error")
        }

        // Exporting is NOT allowed here, because we're not in the root scope
        let mut thread_state =
            ThreadState::new(self.globals.clone(), self, false);
        function
            .call(&mut thread_state, args)
            .map_err(|error| thread_state.qualify_error(error))
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
