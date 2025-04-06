use crate::{
    compile::FunctionDefinitionId, error::RuntimeError, execute::ProcessId,
    value::Value, FromPs, IntoPs, Process,
};
use indexmap::IndexMap;
use std::{
    fmt::{self, Debug, Display},
    sync::Arc,
};

/// An executable function, bound to a specific program. All functions in
/// PetitScript are closures, meaning they capture their environment when
/// created, and references to outside variables may be used within the function
/// body.
///
/// TODO should we add refcounting somewhere in here? So we don't have to clone
/// the name/captures
#[derive(Clone, Debug, PartialEq)]
pub struct Function(pub(crate) FunctionInner);

impl Function {
    /// Create a new function. A "user" function is a function defined in
    /// PetitScript, as opposed to a "native" function that's defined in Rust.
    pub(crate) fn user(
        id: UserFunctionId,
        name: Option<String>,
        captures: Captures,
    ) -> Self {
        Self(FunctionInner::User { id, name, captures })
    }

    /// Create a new native function. The function must have been predefined in
    /// the engine. This is just a "pointer" to the function definition.
    pub(crate) fn native(id: NativeFunctionId, name: Option<String>) -> Self {
        Self(FunctionInner::Native { id, name })
    }

    pub(crate) fn id(&self) -> FunctionId {
        match &self.0 {
            FunctionInner::User { id, .. } => FunctionId::User(*id),
            FunctionInner::Native { id, .. } => FunctionId::Native(*id),
        }
    }

    /// TODO
    pub fn name(&self) -> Option<&str> {
        match &self.0 {
            FunctionInner::User { name, .. } => name.as_deref(),
            FunctionInner::Native { name, .. } => name.as_deref(),
        }
    }

    pub fn set_name(&mut self, name: String) {
        match &mut self.0 {
            FunctionInner::User { name: n, .. } => *n = Some(name),
            FunctionInner::Native { name: n, .. } => *n = Some(name),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let kind = match &self.0 {
            FunctionInner::User { .. } => "user",
            FunctionInner::Native { .. } => "native",
        };
        write!(
            f,
            "[Function: {} ({kind})]",
            self.name().unwrap_or("(anonymous)")
        )
    }
}

/// The implementation of a function, which is hidden from the external API
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum FunctionInner {
    /// A function defined in PetitScript. User functions must be defined in
    /// code and their definnitions interned at the program level during
    /// the compilation process. Once a function _value_ is created, that
    /// function can only be called within the process in which it was
    /// created.
    User {
        /// TODO
        id: UserFunctionId,
        /// The name is also contained in the function definition that the ID
        /// points to, but we duplicate it here for easy access during
        /// printing/debugging
        name: Option<String>,
        /// All external bound values captured by this function. It would be
        /// much easier to just store a pointer to the parent's scope, but we
        /// need functions to be serializable so we have to store the raw
        /// values instead. This must contain a reference to every captured
        /// identifier in the function definition; if an identifier is
        /// referenced in the function but not available in the parent
        /// scope, it won't be captured in either the definition or here. In
        /// that case, it must either be provided by the global scope or we'll
        /// hit a reference error.
        captures: Captures,
    },
    /// A function defined in Rust. Native function definitions are interned at
    /// the engine level. A native function _value_ can be called within any
    /// process owned by the engine that contains the native function.
    Native {
        /// TODO
        id: NativeFunctionId,
        /// Display name for this function. This may not be available when the
        /// function is created, but it should be set once the function is
        /// bound to a value. This is for labelling purposes only; it does not
        /// affect program behavior beyond its `toString()` output.
        name: Option<String>,
    },
}

/// A unique ID for a user OR native function. This is unique only within the
/// engine that the function is defined and executed.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) enum FunctionId {
    User(UserFunctionId),
    Native(NativeFunctionId),
}

/// A unique ID for a user function. This is unique only within the engine that
/// the function is defined and executed.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct UserFunctionId {
    pub process_id: ProcessId,
    pub definition_id: FunctionDefinitionId,
}

/// A set of captured values for a closure
pub(crate) type Captures = IndexMap<String, Value>;

/// A pool of interned native function definitions. Each engine gets one pool.
/// When a process is spawned, the pool is cloned so that subsequent additions
/// to the pool are _not_ reflected in existing processes. The function
/// definitions are wrapped
#[derive(Clone, Debug, Default)]
pub(crate) struct NativeFunctionTable(Vec<NativeFunctionDefinition>);

impl NativeFunctionTable {
    /// Look up a function definition by its ID. This is analagous to
    /// derefencing a function pointer into the .text section
    pub fn get(
        &self,
        id: NativeFunctionId,
    ) -> Result<&NativeFunctionDefinition, RuntimeError> {
        self.0.get(id.0 as usize).ok_or_else(|| todo!())
    }

    /// Add a Rust native function definition to the table and return a pointer
    /// to it
    pub fn create_fn<F, Args, Out>(&mut self, function: F) -> Function
    where
        F: 'static + Fn(&Process, Args) -> Out + Send + Sync,
        Args: FromPsArgs,
        Out: IntoPsResult,
    {
        let id = NativeFunctionId(self.0.len() as u64);
        self.0.push(NativeFunctionDefinition::new(function));
        Function::native(id, None)
    }
}

/// TODO
/// TODO namespace this by engine
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct NativeFunctionId(pub u64);

/// TODO
#[derive(Clone)]
pub(crate) struct NativeFunctionDefinition(
    #[allow(clippy::type_complexity)]
    Arc<
        dyn Fn(&Process, &[Value]) -> Result<Value, RuntimeError> + Send + Sync,
    >,
);

impl NativeFunctionDefinition {
    /// TODO
    pub fn new<F, Args, Out>(f: F) -> Self
    where
        F: 'static + Fn(&Process, Args) -> Out + Send + Sync,
        Args: FromPsArgs,
        Out: IntoPsResult,
    {
        // Wrap the lambda with logic to convert input/output/error, and box it
        let function = move |process: &Process, args: &[Value]| {
            // TODO add error context
            let args = Args::from_ps_args(args)?;
            let output = f(process, args).into_ps_result()?;
            output.into_ps().map_err(RuntimeError::Value)
        };
        Self(Arc::new(function))
    }

    /// Call this function
    pub(crate) fn call(
        &self,
        process: &Process,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        (self.0)(process, args)
    }
}

impl Debug for NativeFunctionDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("NativeFunction").field(&"..").finish()
    }
}

impl PartialEq for NativeFunctionDefinition {
    fn eq(&self, other: &Self) -> bool {
        // If we point to the same function, we're the same
        Arc::ptr_eq(&self.0, &other.0)
    }
}

/// TODO
pub struct Varargs(pub Vec<Value>);

/// TODO
pub trait FromPsArgs: Sized {
    fn from_ps_args(args: &[Value]) -> Result<Self, RuntimeError>;
}

/// A recursive macro to pull a static number of arguments out of the arg array,
/// convert each one according to its FromPs impl, then pass them all to a
/// function
macro_rules! call_fn {
    // Entrypoint - pass a function you want called, the array of arguments to
    // convert, and a list of the static types of each argument
    ($args:expr, ($($arg_types:ident,)*)) => {
        call_fn!(@step $args, 0usize, (), ($($arg_types,)*))
    };
    // Recursive step - Pop the next arg type off the front of a list, then
    // generate an expression to pull the corresponding arg out of the array
    // and convert it
    (@step
        $args:expr, // Untyped arg array
        $index:expr, // Index of the *next* arg to convert
        ($($acc:expr,)*), // Args that have been converted so far
        // Types of args that have yet to be converted
        ($first:ident, $($rest:ident,)*)
    ) => {
        call_fn!(@step
            $args,
            $index + 1,
            ($($acc,)* get_arg($args, $index)?,),
            ($($rest,)*)
        )
    };
    // Base case - all args have been converted. Call the function with all our
    // arg expressions
    (@step $args:expr, $_index:expr, ($($acc:expr,)*), ()) => {
        ($($acc,)*)
    };
}

/// Generate an implementation of FromPsArgs for a fixed number of arguments
macro_rules! impl_from_ps_args {
    ($($arg_types:ident),*) => {
        impl<'a, $($arg_types,)*> FromPsArgs for ($($arg_types,)*)
            where $($arg_types: FromPs,)*
        {
            fn from_ps_args(args: &[Value]) -> Result<Self, RuntimeError> {
                Ok(call_fn!(args, ($($arg_types,)*)))
            }
        }
    };
}

impl FromPsArgs for () {
    fn from_ps_args(_: &[Value]) -> Result<Self, RuntimeError> {
        Ok(())
    }
}

/// TODO
impl FromPsArgs for Varargs {
    fn from_ps_args(values: &[Value]) -> Result<Self, RuntimeError> {
        // TODO remove clones
        Ok(Self(values.to_owned()))
    }
}

/// Special case implementation: a single argument doesn't need a tuple wrapper
impl<T0: FromPs> FromPsArgs for T0 {
    fn from_ps_args(args: &[Value]) -> Result<Self, RuntimeError> {
        let arg0 = get_arg(args, 0)?;
        Ok(arg0)
    }
}

impl_from_ps_args!(T0);
impl_from_ps_args!(T0, T1);
impl_from_ps_args!(T0, T1, T2);
impl_from_ps_args!(T0, T1, T2, T3);
impl_from_ps_args!(T0, T1, T2, T3, T4);
impl_from_ps_args!(T0, T1, T2, T3, T4, T5);
impl_from_ps_args!(T0, T1, T2, T3, T4, T5, T6);
impl_from_ps_args!(T0, T1, T2, T3, T4, T5, T6, T7);
impl_from_ps_args!(T0, T1, T2, T3, T4, T5, T6, T7, T8);
impl_from_ps_args!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9);

/// Helper to get a particular arg from the array and convert it to a static
/// type
fn get_arg<T: FromPs>(args: &[Value], index: usize) -> Result<T, RuntimeError> {
    // If the arg is missing, use undefined instead to mirror PS semantics
    // TODO remove clone? we'd have to make FromPs take &Value
    let value = args.get(index).cloned().unwrap_or_default();
    let converted = T::from_ps(value).map_err(|error| {
        RuntimeError::from(error)
            .context(format!("Error converting argument {index}"))
    })?;
    Ok(converted)
}

/// TODO
pub trait IntoPsResult {
    /// TODO
    fn into_ps_result(self) -> Result<Value, RuntimeError>;
}

impl<T: IntoPs> IntoPsResult for T {
    fn into_ps_result(self) -> Result<Value, RuntimeError> {
        self.into_ps().map_err(RuntimeError::from)
    }
}

impl<T: IntoPs, E: Into<RuntimeError>> IntoPsResult for Result<T, E> {
    fn into_ps_result(self) -> Result<Value, RuntimeError> {
        match self {
            Ok(value) => Ok(value.into_ps()?),
            Err(error) => Err(error.into()),
        }
    }
}
