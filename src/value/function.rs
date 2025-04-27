use crate::{
    ast::FunctionDefinition,
    error::RuntimeError,
    value::{FromPetit, IntoPetit, Value},
    Process,
};
use indexmap::IndexMap;
use std::{
    fmt::{self, Debug, Display},
    mem,
    sync::Arc,
};

/// An executable function, bound to a specific program. All functions in
/// PetitScript are closures, meaning they capture their environment when
/// created, and references to outside variables may be used within the function
/// body.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
pub struct Function(pub(crate) Arc<FunctionInner>);

impl Function {
    /// Create a new function. A "user" function is a function defined in
    /// PetitScript, as opposed to a "native" function that's defined in Rust.
    pub(crate) fn user(
        definition: Arc<FunctionDefinition>,
        name: Option<String>,
        captures: Captures,
    ) -> Self {
        Self(
            FunctionInner::User {
                definition,
                name,
                captures,
            }
            .into(),
        )
    }

    /// Create a new native function. The function must have been predefined in
    /// the engine. This is just a "pointer" to the function definition.
    pub(crate) fn native(id: NativeFunctionId, name: Option<String>) -> Self {
        Self(FunctionInner::Native { id, name }.into())
    }

    /// Create a bound function. This will bind the function definition to the
    /// given receiver. The name will always be present for bound functions
    /// because they're only accessible through the prototype, which can only
    /// be accessed via string keys.
    pub(crate) fn bound(
        id: NativeFunctionId,
        receiver: Value,
        name: String,
    ) -> Self {
        Self(
            FunctionInner::Bound {
                id,
                receiver,
                name: Some(name),
            }
            .into(),
        )
    }

    /// TODO
    pub fn name(&self) -> Option<&str> {
        match &*self.0 {
            FunctionInner::User { name, .. }
            | FunctionInner::Native { name, .. }
            | FunctionInner::Bound { name, .. } => name.as_deref(),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let kind = match &*self.0 {
            FunctionInner::User { .. } => "user",
            FunctionInner::Native { .. } | FunctionInner::Bound { .. } => {
                "native"
            }
        };
        write!(
            f,
            "[Function: {} ({kind})]",
            self.name().unwrap_or("(anonymous)")
        )
    }
}

/// The implementation of a function, which is hidden from the external API
///
/// TODO include note about serde attributes matching the fields defs in
/// impls.rs
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub(crate) enum FunctionInner {
    /// A function defined in PetitScript
    #[cfg_attr(feature = "serde", serde(rename = "__UserFunction"))]
    User {
        /// A pointer to the function's definition. This is reference counted
        /// so the function can be cheaply clonable. We expect to clone
        /// definitions once from the AST during value creation, but after that
        /// the value can be cloned cheaply.
        /// TODO use Arc in the AST as well?
        #[cfg_attr(
            feature = "serde",
            serde(with = "crate::serde::serde_function_definition")
        )]
        definition: Arc<FunctionDefinition>,
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
    #[cfg_attr(feature = "serde", serde(rename = "__NativeFunction"))]
    Native {
        /// A pointer to this function's definition in the native function
        /// table
        id: NativeFunctionId,
        /// Display name for this function. This may not be available when the
        /// function is created, but it should be set once the function is
        /// bound to a value. This is for labelling purposes only; it does not
        /// affect program behavior beyond its `toString()` output.
        name: Option<String>,
    },
    /// A special case of a native function that is bound to a particular
    /// method receiver. This is used to implement prototype functions. In the
    /// case of `array.includes(3)`, `array.includes` represents a bound
    /// function value.
    #[cfg_attr(feature = "serde", serde(rename = "__BoundFunction"))]
    Bound {
        /// A pointer to this function's definition in the native function
        /// table
        id: NativeFunctionId,
        /// The value bound to this method. This is akin to the `self` value in
        /// Rust or Python methods. Unlike JS, PS doesn't support accessing the
        /// receiver via the `this` keyword, because bound functions can only
        /// be defined natively and not in PS code.
        receiver: Value,
        /// Display name for this function. This may not be available when the
        /// function is created, but it should be set once the function is
        /// bound to a value. This is for labelling purposes only; it does not
        /// affect program behavior beyond its `toString()` output.
        name: Option<String>,
    },
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
        Args: FromPetitArgs,
        Out: IntoPetitResult,
    {
        let id = NativeFunctionId(self.0.len() as u64);
        self.0.push(NativeFunctionDefinition::static_(function));
        Function::native(id, None)
    }

    /// Define a native function that must be bound to a receiver to be called.
    /// This can't create a [Function] because we don't know what value it will
    /// be bound to. The returned ID can be stored in a prototype to be used
    /// later.
    pub fn create_bound<F, This, Args, Out>(
        &mut self,
        function: F,
    ) -> NativeFunctionId
    where
        F: 'static + Fn(&Process, This, Args) -> Out + Send + Sync,
        This: FromPetit,
        Args: FromPetitArgs,
        Out: IntoPetitResult,
    {
        let id = NativeFunctionId(self.0.len() as u64);
        self.0.push(NativeFunctionDefinition::bound(function));
        id
    }
}

/// TODO
/// TODO namespace this by engine
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
pub(crate) struct NativeFunctionId(pub u64);

/// TODO
/// TODO split this into two structs in two tables
#[derive(Clone)]
pub(crate) enum NativeFunctionDefinition {
    /// A function that can be from anywhere with no receiver
    Static(
        #[allow(clippy::type_complexity)]
        Arc<
            dyn Fn(&Process, Vec<Value>) -> Result<Value, RuntimeError>
                + Send
                + Sync,
        >,
    ),
    /// A function bound to a receiver value. This is used to define prototype
    /// functions. Bound functions take an additional argument, which is the
    /// bound method receiver. For example, in `a.includes(1)`, `a` is the
    /// receiver and `[1]` is the argument list.
    Bound(
        #[allow(clippy::type_complexity)]
        Arc<
            dyn Fn(&Process, &Value, Vec<Value>) -> Result<Value, RuntimeError>
                + Send
                + Sync,
        >,
    ),
}

impl NativeFunctionDefinition {
    /// TODO
    /// TODO better name
    pub fn static_<F, Args, Out>(f: F) -> Self
    where
        F: 'static + Fn(&Process, Args) -> Out + Send + Sync,
        Args: FromPetitArgs,
        Out: IntoPetitResult,
    {
        // Wrap the lambda with logic to convert input/output/error, and box it
        let function = move |process: &Process, args: Vec<Value>| {
            // TODO add error context
            let args = Args::from_petit_args(args)?;
            let output = f(process, args).into_petit_result()?;
            output.into_petit().map_err(RuntimeError::Value)
        };
        Self::Static(Arc::new(function))
    }

    /// TODO
    pub fn bound<F, This, Args, Out>(f: F) -> Self
    where
        F: 'static + Fn(&Process, This, Args) -> Out + Send + Sync,
        This: FromPetit,
        Args: FromPetitArgs,
        Out: IntoPetitResult,
    {
        // Wrap the lambda with logic to convert input/output/error, and box it
        let function =
            move |process: &Process, this: &Value, args: Vec<Value>| {
                // TODO add error context
                // Clone is necessary on this receiver because there could be
                // multiple references to `this.f`
                let this = This::from_petit(this.clone())?;
                let args = Args::from_petit_args(args)?;
                let output = f(process, this, args).into_petit_result()?;
                output.into_petit().map_err(RuntimeError::Value)
            };
        Self::Bound(Arc::new(function))
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
        match (self, other) {
            (Self::Static(f0), Self::Static(f1)) => Arc::ptr_eq(f0, f1),
            (Self::Bound(f0), Self::Bound(f1)) => Arc::ptr_eq(f0, f1),
            (Self::Static(_), Self::Bound(_))
            | (Self::Bound(_), Self::Static(_)) => false,
        }
    }
}

/// TODO
pub struct Varargs(pub Vec<Value>);

/// TODO
pub trait FromPetitArgs: Sized {
    fn from_petit_args(args: Vec<Value>) -> Result<Self, RuntimeError>;
}

/// A recursive macro to pull a static number of arguments out of the arg array,
/// convert each one according to its FromPetit impl, then pass them all to a
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

/// Generate an implementation of FromPetitArgs for a fixed number of arguments
macro_rules! impl_from_petit_args {
    ($($arg_types:ident),*) => {
        impl<'a, $($arg_types,)*> FromPetitArgs for ($($arg_types,)*)
            where $($arg_types: FromPetit,)*
        {
            fn from_petit_args(mut args: Vec<Value>) -> Result<Self, RuntimeError> {
                Ok(call_fn!(&mut args, ($($arg_types,)*)))
            }
        }
    };
}

impl FromPetitArgs for () {
    fn from_petit_args(_: Vec<Value>) -> Result<Self, RuntimeError> {
        Ok(())
    }
}

/// TODO
impl FromPetitArgs for Varargs {
    fn from_petit_args(values: Vec<Value>) -> Result<Self, RuntimeError> {
        Ok(Self(values))
    }
}

/// Special case implementation: a single argument doesn't need a tuple wrapper
impl<T0: FromPetit> FromPetitArgs for T0 {
    fn from_petit_args(mut args: Vec<Value>) -> Result<Self, RuntimeError> {
        let arg0 = get_arg(&mut args, 0)?;
        Ok(arg0)
    }
}

impl_from_petit_args!(T0);
impl_from_petit_args!(T0, T1);
impl_from_petit_args!(T0, T1, T2);
impl_from_petit_args!(T0, T1, T2, T3);
impl_from_petit_args!(T0, T1, T2, T3, T4);
impl_from_petit_args!(T0, T1, T2, T3, T4, T5);
impl_from_petit_args!(T0, T1, T2, T3, T4, T5, T6);
impl_from_petit_args!(T0, T1, T2, T3, T4, T5, T6, T7);
impl_from_petit_args!(T0, T1, T2, T3, T4, T5, T6, T7, T8);
impl_from_petit_args!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9);

/// Extract an argument from the argument list by index, replacing it with
/// `undefined`. This should be called once per expected arg for a call, so
/// removing the value allows us to get an owned value without any cloning.
fn get_arg<T: FromPetit>(
    args: &mut [Value],
    index: usize,
) -> Result<T, RuntimeError> {
    let value = if index < args.len() {
        mem::take(&mut args[index])
    } else {
        // If the arg is missing, use undefined instead to mirror JS semantics
        Value::Undefined
    };
    let converted = T::from_petit(value).map_err(|error| {
        RuntimeError::from(error)
            .context(format!("Error converting argument {index}"))
    })?;
    Ok(converted)
}

/// TODO
pub trait IntoPetitResult {
    /// TODO
    fn into_petit_result(self) -> Result<Value, RuntimeError>;
}

impl<T: IntoPetit> IntoPetitResult for T {
    fn into_petit_result(self) -> Result<Value, RuntimeError> {
        self.into_petit().map_err(RuntimeError::from)
    }
}

impl<T: IntoPetit, E: Into<RuntimeError>> IntoPetitResult for Result<T, E> {
    fn into_petit_result(self) -> Result<Value, RuntimeError> {
        match self {
            Ok(value) => Ok(value.into_petit()?),
            Err(error) => Err(error.into()),
        }
    }
}
