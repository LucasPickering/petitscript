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

/// An executable function
#[derive(Clone, Debug, PartialEq)]
pub enum Function {
    /// A function defined in PetitScript code. All user functions are
    /// closures, meaning they capture their environment when created.
    /// References to outside variables can be used within the function body.
    User {
        /// A pointer to the function's definition. This is reference counted
        /// so the function can be cheaply clonable. We expect to clone
        /// definitions once from the AST during value creation, but after that
        /// the value can be cloned cheaply.
        /// TODO use Arc in the AST as well?
        definition: Arc<FunctionDefinition>,
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

    /// A function defined in Rust
    Native {
        /// Display name for this function. Native functions should always be
        /// created via [Function::native], which requires a name to be given.
        /// There's no such thing as an anonymous native function. This is for
        /// labelling purposes only; it does not affect program behavior beyond
        /// its `toString()` output.
        name: Arc<str>,
        /// A pointer to this function's definition in the native function
        /// table
        function: NativeFunction,
    },

    /// A special case of a native function that is bound to a particular
    /// method receiver. This is used to implement prototype functions. In the
    /// example `array.includes(3)`, `array.includes` represents a bound
    /// function value.
    ///
    /// Prototypes cannot be modified externally. Therefore, there is no way to
    /// create a bound function in external consumers.
    Bound {
        /// Display name for this function. Native functions should always be
        /// created via [Function::bound], which requires a name to be given.
        /// There's no such thing as an anonymous native function. This is for
        /// labelling purposes only; it does not affect program behavior beyond
        /// its `toString()` output.
        name: Arc<str>,
        /// A pointer to this function's definition in the native function
        /// table
        function: BoundFunction,
        /// The value bound to this method. This is akin to the `self` value in
        /// Rust or Python methods. Unlike JS, PS doesn't support accessing the
        /// receiver via the `this` keyword, because bound functions can only
        /// be defined natively and not in PS code.
        receiver: Box<Value>,
    },
}

impl Function {
    /// Create a new function. A "user" function is a function defined in
    /// PetitScript, as opposed to a "native" function that's defined in Rust.
    /// TODO rename to `new`?
    pub fn user(
        definition: Arc<FunctionDefinition>,
        captures: Captures,
    ) -> Self {
        Self::User {
            definition,
            captures,
        }
    }

    /// Create a new native function. Generally it's more convenient to call
    /// [Object::insert_fn](crate::value::Object::insert_fn)
    /// or [Exports::export_fn](crate::Exports::export_fn).
    pub fn native<F, Args, Out>(name: String, function: F) -> Self
    where
        F: 'static + Fn(&Process, Args) -> Out + Send + Sync,
        Args: FromPetitArgs,
        Out: IntoPetitResult,
    {
        Self::Native {
            name: name.into(),
            function: NativeFunction::new(function),
        }
    }

    /// Create a bound function. This will bind the function definition to the
    /// given receiver. The name will always be present for bound functions
    /// because they're only accessible through the prototype, which can only
    /// be accessed via string keys.
    pub(crate) fn bound(
        name: String,
        function: BoundFunction,
        receiver: Value,
    ) -> Self {
        Self::Bound {
            name: name.into(),
            function,
            receiver: Box::new(receiver),
        }
    }

    /// TODO
    pub fn name(&self) -> Option<&str> {
        match self {
            Self::User { definition, .. } => {
                definition.name.as_ref().map(|name| name.as_str())
            }
            Self::Native { name, .. } | Self::Bound { name, .. } => Some(name),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let kind = match self {
            Self::User { .. } => "user",
            Self::Native { .. } | Self::Bound { .. } => "native",
        };
        write!(
            f,
            "[Function: {} ({kind})]",
            self.name().unwrap_or("(anonymous)")
        )
    }
}

/// A set of captured values for a closure
pub type Captures = IndexMap<String, Value>;

/// TODO
#[derive(Clone)]
pub struct NativeFunction(
    #[allow(clippy::type_complexity)]
    Arc<
        dyn Fn(&Process, Vec<Value>) -> Result<Value, RuntimeError>
            + Send
            + Sync,
    >,
);

impl NativeFunction {
    /// TODO
    fn new<F, Args, Out>(f: F) -> Self
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
        Self(Arc::new(function))
    }

    /// Invoke this function
    pub(crate) fn call(
        &self,
        process: &Process,
        arguments: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        (self.0)(process, arguments)
    }
}

impl Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "NativeFunction(0x{:x})",
            // Print the pointer to the function as something identifying
            Arc::as_ptr(&self.0) as *const () as usize
        )
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

/// TODO
#[derive(Clone)]
pub struct BoundFunction(
    #[allow(clippy::type_complexity)]
    Arc<
        dyn Fn(&Process, Value, Vec<Value>) -> Result<Value, RuntimeError>
            + Send
            + Sync,
    >,
);

impl BoundFunction {
    /// TODO
    pub(crate) fn new<F, This, Args, Out>(f: F) -> Self
    where
        F: 'static + Fn(&Process, This, Args) -> Out + Send + Sync,
        This: FromPetit,
        Args: FromPetitArgs,
        Out: IntoPetitResult,
    {
        // Wrap the lambda with logic to convert input/output/error, and box it
        let function =
            move |process: &Process, this: Value, args: Vec<Value>| {
                // TODO add error context
                let this = This::from_petit(this.clone())?;
                let args = Args::from_petit_args(args)?;
                let output = f(process, this, args).into_petit_result()?;
                output.into_petit().map_err(RuntimeError::Value)
            };
        Self(Arc::new(function))
    }

    /// Invoke this function
    pub(crate) fn call(
        &self,
        process: &Process,
        receiver: Value,
        arguments: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        (self.0)(process, receiver, arguments)
    }
}

impl Debug for BoundFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "BoundFunction(0x{:x})",
            // Print the pointer to the function as something identifying
            Arc::as_ptr(&self.0) as *const () as usize
        )
    }
}

impl PartialEq for BoundFunction {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
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
