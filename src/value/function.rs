use crate::{
    compile::FunctionDefinitionId, error::RuntimeError, execute::ProcessId,
    value::Value, FromJs, IntoJs, Process,
};
use indexmap::IndexMap;
use std::{
    fmt::{self, Debug, Display},
    sync::Arc,
};

/// An executable function, bound to a specific program. All functions in
/// PetitJS are closures, meaning they capture their environment when created,
/// and references to outside variables may be used within the function body.
#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    /// TODO
    id: FunctionId,
    /// The name is also contained in the function definition that the ID
    /// points to, but we duplicate it here for easy access during
    /// printing/debugging
    name: Option<String>,
    /// All external bound values captured by this function. Functions _cannot_
    /// mutate captured bindings, only read them. Therefore, we don't need
    /// to capture the bindings, we can just take the contained value.
    captures: Captures,
}

impl Function {
    /// TODO
    pub(crate) fn new(
        id: FunctionId,
        name: Option<String>,
        captures: Captures,
    ) -> Self {
        Self { id, name, captures }
    }

    /// TODO
    pub(crate) fn id(&self) -> FunctionId {
        self.id
    }

    /// TODO
    pub fn name(&self) -> Option<&str> {
        self.name.as_deref()
    }

    /// TODO
    pub fn captures(&self) -> &Captures {
        &self.captures
    }

    /// TODO
    #[cfg(feature = "serde")]
    pub(crate) fn into_parts(self) -> (FunctionId, Option<String>, Captures) {
        (self.id, self.name, self.captures)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[Function: {}]", self.name().unwrap_or("(anonymous)"))
    }
}

/// A unique ID for a function. TODO more info on uniqueness
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct FunctionId {
    pub process_id: ProcessId,
    pub definition_id: FunctionDefinitionId,
}

pub type Captures = IndexMap<String, Value>;

/// TODO
#[derive(Clone)]
pub struct NativeFunction {
    // TODO track name
    #[allow(clippy::type_complexity)]
    function: Arc<
        dyn Fn(&Process, Vec<Value>) -> Result<Value, RuntimeError>
            + Send
            + Sync,
    >,
}

impl NativeFunction {
    /// TODO
    pub(crate) fn new<F, Args, Out, Err>(f: F) -> Self
    where
        F: 'static + Fn(&Process, Args) -> Result<Out, Err> + Send + Sync,
        Args: FromJsArgs,
        Out: IntoJs,
        Err: Into<RuntimeError>,
    {
        // Wrap the lambda with logic to convert input/output/error, and box it
        // TODO take &'a [Value] instead?
        let function = move |process: &Process, args: Vec<Value>| {
            let args = Args::from_js_args(&args)?;
            let output = f(process, args).map_err(Err::into)?;
            output.into_js().map_err(RuntimeError::Value)
        };
        Self {
            function: Arc::new(function),
        }
    }

    /// Call this function
    pub(crate) fn call(
        &self,
        process: &Process,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        (self.function)(process, args)
    }
}

impl Display for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[NativeFunction]")
    }
}

impl Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("NativeFunction")
            .field("function", &"...")
            .finish()
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        // If we both point to the same function, we're the same
        Arc::ptr_eq(&self.function, &other.function)
    }
}

/// TODO
pub struct Varargs(pub Vec<Value>);

/// TODO
pub trait FromJsArgs: Sized {
    fn from_js_args(args: &[Value]) -> Result<Self, RuntimeError>;
}

/// A recursive macro to pull a static number of arguments out of the arg array,
/// convert each one according to its FromJs impl, then pass them all to a
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

/// Generate an implementation of FromJsArgs for a fixed number of arguments
macro_rules! impl_from_js_args {
    ($($arg_types:ident),*) => {
        impl<'a, $($arg_types,)*> FromJsArgs for ($($arg_types,)*)
            where $($arg_types: FromJs,)*
        {
            fn from_js_args(args: &[Value]) -> Result<Self, RuntimeError> {
                Ok(call_fn!(args, ($($arg_types,)*)))
            }
        }
    };
}

impl FromJsArgs for () {
    fn from_js_args(_: &[Value]) -> Result<Self, RuntimeError> {
        Ok(())
    }
}

/// TODO
impl FromJsArgs for Varargs {
    fn from_js_args(values: &[Value]) -> Result<Self, RuntimeError> {
        // TODO remove clones
        Ok(Self(values.to_owned()))
    }
}

/// Special case implementation: a single argument doesn't need a tuple wrapper
impl<T0: FromJs> FromJsArgs for T0 {
    fn from_js_args(args: &[Value]) -> Result<Self, RuntimeError> {
        let arg0 = get_arg(args, 0)?;
        Ok(arg0)
    }
}

impl_from_js_args!(T0);
impl_from_js_args!(T0, T1);
impl_from_js_args!(T0, T1, T2);
impl_from_js_args!(T0, T1, T2, T3);
impl_from_js_args!(T0, T1, T2, T3, T4);
impl_from_js_args!(T0, T1, T2, T3, T4, T5);
impl_from_js_args!(T0, T1, T2, T3, T4, T5, T6);
impl_from_js_args!(T0, T1, T2, T3, T4, T5, T6, T7);
impl_from_js_args!(T0, T1, T2, T3, T4, T5, T6, T7, T8);
impl_from_js_args!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9);

/// Helper to get a particular arg from the array and convert it to a static
/// type
fn get_arg<T: FromJs>(args: &[Value], index: usize) -> Result<T, RuntimeError> {
    // If the arg is missing, use undefined instead to mirror JS semantics
    // TODO remove clone? we'd have to make FromJs take &Value
    let value = args.get(index).cloned().unwrap_or_default();
    Ok(T::from_js(value)?)
}
