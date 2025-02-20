use crate::{
    ast::{FunctionId, FunctionParameter, Spanned, Statement},
    error::RuntimeError,
    value::Value,
    FromJs, IntoJs, Process,
};
use std::{
    fmt::{self, Debug, Display},
    sync::Arc,
};

/// TODO
#[derive(Clone, Debug)]
pub struct Function {
    /// TODO
    id: FunctionId,
    /// Duplicate the name, for printing/debugging
    name: Option<String>,
}

impl Function {
    /// TODO
    pub(crate) fn new(id: FunctionId, name: Option<String>) -> Self {
        Self { id, name }
    }

    /// TODO
    pub(crate) fn id(&self) -> FunctionId {
        self.id
    }

    /// TODO
    pub fn name(&self) -> &str {
        self.name.as_deref().unwrap_or("(anonymous)")
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[Function: {}]", self.name())
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for Function {
    fn serialize<S>(&self, _: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        todo!()
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for Function {
    fn deserialize<D>(_: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        todo!()
    }
}

/// TODO
#[derive(Debug)]
pub(crate) struct FunctionDefinition {
    pub name: Option<String>,
    pub parameters: Box<[Spanned<FunctionParameter>]>,
    pub body: Box<[Spanned<Statement>]>,
}

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
