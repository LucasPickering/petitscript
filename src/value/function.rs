use crate::{
    ast::{FunctionParameter, Statement},
    error::RuntimeResult,
    scope::Scope,
    value::Value,
    FromJs, IntoJs, RuntimeError,
};
use std::{
    fmt::{self, Debug, Display},
    future::{self, Future},
    pin::Pin,
    sync::Arc,
};

/// TODO
#[derive(Clone, Debug)]
pub struct Function(Arc<FunctionInner>);

impl Function {
    pub fn new(
        name: Option<String>,
        parameters: Box<[FunctionParameter]>,
        body: Box<[Statement]>,
        scope: Scope,
    ) -> Self {
        let inner = FunctionInner {
            name,
            parameters,
            body,
            scope,
        };
        Self(inner.into())
    }

    /// TODO
    pub fn name(&self) -> Option<&str> {
        self.0.name.as_deref()
    }

    /// TODO
    pub(crate) fn scope(&self) -> &Scope {
        &self.0.scope
    }

    /// TODO
    pub(crate) fn parameters(&self) -> &[FunctionParameter] {
        self.0.parameters.as_ref()
    }

    /// Get the body's list of executable statements
    pub(crate) fn body(&self) -> &[Statement] {
        &self.0.body
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[Function: {}]", self.name().unwrap_or("(anonymous)"))
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for Function {
    fn serialize<S>(&self, _: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        todo!()
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for Function {
    fn deserialize<D>(_: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        todo!()
    }
}

#[derive(Debug)]
struct FunctionInner {
    name: Option<String>,
    parameters: Box<[FunctionParameter]>,
    body: Box<[Statement]>,
    /// Captured variables. This is defined at function definition, and will be
    /// exposed to all calls of the function
    scope: Scope,
}

/// TODO
#[derive(Clone)]
pub struct NativeFunction {
    // TODO track name
    function: Arc<dyn Fn(Vec<Value>) -> RuntimeResult<Value> + Send + Sync>,
}

impl NativeFunction {
    /// Call this function
    pub(crate) fn call(&self, args: Vec<Value>) -> RuntimeResult<Value> {
        (self.function)(args)
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
pub trait IntoNativeFunction<In, Out, Err> {
    /// TODO
    fn into_native_fn(self) -> NativeFunction;
}

/// TODO
#[derive(Clone)]
pub struct AsyncNativeFunction {
    // TODO track name
    function: Arc<
        dyn Fn(
                Vec<Value>,
            )
                -> Pin<Box<dyn Future<Output = RuntimeResult<Value>>>>
            + Send
            + Sync,
    >,
}

impl AsyncNativeFunction {
    /// Call this function
    pub(crate) async fn call(&self, args: Vec<Value>) -> RuntimeResult<Value> {
        (self.function)(args).await
    }
}

impl Display for AsyncNativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[AsyncNativeFunction]")
    }
}

impl Debug for AsyncNativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AsyncNativeFunction")
            .field("function", &"...")
            .finish()
    }
}

/// TODO
pub trait IntoAsyncNativeFunction<In, Out, Err> {
    /// TODO
    fn into_native_fn(self) -> AsyncNativeFunction;
}

/// A recursive macro to pull a static number of arguments out of the arg array,
/// convert each one according to its FromJs impl, then pass them all to a
/// function
macro_rules! call_fn {
    // Entrypoint - pass a function you want called, the array of arguments to
    // convert, and a list of the static types of each argument
    ($f:expr, $args:expr, $($arg_types:ident)*) => {
        call_fn!(@step $f, $args, 0usize, [], [$($arg_types,)*])
    };
    // Recursive step - Pop the next arg type off the front of a list, then
    // generate an expression to pull the corresponding arg out of the array
    // and convert it
    (@step
        $f:expr, // Function to call
        $args:expr, // Untyped arg array
        $index:expr, // Index of the *next* arg to convert
        [$($acc:expr,)*], // Args that have been converted so far
        // Types of args that have yet to be converted
        [$first:ident, $($rest:ident,)*]
    ) => {
        call_fn!(@step
            $f,
            $args,
            $index + 1,
            [$($acc,)* get_arg($args, $index)?,],
            [$($rest,)*]
        )
    };
    // Base case - all args have been converted. Call the function with all our
    // arg expressions
    (@step $f:expr, $args:expr, $_index:expr, [$($acc:expr,)*], []) => {
        $f($($acc,)*)
    };
}

/// Generate an implementation of IntoNativeFunction for a fixed number of
/// arguments
macro_rules! impl_into_native_function {
    ($($arg_types:ident),*) => {
        impl<F, $($arg_types,)* Out, Err> IntoNativeFunction<($($arg_types,)*), Out, Err> for F
        where
            F: 'static + Fn($($arg_types,)*) -> Result<Out, Err> + Send + Sync,
            $($arg_types: FromJs,)*
            Out: IntoJs,
            Err: Into<RuntimeError>,
        {
            fn into_native_fn(self) -> NativeFunction {
                #[allow(unused_variables)]
                let function = move |args: Vec<Value>| -> RuntimeResult<Value> {
                    let output = call_fn!(self, &args, $($arg_types)*).map_err(Err::into)?;
                    output.into_js()
                };
                NativeFunction {
                    function: Arc::new(function),
                }
            }
        }
    };
}

impl_into_native_function!();
impl_into_native_function!(I1);
impl_into_native_function!(I1, I2);
impl_into_native_function!(I1, I2, I3);
impl_into_native_function!(I1, I2, I3, I4);
impl_into_native_function!(I1, I2, I3, I4, I5);
impl_into_native_function!(I1, I2, I3, I4, I5, I6);
impl_into_native_function!(I1, I2, I3, I4, I5, I6, I7);
impl_into_native_function!(I1, I2, I3, I4, I5, I6, I7, I8);
impl_into_native_function!(I1, I2, I3, I4, I5, I6, I7, I8, I9);
impl_into_native_function!(I1, I2, I3, I4, I5, I6, I7, I8, I9, I10);

/// Generate an implementation of IntoNativeFunction for a fixed number of
/// arguments
macro_rules! impl_into_async_native_function {
    ($($arg_types:ident),*) => {
        impl<F, $($arg_types,)* Fut, Out, Err> IntoAsyncNativeFunction<($($arg_types,)*), Out, Err> for F
        where
            F: 'static + Fn($($arg_types,)*) -> Fut + Send + Sync,
            $($arg_types: FromJs,)*
            Fut: 'static + Future<Output = Result<Out, Err>>,
            Out: IntoJs,
            Err: Into<RuntimeError>,
        {
            fn into_native_fn(self) -> AsyncNativeFunction {
                #[allow(unused_variables, clippy::redundant_closure_call)]
                let function = move |args: Vec<Value>| {
                    // TODO explain
                    let result = (|| Ok(call_fn!(self, &args, $($arg_types)*)))();
                    match result {
                        Ok(future) => Box::pin(async move {
                            let output = future.await.map_err(Err::into)?;
                            output.into_js()
                        })
                            as Pin<Box<dyn Future<Output = _>>>,
                        Err(error) => Box::pin(future::ready(Err(error))),
                    }
                };
                AsyncNativeFunction {
                    function: Arc::new(function),
                }
            }
        }
    };
}

impl_into_async_native_function!();
impl_into_async_native_function!(I1);
impl_into_async_native_function!(I1, I2);
impl_into_async_native_function!(I1, I2, I3);
impl_into_async_native_function!(I1, I2, I3, I4);
impl_into_async_native_function!(I1, I2, I3, I4, I5);
impl_into_async_native_function!(I1, I2, I3, I4, I5, I6);
impl_into_async_native_function!(I1, I2, I3, I4, I5, I6, I7);
impl_into_async_native_function!(I1, I2, I3, I4, I5, I6, I7, I8);
impl_into_async_native_function!(I1, I2, I3, I4, I5, I6, I7, I8, I9);
impl_into_async_native_function!(I1, I2, I3, I4, I5, I6, I7, I8, I9, I10);

/// Helper to get a particular arg from the array and convert it to a static
/// type
fn get_arg<T: FromJs>(args: &[Value], index: usize) -> RuntimeResult<T> {
    // If the arg is missing, use undefined instead to mirror JS semantics
    // TODO remove clone? we'd have to make FromJs take &Value
    let value = args.get(index).cloned().unwrap_or_default();
    T::from_js(value)
}
