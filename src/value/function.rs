use crate::{
    ast::{FunctionParameter, Statement},
    error::RuntimeResult,
    execute::scope::Scope,
    value::Value,
};
use std::{
    fmt::{self, Debug, Display},
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
    function: Arc<dyn NativeFunctionTrait>,
}

impl NativeFunction {
    /// Call this function
    pub fn call(&self, args: &[Value]) -> RuntimeResult<Value> {
        self.function.call(args)
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

impl<F: NativeFunctionTrait> From<F> for NativeFunction {
    fn from(function: F) -> Self {
        Self {
            function: Arc::new(function),
        }
    }
}

/// TODO doc and rename
pub trait NativeFunctionTrait: 'static + Send + Sync {
    fn call(&self, args: &[Value]) -> RuntimeResult<Value>;
}

impl<F> NativeFunctionTrait for F
where
    F: 'static + Fn(&[Value]) -> RuntimeResult<Value> + Send + Sync,
{
    fn call(&self, args: &[Value]) -> RuntimeResult<Value> {
        (self)(args)
    }
}
