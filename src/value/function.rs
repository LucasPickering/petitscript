use crate::{runtime::scope::Scope, value::Value, Result};
use boa_ast::{
    function::{FormalParameter, FormalParameterList, FunctionBody},
    StatementListItem,
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
        parameters: FormalParameterList,
        body: FunctionBody,
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
    pub(crate) fn parameters(&self) -> &[FormalParameter] {
        self.0.parameters.as_ref()
    }

    /// Get the body's list of executable statements
    pub(crate) fn body(&self) -> &[StatementListItem] {
        self.0.body.statements()
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
    parameters: FormalParameterList,
    body: FunctionBody,
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
    pub fn call(&self, args: &[Value]) -> Result<Value> {
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
    fn call(&self, args: &[Value]) -> Result<Value>;
}

impl<F> NativeFunctionTrait for F
where
    F: 'static + Fn(&[Value]) -> Result<Value> + Send + Sync,
{
    fn call(&self, args: &[Value]) -> Result<Value> {
        (self)(args)
    }
}
