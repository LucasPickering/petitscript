//! Expression evaluation

use crate::{
    ast::{
        Argument, ArrayElementKind, ArrayLiteral, BinaryOperation,
        BinaryOperator, Expression, ExpressionKind, FunctionCall,
        FunctionDefinition, FunctionParameter, Literal, Node, ObjectLiteral,
        ObjectPropertyKind, OptionalPropertyAccess, PropertyAccess,
        PropertyName, PropertyNameKind, TemplateChunkKind, TemplateLiteral,
        UnaryOperation, UnaryOperator,
    },
    error::{RuntimeError, RuntimeErrorKind, ValueError},
    execute::{ThreadState, exec::Execute, state::CallSite},
    value::{Array, Number, Object, Value, ValueType, function::Function},
};
use std::{borrow::Cow, sync::Arc};

/// Evaluate an expression into a value
pub trait Evaluate: Node {
    /// Evaluate an AST node into a runtime [Value]. Errors are returned wrapped
    /// with the source span of the AST node from which the error originated.
    /// We use `Traced<RuntimeError,Span>` instead of `Error::Runtime` because
    /// we don't have the context needed here to qualify the spans.
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, RuntimeError>;
}

impl Evaluate for Expression {
    /// Evaluate an expression
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, RuntimeError> {
        match &self.kind {
            ExpressionKind::Parenthesized(expression) => expression.eval(state),
            ExpressionKind::Literal(literal) => match literal {
                Literal::Null => Ok(Value::Null),
                Literal::Undefined => Ok(Value::Undefined),
                Literal::Boolean(b) => Ok(Value::Boolean(*b)),
                Literal::String(s) => Ok(Value::String(s.as_str().into())),
                Literal::Float(f) => Ok(Value::Number(Number::Float(*f))),
                Literal::Int(i) => Ok(Value::Number(Number::Int(*i))),
                Literal::Array(array_literal) => array_literal.eval(state),
                Literal::Object(object_literal) => object_literal.eval(state),
            },
            ExpressionKind::Template(template_literal) => {
                template_literal.eval(state)
            }
            ExpressionKind::Identifier(identifier) => state
                .scope()
                .get(identifier.as_str())
                .map_err(|error| state.trace_error(error, self.id())),
            ExpressionKind::Function(function) => function.eval(state),
            ExpressionKind::Call(call) => call.eval(state),
            ExpressionKind::Property(access) => access.eval(state),
            ExpressionKind::OptionalProperty(access) => access.eval(state),
            ExpressionKind::Unary(unary) => unary.eval(state),
            ExpressionKind::Binary(binary) => binary.eval(state),
            ExpressionKind::Ternary(conditional) => {
                if conditional.condition.eval(state)?.to_bool() {
                    conditional.true_expression.eval(state)
                } else {
                    conditional.false_expression.eval(state)
                }
            }
        }
    }
}

impl Evaluate for ArrayLiteral {
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, RuntimeError> {
        let mut array = Array::default();
        for element in &self.elements {
            match &element.kind {
                // These are optimized to avoid allocations where possible
                ArrayElementKind::Expression(expression) => {
                    let value = expression.eval(state)?;
                    array = array.push(value);
                }
                ArrayElementKind::Spread(expression) => {
                    let new = expression
                        .eval(state)?
                        .try_into_array()
                        .map_err(|error| {
                            state.trace_error(error.into(), expression.id())
                        })?;
                    array = array.concat(new);
                }
            }
        }
        Ok(array.into())
    }
}

impl Evaluate for ObjectLiteral {
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, RuntimeError> {
        let mut object = Object::default();
        for property in &self.properties {
            match &property.kind {
                ObjectPropertyKind::Property {
                    property,
                    expression,
                } => {
                    // Convert to a string using coercion semantics.
                    // Make sure *not* to convert with the Display impl
                    let name = String::from(property.eval(state)?.to_string());
                    let value = expression.eval(state)?;
                    object = object.insert(name, value);
                }
                ObjectPropertyKind::Spread(expression) => {
                    let new = expression
                        .eval(state)?
                        .try_into_object()
                        .map_err(|error| {
                            state.trace_error(error.into(), expression.id())
                        })?;
                    object = object.insert_all(new);
                }
            }
        }
        Ok(object.into())
    }
}

impl Evaluate for PropertyName {
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, RuntimeError> {
        match &self.kind {
            // {field: "value"}
            // Shorthand syntax unpacks to {field: field} in parsing
            PropertyNameKind::Identifier(identifier) => {
                Ok(identifier.as_str().into())
            }
            // {["field"]: "value"}
            PropertyNameKind::Expression(expression) => expression.eval(state),
        }
    }
}

impl Evaluate for TemplateLiteral {
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, RuntimeError> {
        self.chunks
            .iter()
            .map(|chunk| match &chunk.kind {
                TemplateChunkKind::Literal(literal) => {
                    Ok(Cow::Borrowed(literal.as_str()))
                }
                TemplateChunkKind::Expression(expression) => {
                    // Convert to a string using coercion semantics.
                    // Make sure *not* to convert with the Display impl
                    Ok(Cow::Owned(String::from(
                        expression.eval(state)?.to_string(),
                    )))
                }
            })
            .collect::<Result<String, _>>()
            .map(Value::from)
    }
}

impl Evaluate for FunctionDefinition {
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, RuntimeError> {
        let scope = state.scope();
        let captures = scope
            .captures(&self.captures)
            .map_err(|error| state.trace_error(error, self.id()))?;

        Ok(Function::user(Arc::new(self.clone()), captures).into())
    }
}

/// This is implemented on `FunctionCall` so we can access our own node
/// ID to add to the call stack
impl Evaluate for FunctionCall {
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, RuntimeError> {
        let function = self.function.eval(state)?;
        match function {
            Value::Function(function) => {
                // Collect arguments
                let mut arguments = Vec::new();
                for argument in &self.arguments {
                    match argument {
                        Argument::Expression(expression) => {
                            arguments.push(expression.eval(state)?);
                        }
                        Argument::Spread(expression) => {
                            let array = expression
                                .eval(state)?
                                .try_into_array()
                                .map_err(|error| {
                                    state.trace_error(
                                        error.into(),
                                        expression.id(),
                                    )
                                })?;
                            arguments.extend(array);
                        }
                    }
                }

                function.call(state, self.id().into(), arguments)
            }
            // f?.() syntax. Skip the call, do NOT eval arguments
            Value::Undefined | Value::Null if self.optional => {
                Ok(Value::Undefined)
            }
            _ => Err(ValueError::Type {
                expected: ValueType::Function,
                actual: function.type_(),
            })
            .map_err(|error| state.trace_error(error.into(), self.id())),
        }
    }
}

/// Property access: `.` or `[...]`
impl Evaluate for PropertyAccess {
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, RuntimeError> {
        let target = self.object.eval(state)?;
        let property = self.property.eval(state)?;
        get_property(state, target, property)
            .map_err(|error| state.trace_error(error, self.id()))
    }
}

/// Optional object/array property access: `?.` or `?.[...]`
impl Evaluate for OptionalPropertyAccess {
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, RuntimeError> {
        match self.object.eval(state)? {
            // Do NOT evaluate the property expression if the target is
            // null/undefined
            Value::Undefined | Value::Null => Ok(Value::Undefined),
            target => {
                let property = self.property.eval(state)?;
                get_property(state, target, property).map_err(|error| {
                    // Error should be impossible here, because we handled the
                    // failure cases above
                    let error = RuntimeErrorKind::internal(error);
                    state.trace_error(error, self.id())
                })
            }
        }
    }
}

/// Get a property from a target
///
/// This will first check the value itself (for arrays/objects), then check
/// its prototype. Error if `target` is `null`/`undefined`.
fn get_property(
    state: &mut ThreadState<'_>,
    target: Value,
    property: Value,
) -> Result<Value, RuntimeErrorKind> {
    Ok(target
        // Check for the property directly on the value
        .get(&property)?
        // Check the prototype
        .or_else(|| {
            // If the key isn't a string, we know it won't be in the prototype
            // so we can bail out
            let name = property.as_str()?;
            let function = state.scope().get_prototype(target.type_(), name)?;
            Some(
                Function::bound(name.into(), function.clone(), target.clone())
                    .into(),
            )
        })
        .unwrap_or_default())
}

impl Evaluate for UnaryOperation {
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, RuntimeError> {
        let value = self.expression.eval(state)?;
        match self.operator {
            UnaryOperator::BooleanNot => Ok(!value),
            UnaryOperator::Plus => {
                Ok(value.to_number().unwrap_or(Number::NAN).into())
            }
            UnaryOperator::Minus => Ok(-value),
            UnaryOperator::Typeof => Ok(value.type_().to_string().into()),
        }
    }
}

impl Evaluate for BinaryOperation {
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, RuntimeError> {
        let lhs = self.lhs.eval(state)?;
        let rhs = self.rhs.eval(state)?;
        match self.operator {
            BinaryOperator::Add => Ok(lhs + rhs),
            BinaryOperator::Sub => Ok(lhs - rhs),
            BinaryOperator::Mul => Ok(lhs * rhs),
            BinaryOperator::Div => Ok(lhs / rhs),
            BinaryOperator::Mod => Ok(lhs % rhs),

            BinaryOperator::Equal => Ok((lhs == rhs).into()),
            BinaryOperator::NotEqual => Ok((lhs != rhs).into()),
            BinaryOperator::GreaterThan => Ok((lhs > rhs).into()),
            BinaryOperator::GreaterThanEqual => Ok((lhs >= rhs).into()),
            BinaryOperator::LessThan => Ok((lhs < rhs).into()),
            BinaryOperator::LessThanEqual => Ok((lhs <= rhs).into()),

            BinaryOperator::LogicalAnd => Ok(lhs.and(rhs)),
            BinaryOperator::LogicalOr => Ok(lhs.or(rhs)),
            BinaryOperator::NullishCoalesce => Ok(lhs.coalesce(rhs)),
        }
    }
}

impl Function {
    /// Call this function with some arguments. `call_site` is the AST node from
    /// which the function is being invoked. We track this for a  potential
    /// stack trace.
    pub(super) fn call(
        &self,
        state: &mut ThreadState<'_>,
        call_site: CallSite,
        arguments: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match self {
            Self::User {
                definition,
                captures,
            } => {
                // Put all declarations in a new frame. This will init a clean
                // scope from the globals. We'll add captures and args to that
                state.with_frame(
                    state.process().globals.clone().scope(),
                    call_site,
                    self.name().map(String::from),
                    |state| {
                        // Insert captured bindings into the scope
                        for (name, value) in captures {
                            state.scope_mut().declare(name, value.clone());
                        }
                        Self::bind_arguments(
                            state,
                            &definition.parameters,
                            arguments,
                        )?;
                        // Execute the body
                        definition.body.exec(state)
                    },
                )
            }
            Self::Native { function, .. } => function
                .call(state.process(), arguments)
                .map_err(|error| state.trace_error(error, call_site)),
            Self::Bound {
                function, receiver, ..
            } => function
                // Clone is necessary on receiver because there could be
                // multiple references to `this.f`
                .call(state.process(), Value::clone(receiver), arguments)
                .map_err(|error| state.trace_error(error, call_site)),
        }
    }

    /// Bind argument values to parameters in the current scope
    fn bind_arguments(
        state: &mut ThreadState,
        parameters: &[FunctionParameter],
        arguments: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        // Each parameter will be added to the scope for the next param,
        // meaning `f(a, b = a + 1) {...}` is *legal*
        for (i, parameter) in parameters.iter().enumerate() {
            // If we got fewer args than the function has defined, we'll pad it
            // out with defaults/undefined
            let value = if let Some(value) = arguments.get(i) {
                // ... parameters get all the remaining args
                if parameter.varargs {
                    if i < arguments.len() {
                        Array::from(arguments[i..].to_owned())
                    } else {
                        Array::new()
                    }
                    .into()
                } else {
                    value.clone()
                }
            } else if let Some(init) = &parameter.variable.init {
                // If the arg wasn't given, fall back to the init expression
                init.eval(state)?
            } else {
                // No init expression, use undefined
                Value::Undefined
            };
            state.bind(&parameter.variable.binding, value)?;
        }
        Ok(())
    }
}
