//! Expression evaluation

use crate::{
    ast::{
        source::{Span, Spanned},
        ArrayElement, ArrayLiteral, BinaryOperation, BinaryOperator,
        Expression, FunctionCall, FunctionPointer, Literal, ObjectLiteral,
        ObjectProperty, OptionalPropertyAccess, PropertyAccess, PropertyName,
        TemplateChunk, TemplateLiteral, UnaryOperation, UnaryOperator,
    },
    error::{RuntimeError, TracedError, ValueError},
    execute::{
        exec::{Execute, Terminate},
        ThreadState,
    },
    function::{Function, FunctionInner, UserFunctionId},
    value::{Array, Number, Object, Value, ValueType},
};
use std::{borrow::Cow, sync::Arc};

/// Evaluate an expression into a value
pub trait Evaluate {
    /// Evaluate an AST node into a runtime [Value]. Errors are returned wrapped
    /// with the source span of the AST node from which the error originated.
    /// We use `Traced<RuntimeError,Span>` instead of `Error::Runtime` because
    /// we don't have the context needed here to qualify the spans.
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, TracedError>;
}

impl Evaluate for Expression {
    /// Evaluate an expression
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, TracedError> {
        match self {
            Expression::Parenthesized(expression) => expression.eval(state),
            Expression::Literal(literal) => literal.eval(state),
            Expression::Template(template_literal) => {
                template_literal.eval(state)
            }
            Expression::Identifier(identifier) => state
                .scope()
                .get(identifier.as_str())
                .map_err(|error| state.trace_error(error, identifier.span)),
            Expression::ArrowFunction(function) => function.eval(state),
            Expression::Call(call) => call.eval(state),
            Expression::Property(access) => access.eval(state),
            Expression::OptionalProperty(access) => access.eval(state),
            Expression::Unary(unary) => unary.eval(state),
            Expression::Binary(binary) => binary.eval(state),
            Expression::Ternary(conditional) => {
                if conditional.condition.eval(state)?.to_bool() {
                    conditional.true_expression.eval(state)
                } else {
                    conditional.false_expression.eval(state)
                }
            }
        }
    }
}

impl Evaluate for Literal {
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, TracedError> {
        match self {
            Self::Null => Ok(Value::Null),
            Self::Undefined => Ok(Value::Undefined),
            Self::Boolean(b) => Ok(Value::Boolean(*b)),
            Self::String(s) => Ok(Value::String(s.as_str().into())),
            Self::Float(f) => Ok(Value::Number(Number::Float(*f))),
            Self::Int(i) => Ok(Value::Number(Number::Int(*i))),
            Self::Array(array_literal) => array_literal.eval(state),
            Self::Object(object_literal) => object_literal.eval(state),
        }
    }
}

impl Evaluate for ArrayLiteral {
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, TracedError> {
        let mut array = Array::default();
        for element in &self.elements {
            match &element.data {
                // These are optimized to avoid allocations where
                // possible
                ArrayElement::Expression(expression) => {
                    let value = expression.eval(state)?;
                    array = array.push(value);
                }
                ArrayElement::Spread(expression) => {
                    let new = expression
                        .eval(state)?
                        .try_into_array()
                        .map_err(|error| {
                            state.trace_error(error.into(), expression.span)
                        })?;
                    array = array.concat(new);
                }
            }
        }
        Ok(array.into())
    }
}

impl Evaluate for ObjectLiteral {
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, TracedError> {
        let mut object = Object::default();
        for property in &self.properties {
            match &property.data {
                ObjectProperty::Property {
                    property,
                    expression,
                } => {
                    let name = match &property.data {
                        // {field: "value"}
                        PropertyName::Literal(identifier) => {
                            identifier.as_str().to_owned()
                        }
                        // {["field"]: "value"}
                        PropertyName::Expression(expression) => {
                            // TODO should we fail for non-string props
                            // instead?
                            expression.eval(state)?.to_string()
                        }
                    };
                    let value = expression.eval(state)?;
                    object = object.insert(name, value);
                }
                ObjectProperty::Identifier(identifier) => {
                    // Shorthand notation: { field }
                    let name = identifier.as_str().to_owned();
                    let value = state.scope().get(&name).map_err(|error| {
                        state.trace_error(error, identifier.span)
                    })?;
                    object = object.insert(name, value);
                }
                ObjectProperty::Spread(expression) => {
                    let new = expression
                        .eval(state)?
                        .try_into_object()
                        .map_err(|error| {
                            state.trace_error(error.into(), expression.span)
                        })?;
                    object = object.insert_all(new);
                }
            }
        }
        Ok(object.into())
    }
}

impl Evaluate for TemplateLiteral {
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, TracedError> {
        self.chunks
            .iter()
            .map(|chunk| match &chunk.data {
                TemplateChunk::Literal(literal) => {
                    Ok(Cow::Borrowed(literal.data.as_str()))
                }
                TemplateChunk::Expression(expression) => {
                    Ok(Cow::Owned(expression.eval(state)?.to_string()))
                }
            })
            .collect::<Result<String, _>>()
            .map(Value::from)
    }
}

impl Evaluate for Spanned<FunctionPointer> {
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, TracedError> {
        // All functions should have been lifted during compilation. If not,
        // that's a compiler bug
        match &self.data {
            FunctionPointer::Inline(definition) => Err(RuntimeError::internal(
                format!("Function definition {definition:?} was not lifted"),
            ))
            .map_err(|error| state.trace_error(error, self.span)),
            FunctionPointer::Lifted(definition_id) => {
                let id = UserFunctionId {
                    process_id: state.process().id(),
                    definition_id: *definition_id,
                };

                // Copy the name from the definition so the function knows its
                // own name easily, for printing and errors
                let definition = state
                    .process()
                    .program
                    .function_table()
                    .get(id.definition_id)
                    .map_err(|error| state.trace_error(error, self.span))?;
                let name =
                    definition.name.as_ref().map(|name| name.data.to_string());

                let scope = state.scope();
                let captures = scope
                    .captures(&definition.captures)
                    .map_err(|error| state.trace_error(error, self.span))?;

                Ok(Function::user(id, name, captures).into())
            }
        }
    }
}

impl Evaluate for Spanned<FunctionCall> {
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, TracedError> {
        let function = self.function.eval(state)?;
        let mut arguments = Vec::with_capacity(self.arguments.len());
        for argument in &self.arguments {
            let value = argument.eval(state)?;
            arguments.push(value);
        }

        match function {
            Value::Function(function) => {
                function.call(state, self.span, &arguments)
            }
            _ => Err(ValueError::Type {
                expected: ValueType::Function,
                actual: function.type_(),
            })
            .map_err(|error| state.trace_error(error.into(), self.span)),
        }
    }
}

impl Evaluate for Spanned<PropertyAccess> {
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, TracedError> {
        let value = self.expression.eval(state)?;
        let key: Value = match &self.property.data {
            PropertyName::Literal(identifier) => identifier.as_str().into(),
            PropertyName::Expression(expression) => expression.eval(state)?,
        };
        value
            .get(&key)
            .map_err(|error| state.trace_error(error, self.span))
    }
}

impl Evaluate for OptionalPropertyAccess {
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, TracedError> {
        let target = self.expression.eval(state)?;
        match target {
            Value::Undefined | Value::Null => Ok(Value::Undefined),
            Value::Boolean(_)
            | Value::Number(_)
            | Value::String(_)
            | Value::Array(_)
            | Value::Object(_)
            | Value::Function(_) => todo!(),
            #[cfg(feature = "bytes")]
            Value::Buffer(_) => todo!(),
        }
    }
}

impl Evaluate for UnaryOperation {
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, TracedError> {
        let value = self.expression.eval(state)?;
        match self.operator {
            UnaryOperator::BooleanNot => Ok(!value),
            UnaryOperator::Negate => Ok(-value),
        }
    }
}

impl Evaluate for BinaryOperation {
    fn eval(&self, state: &mut ThreadState<'_>) -> Result<Value, TracedError> {
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

            BinaryOperator::BooleanAnd => Ok(lhs.and(&rhs).into()),
            BinaryOperator::BooleanOr => Ok(lhs.or(&rhs).into()),
            BinaryOperator::NullishCoalesce => Ok(lhs.coalesce(rhs)),
        }
    }
}

impl Function {
    /// Call this function with some arguments. `call_site` is the source
    /// location from which the function is being invoked. We track this for a
    /// potential stack trace
    pub(super) fn call(
        &self,
        state: &mut ThreadState<'_>,
        call_site: Span,
        arguments: &[Value],
    ) -> Result<Value, TracedError> {
        match &self.0 {
            FunctionInner::User { id, captures, .. } => {
                let definition = Arc::clone(
                    state
                        .program()
                        .function_table()
                        .get(id.definition_id)
                        .map_err(|error| state.trace_error(error, call_site))?,
                );
                // Create a new scope based on the global namespace, with
                // captured bindings applied
                let mut scope = state.process().globals.clone().child();
                for (name, value) in captures {
                    scope.declare(name, value.clone());
                }

                // Add args to scope
                // If we got fewer args than the function has defined, we'll pad
                // it out with undefineds (or use the init expression defined in
                // the func)
                for (i, parameter) in definition.parameters.iter().enumerate() {
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
                        // If the arg wasn't given, fall back to the init
                        // expression
                        // TODO make sure previous args are available here
                        init.eval(state)?
                    } else {
                        // No init expression, use undefined
                        Value::Undefined
                    };
                    scope.bind(&parameter.variable.binding, value).map_err(
                        |error| {
                            state.trace_error(error, parameter.variable.span)
                        },
                    )?;
                }

                // Push the new frame onto the stack and execute the function
                // body
                state.with_frame(scope, call_site, self.id(), |state| {
                    match definition.body.exec(state)? {
                        Some(Terminate::Return {
                            return_value: Some(return_value),
                        }) => Ok(return_value),
                        _ => Ok(Value::Undefined),
                    }
                })
            }
            FunctionInner::Native { id, .. } => {
                let definition = state
                    .process()
                    .native_functions
                    .get(*id)
                    .map_err(|error| state.trace_error(error, call_site))?;
                definition
                    .call(state.process(), arguments)
                    .map_err(|error| state.trace_error(error, call_site))
            }
        }
    }
}
