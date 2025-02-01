//! Expression evaluation

use crate::{
    ast::{
        ArrayElement, ArrayLiteral, ArrowFunctionBody, AssignOperation,
        AssignOperator, BinaryOperation, BinaryOperator, Binding, Expression,
        FunctionCall, Literal, ObjectLiteral, ObjectProperty,
        OptionalPropertyAccess, PropertyAccess, PropertyName, Statement,
        TemplateLiteral, UnaryOperation, UnaryOperator,
    },
    error::RuntimeResult,
    runtime::{
        exec::{Execute, Terminate},
        state::RuntimeState,
    },
    value::{Array, Function, Number, Object, Value, ValueType},
    RuntimeError,
};
use std::iter;

/// TODO
pub trait Evaluate {
    /// TODO
    fn eval(&self, state: &mut RuntimeState) -> RuntimeResult<Value>;
}

impl Evaluate for Expression {
    /// Evaluate an expression
    fn eval(&self, state: &mut RuntimeState) -> RuntimeResult<Value> {
        match self {
            Expression::Parenthesized(expression) => expression.eval(state),
            Expression::Literal(literal) => literal.eval(state),
            Expression::Template(template_literal) => {
                template_literal.eval(state)
            }
            Expression::Identifier(identifier) => {
                state.scope().get(identifier.to_str())
            }
            Expression::ArrowFunction(function) => Ok(Function::new(
                None, // TODO grab name from binding if possible
                function.parameters.clone(),
                match function.body.clone() {
                    ArrowFunctionBody::Block(statements) => statements,
                    ArrowFunctionBody::Expression(expression) => {
                        Box::new([Statement::Return(Some(*expression))])
                    }
                },
                state.scope().capture(),
            )
            .into()),
            Expression::Call(call) => call.eval(state),
            Expression::Property(access) => access.eval(state),
            Expression::OptionalProperty(access) => access.eval(state),
            Expression::Assign(assign) => assign.eval(state),
            Expression::Unary(unary) => unary.eval(state),
            // Expression::Update(update) => update.eval(state),
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
    fn eval(&self, state: &mut RuntimeState) -> RuntimeResult<Value> {
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
    fn eval(&self, state: &mut RuntimeState) -> RuntimeResult<Value> {
        let array = self
            .elements
            .iter()
            .try_fold::<_, _, RuntimeResult<Array>>(
                Array::default(),
                |acc, element| {
                    match element {
                        // These are optimized to avoid allocations where
                        // possible
                        ArrayElement::Expression(expression) => {
                            let value = expression.eval(state)?;
                            Ok(acc.insert(value))
                        }
                        ArrayElement::Spread(expression) => {
                            let array =
                                expression.eval(state)?.try_into_array()?;
                            Ok(acc.insert_all(array))
                        }
                    }
                },
            )?;
        Ok(array.into())
    }
}

impl Evaluate for ObjectLiteral {
    fn eval(&self, state: &mut RuntimeState) -> RuntimeResult<Value> {
        let object = self.properties.iter().try_fold(
            Object::default(),
            |acc, property| match property {
                ObjectProperty::Property {
                    property,
                    expression,
                } => {
                    let name = match property {
                        // {field: "value"}
                        PropertyName::Literal(identifier) => {
                            identifier.to_str().to_owned()
                        }
                        // {["field"]: "value"}
                        PropertyName::Expression(expression) => {
                            // TODO should we fail for non-string props instead?
                            expression.eval(state)?.to_string()
                        }
                    };
                    let value = expression.eval(state)?;
                    Ok(acc.insert(name, value))
                }
                ObjectProperty::Identifier(identifier) => {
                    // Shorthand notation: { field }
                    let name = identifier.to_str().to_owned();
                    let value = state.scope().get(&name)?;
                    Ok::<_, RuntimeError>(acc.insert(name, value))
                }
                ObjectProperty::Spread(expression) => {
                    let object = expression.eval(state)?.try_into_object()?;
                    Ok(acc.insert_all(object))
                }
            },
        )?;
        Ok(object.into())
    }
}

impl Evaluate for TemplateLiteral {
    fn eval(&self, _: &mut RuntimeState) -> RuntimeResult<Value> {
        todo!()
    }
}

impl Evaluate for FunctionCall {
    fn eval(&self, state: &mut RuntimeState) -> RuntimeResult<Value> {
        let function = self.function.eval(state)?;
        let arguments = self
            .arguments
            .iter()
            .map(|arg| arg.eval(state))
            .collect::<RuntimeResult<Vec<_>>>()?;

        match function {
            Value::Function(function) => function.call(&arguments, state),
            Value::Native(function) => function.call(&arguments),
            Value::Undefined
            | Value::Null
            | Value::Boolean(_)
            | Value::Number(_)
            | Value::String(_)
            | Value::Array(_)
            | Value::Object(_) => Err(RuntimeError::Type {
                expected: ValueType::Function,
                actual: function.type_(),
            }),
        }
    }
}

impl Evaluate for PropertyAccess {
    fn eval(&self, state: &mut RuntimeState) -> RuntimeResult<Value> {
        let value = self.expression.eval(state)?;
        let key: Value = match &self.property {
            PropertyName::Literal(identifier) => identifier.to_str().into(),
            PropertyName::Expression(expression) => expression.eval(state)?,
        };
        value.get(&key)
    }
}

impl Evaluate for OptionalPropertyAccess {
    fn eval(&self, state: &mut RuntimeState) -> RuntimeResult<Value> {
        let target = self.expression.eval(state)?;
        match target {
            Value::Undefined | Value::Null => Ok(Value::Undefined),
            Value::Boolean(_)
            | Value::Number(_)
            | Value::String(_)
            | Value::Array(_)
            | Value::Object(_)
            | Value::Function(_)
            | Value::Native(_) => todo!(),
        }
    }
}

impl Evaluate for AssignOperation {
    fn eval(&self, state: &mut RuntimeState) -> RuntimeResult<Value> {
        let name = match &self.lhs {
            Binding::Identifier(identifier) => identifier.to_str(),
            Binding::Pattern(_) => todo!(),
        };
        let value = self.rhs.eval(state)?;
        match self.operator {
            AssignOperator::Assign => {
                state.scope().set(name, value.clone())?;
                // Return assigned value
                Ok(value)
            }
            AssignOperator::Add => todo!(),
            AssignOperator::Sub => todo!(),
            AssignOperator::Mul => todo!(),
            AssignOperator::Div => todo!(),
            AssignOperator::Mod => todo!(),
            AssignOperator::Coalesce => todo!(),
            AssignOperator::BooleanAnd => todo!(),
            AssignOperator::BooleanOr => todo!(),
        }
    }
}

impl Evaluate for UnaryOperation {
    fn eval(&self, state: &mut RuntimeState) -> RuntimeResult<Value> {
        let _ = self.expression.eval(state)?;
        match self.operator {
            UnaryOperator::BooleanNot => todo!(),
            UnaryOperator::Negate => todo!(),
        }
    }
}

impl Evaluate for BinaryOperation {
    fn eval(&self, state: &mut RuntimeState) -> RuntimeResult<Value> {
        let lhs = self.lhs.eval(state)?;
        let rhs = self.rhs.eval(state)?;
        match self.operator {
            BinaryOperator::Add => Ok(lhs + rhs),
            BinaryOperator::Sub => Ok(lhs - rhs),
            BinaryOperator::Mul => Ok(lhs * rhs),
            BinaryOperator::Div => Ok(lhs / rhs),
            BinaryOperator::Mod => todo!(),

            BinaryOperator::Equal => Ok((lhs == rhs).into()),
            BinaryOperator::NotEqual => Ok((lhs != rhs).into()),
            BinaryOperator::GreaterThan => todo!(),
            BinaryOperator::GreaterThanEqual => todo!(),
            BinaryOperator::LessThan => todo!(),
            BinaryOperator::LessThanEqual => todo!(),

            BinaryOperator::BooleanAnd => Ok(lhs.and(&rhs).into()),
            BinaryOperator::BooleanOr => Ok(lhs.or(&rhs).into()),
            BinaryOperator::NullishCoalesce => Ok(lhs.coalesce(rhs)),
        }
    }
}

impl Function {
    fn call(
        &self,
        args: &[Value],
        state: &mut RuntimeState,
    ) -> RuntimeResult<Value> {
        // Start with the function's captured scope
        let mut scope = self.scope().clone();

        // Add args to scope
        // If we got fewer args than the function has defined, we'll pad it out
        // with undefineds (or use the init expression defined in the func)
        let args_iter =
            args.iter().cloned().map(Some).chain(iter::repeat(None));
        for (parameter, value) in self.parameters().iter().zip(args_iter) {
            let value = if let Some(value) = value {
                value
            } else if let Some(init) = &parameter.variable.init {
                // If the arg wasn't given, fall back to the init expression
                init.eval(state)?
            } else {
                // No init expression, use undefined
                Value::Undefined
            };
            scope.bind(&parameter.variable.binding, value, false)?;
        }

        // Push the new frame onto the stack and execute the function body
        state.with_frame(scope, |state| match self.body().exec(state)? {
            Some(Terminate::Return {
                return_value: Some(return_value),
            }) => Ok(return_value),
            _ => Ok(Value::Undefined),
        })
    }
}
