//! Expression evaluation

use crate::{
    runtime::{
        exec::{Execute, Terminate},
        RuntimeState,
    },
    value::{Array, Function, Number, Object, Value, ValueType},
    Error, Result,
};
use boa_ast::{
    expression::{
        access::{PropertyAccess, PropertyAccessField},
        literal::{
            ArrayLiteral, Literal, ObjectLiteral, PropertyDefinition,
            TemplateLiteral,
        },
        operator::{
            assign::{AssignOp, AssignTarget},
            binary::{
                ArithmeticOp, BinaryOp, BitwiseOp, LogicalOp, RelationalOp,
            },
            Assign, Binary, Unary, Update,
        },
        Optional, Spread,
    },
    property::PropertyName,
    Expression,
};
use std::iter;

/// TODO
pub trait Evaluate {
    /// TODO
    fn eval(&self, state: &mut RuntimeState) -> Result<Value>;
}

impl Evaluate for Expression {
    /// Evaluate an expression
    fn eval(&self, state: &mut RuntimeState) -> Result<Value> {
        match self {
            Expression::Identifier(identifier) => {
                let resolver = state.resolver();
                let name = resolver.resolve(identifier.sym());
                state.scope().get(name)
            }
            Expression::Literal(literal) => literal.eval(state),
            Expression::TemplateLiteral(template_literal) => {
                template_literal.eval(state)
            }
            Expression::ArrayLiteral(array_literal) => {
                array_literal.eval(state)
            }
            Expression::ObjectLiteral(object_literal) => {
                object_literal.eval(state)
            }
            Expression::Spread(spread) => spread.eval(state),
            Expression::FunctionExpression(function) => Ok(Function::new(
                function.name().map(|name| {
                    state.resolver().resolve(name.sym()).to_owned()
                }),
                function.parameters().clone(),
                function.body().clone(),
                state.scope().capture(),
            )
            .into()),
            Expression::ArrowFunction(function) => Ok(Function::new(
                function.name().map(|name| {
                    state.resolver().resolve(name.sym()).to_owned()
                }),
                function.parameters().clone(),
                function.body().clone(),
                state.scope().capture(),
            )
            .into()),
            Expression::Call(call) => {
                let function = call.function().eval(state)?;
                let args = call
                    .args()
                    .iter()
                    .map(|arg| arg.eval(state))
                    .collect::<Result<Vec<_>>>()?;
                function.call(&args, state)
            }
            Expression::PropertyAccess(access) => access.eval(state),
            Expression::Optional(access) => access.eval(state),
            Expression::Assign(assign) => assign.eval(state),
            Expression::Unary(unary) => unary.eval(state),
            Expression::Update(update) => update.eval(state),
            Expression::Binary(binary) => binary.eval(state),
            Expression::Conditional(conditional) => {
                if conditional.condition().eval(state)?.to_bool() {
                    conditional.if_true().eval(state)
                } else {
                    conditional.if_false().eval(state)
                }
            }
            Expression::Parenthesized(parenthesized) => {
                parenthesized.expression().eval(state)
            }

            // ===== UNSUPPORTED =====
            // All AST nodes below are unsupported in our language
            Expression::RegExpLiteral(_) => todo!("not allowed"),
            Expression::ImportCall(_) => Error::unsupported(
                "`import()`",
                "Use the `import` keyword instead",
            ),

            Expression::TaggedTemplate(_) => Error::unsupported(
                "tagged template",
                "For simplicity, tagged templates are not supported",
            ),

            Expression::NewTarget | Expression::ImportMeta => {
                Error::unsupported("TODO", "TODO")
            }

            // async
            Expression::AsyncArrowFunction(_)
            | Expression::AsyncFunctionExpression(_)
            | Expression::Await(_) => Error::unsupported(
                "`async`",
                "All operations must be synchronous",
            ),

            // generator
            Expression::GeneratorExpression(_)
            | Expression::AsyncGeneratorExpression(_)
            | Expression::Yield(_) => todo!("not allowed"),

            // class
            Expression::ClassExpression(_)
            | Expression::This
            | Expression::New(_)
            | Expression::SuperCall(_)
            | Expression::BinaryInPrivate(_) => todo!("not allowed"),

            _ => todo!(),
        }
    }
}

impl Evaluate for Literal {
    fn eval(&self, state: &mut RuntimeState) -> Result<Value> {
        match self {
            Self::Null => Ok(Value::Null),
            Self::Undefined => Ok(Value::Undefined),
            Self::Bool(b) => Ok(Value::Boolean(*b)),
            Self::String(sym) => Ok(state.resolver().resolve(*sym).into()),
            Self::Num(f) => Ok(Value::Number(Number::Float(*f))),
            Self::Int(i) => Ok(Value::Number(Number::Int(*i as i64))),
            Self::BigInt(_) => todo!(),
        }
    }
}

impl Evaluate for TemplateLiteral {
    fn eval(&self, _: &mut RuntimeState) -> Result<Value> {
        todo!()
    }
}

impl Evaluate for ArrayLiteral {
    fn eval(&self, state: &mut RuntimeState) -> Result<Value> {
        let array = self.as_ref().iter().try_fold(
            Array::default(),
            |acc, element| match element {
                // Empty array elements are a goofy feature, and don't serve
                // any purpose with immutable semantics
                None => Error::unsupported(
                    "empty array elements",
                    "Use `undefined` instead",
                ),
                // These are optimized to avoid allocations where possible
                Some(Expression::Spread(spread)) => {
                    let array =
                        spread.target().eval(state)?.try_into_array()?;
                    Ok(acc.insert_all(array))
                }
                Some(expr) => {
                    let value = expr.eval(state)?;
                    Ok(acc.insert(value))
                }
            },
        )?;
        Ok(array.into())
    }
}

impl Evaluate for ObjectLiteral {
    fn eval(&self, state: &mut RuntimeState) -> Result<Value> {
        let object = self.properties().as_ref().iter().try_fold(
            Object::default(),
            |acc, property| match property {
                PropertyDefinition::IdentifierReference(identifier) => {
                    // Shorthand notation: { field }
                    let name =
                        state.resolver().resolve(identifier.sym()).to_owned();
                    let value = state.scope().get(&name)?;
                    Ok::<_, Error>(acc.insert(name, value))
                }
                PropertyDefinition::Property(property_name, expression) => {
                    let name = match property_name {
                        // {field: "value"}
                        PropertyName::Literal(sym) => {
                            state.resolver().resolve(*sym).to_owned()
                        }
                        // {["field"]: "value"}
                        PropertyName::Computed(expression) => {
                            // TODO should we fail for non-string props instead?
                            expression.eval(state)?.to_string()
                        }
                    };
                    let value = expression.eval(state)?;
                    Ok(acc.insert(name, value))
                }
                PropertyDefinition::SpreadObject(expression) => {
                    let object = expression.eval(state)?.try_into_object()?;
                    Ok(acc.insert_all(object))
                }
                PropertyDefinition::MethodDefinition(_) => todo!("not allowed"),
                PropertyDefinition::CoverInitializedName(_, _) => {
                    todo!("wtf is this??")
                }
            },
        )?;
        Ok(object.into())
    }
}

impl Evaluate for Spread {
    fn eval(&self, _: &mut RuntimeState) -> Result<Value> {
        // This should probably error in a general context. Array/object
        // literals can handle it manualy
        todo!()
    }
}

impl Evaluate for PropertyAccess {
    fn eval(&self, state: &mut RuntimeState) -> Result<Value> {
        match self {
            Self::Simple(access) => {
                let value = access.target().eval(state)?;
                let key: Value = match access.field() {
                    PropertyAccessField::Const(symbol) => {
                        state.resolver().resolve(*symbol).into()
                    }
                    PropertyAccessField::Expr(expression) => {
                        expression.eval(state)?
                    }
                };
                value.get(&key)
            }
            Self::Private(_) => todo!("not allowed"),
            Self::Super(_) => todo!("not allowed"),
        }
    }
}

impl Evaluate for Optional {
    fn eval(&self, state: &mut RuntimeState) -> Result<Value> {
        let target = self.target().eval(state)?;
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

impl Evaluate for Assign {
    fn eval(&self, state: &mut RuntimeState) -> Result<Value> {
        let name = match self.lhs() {
            AssignTarget::Identifier(identifier) => {
                state.resolver().resolve(identifier.sym()).to_owned()
            }
            AssignTarget::Access(_) => todo!("not allowed"),
            AssignTarget::Pattern(_) => todo!(),
        };
        let value = self.rhs().eval(state)?;
        match self.op() {
            AssignOp::Assign => {
                state.scope().set(&name, value.clone())?;
                // Return assigned value
                Ok(value)
            }
            AssignOp::Add => todo!(),
            AssignOp::Sub => todo!(),
            AssignOp::Mul => todo!(),
            AssignOp::Div => todo!(),
            AssignOp::Mod => todo!(),
            AssignOp::Exp => todo!(),
            AssignOp::And => todo!(),
            AssignOp::Or => todo!(),
            AssignOp::Xor => todo!(),
            AssignOp::Shl => todo!(),
            AssignOp::Shr => todo!(),
            AssignOp::Ushr => todo!(),
            AssignOp::BoolAnd => todo!(),
            AssignOp::BoolOr => todo!(),
            AssignOp::Coalesce => todo!(),
        }
    }
}

impl Evaluate for Unary {
    fn eval(&self, _: &mut RuntimeState) -> Result<Value> {
        todo!()
    }
}

impl Evaluate for Update {
    fn eval(&self, _: &mut RuntimeState) -> Result<Value> {
        todo!()
    }
}

impl Evaluate for Binary {
    fn eval(&self, state: &mut RuntimeState) -> Result<Value> {
        let lhs = self.lhs().eval(state)?;
        let rhs = self.rhs().eval(state)?;
        match self.op() {
            BinaryOp::Arithmetic(op) => match op {
                ArithmeticOp::Add => Ok(lhs + rhs),
                ArithmeticOp::Sub => Ok(lhs - rhs),
                ArithmeticOp::Mul => Ok(lhs * rhs),
                ArithmeticOp::Div => Ok(lhs / rhs),
                ArithmeticOp::Exp => todo!(),
                ArithmeticOp::Mod => todo!(),
            },
            BinaryOp::Bitwise(op) => match op {
                BitwiseOp::And => todo!(),
                BitwiseOp::Or => todo!(),
                BitwiseOp::Xor => todo!(),
                BitwiseOp::Shl => todo!(),
                BitwiseOp::Shr => todo!(),
                BitwiseOp::UShr => todo!(),
            },
            BinaryOp::Relational(op) => match op {
                RelationalOp::StrictEqual => Ok((lhs == rhs).into()),
                RelationalOp::StrictNotEqual => Ok((lhs != rhs).into()),
                RelationalOp::GreaterThan => todo!(),
                RelationalOp::GreaterThanOrEqual => todo!(),
                RelationalOp::LessThan => todo!(),
                RelationalOp::LessThanOrEqual => todo!(),
                RelationalOp::In => todo!(),

                // UNSUPPORTED OPERATIONS
                // No classes, so no need for instanceof
                RelationalOp::InstanceOf => {
                    Error::unsupported("instanceof", "TODO")
                }
                // Loose equality semantics are dogshit, so just ban em
                RelationalOp::Equal => {
                    Error::unsupported("==", "Use `===` instead")
                }
                RelationalOp::NotEqual => {
                    Error::unsupported("!=", "Use `!==` instead")
                }
            },
            BinaryOp::Logical(op) => match op {
                LogicalOp::And => Ok(lhs.and(&rhs).into()),
                LogicalOp::Or => Ok(lhs.or(&rhs).into()),
                LogicalOp::Coalesce => Ok(lhs.coalesce(rhs)),
            },
            // This just evalutes all expressions and returns the final one
            BinaryOp::Comma => Ok(rhs),
        }
    }
}

impl Value {
    /// If the value is a function, call it and return its return value. If it's
    /// not a function, return an error
    fn call(&self, args: &[Value], state: &mut RuntimeState) -> Result<Value> {
        match self {
            Self::Function(function) => function.call(args, state),
            Self::Native(function) => function.call(args),
            Self::Undefined
            | Self::Null
            | Self::Boolean(_)
            | Self::Number(_)
            | Self::String(_)
            | Self::Array(_)
            | Self::Object(_) => Err(Error::Type {
                expected: ValueType::Function,
                actual: self.type_(),
            }),
        }
    }
}

impl Function {
    fn call(&self, args: &[Value], state: &mut RuntimeState) -> Result<Value> {
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
            } else if let Some(init) = parameter.variable().init() {
                // If the arg wasn't given, fall back to the init expression
                init.eval(state)?
            } else {
                // No init expression, use undefined
                Value::Undefined
            };
            scope.bind(
                state.resolver(),
                parameter.variable().binding(),
                value,
                false,
            )?;
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
