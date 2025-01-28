//! Expression evaluation

use crate::{
    runtime::{
        exec::{Execute, Terminate},
        RuntimeState,
    },
    value::{Array, Function, Number, Object, Value},
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
            binary::{ArithmeticOp, BinaryOp},
            Assign, Binary, Unary, Update,
        },
        Optional, Spread,
    },
    property::PropertyName,
    Expression,
};

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
                let name = state.resolve_sym(identifier.sym());
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
                function
                    .name()
                    .map(|name| state.resolve_sym(name.sym()).to_owned()),
                function.parameters().clone(),
                function.body().clone(),
                state.scope().clone(),
            )
            .into()),
            Expression::ArrowFunction(function) => Ok(Function::new(
                function
                    .name()
                    .map(|name| state.resolve_sym(name.sym()).to_owned()),
                function.parameters().clone(),
                function.body().clone(),
                state.scope().clone(),
            )
            .into()),
            Expression::Call(call) => {
                let function =
                    call.function().eval(state)?.try_into_function()?;
                let args = call
                    .args()
                    .iter()
                    .map(|arg| arg.eval(state))
                    .collect::<Result<_>>()?;
                function.call(args, state)
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
            Expression::ImportCall(_) => Err(Error::Unsupported {
                name: "`import()`",
                help: "Use the `import` keyword instead",
            }),

            Expression::TaggedTemplate(_) => Err(Error::Unsupported {
                name: "tagged template",
                help: "For simplicity, tagged templates are not supported",
            }),

            Expression::NewTarget | Expression::ImportMeta => {
                Err(Error::Unsupported {
                    name: "TODO",
                    help: "TODO",
                })
            }

            // async
            Expression::AsyncArrowFunction(_)
            | Expression::AsyncFunctionExpression(_)
            | Expression::Await(_) => Err(Error::Unsupported {
                name: "`async`",
                help: "All operations must be synchronous",
            }),

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
            Self::String(sym) => Ok(state.resolve_sym(*sym).into()),
            Self::Num(f) => Ok(Value::Number(Number::Float(*f))),
            Self::Int(i) => Ok(Value::Number(Number::Int(*i as i64))),
            Self::BigInt(big_int) => todo!(),
        }
    }
}

impl Evaluate for TemplateLiteral {
    fn eval(&self, state: &mut RuntimeState) -> Result<Value> {
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
                None => Err(Error::Unsupported {
                    name: "empty array elements",
                    help: "Use `undefined` instead",
                }),
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
                    let name = state.resolve_sym(identifier.sym()).to_owned();
                    let value = state.scope().get(&name)?;
                    Ok::<_, Error>(acc.insert(name, value))
                }
                PropertyDefinition::Property(property_name, expression) => {
                    let name = match property_name {
                        // {field: "value"}
                        PropertyName::Literal(sym) => {
                            state.resolve_sym(*sym).to_owned()
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
    fn eval(&self, state: &mut RuntimeState) -> Result<Value> {
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
                        state.resolve_sym(*symbol).into()
                    }
                    PropertyAccessField::Expr(expression) => {
                        expression.eval(state)?
                    }
                };
                value.get(&key).cloned()
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
            | Value::Function(_) => todo!(),
        }
    }
}

impl Evaluate for Assign {
    fn eval(&self, state: &mut RuntimeState) -> Result<Value> {
        let name = match self.lhs() {
            AssignTarget::Identifier(identifier) => {
                state.resolve_sym(identifier.sym()).to_owned()
            }
            AssignTarget::Access(property_access) => todo!("not allowed"),
            AssignTarget::Pattern(pattern) => todo!(),
        };
        let value = self.rhs().eval(state)?;
        match self.op() {
            AssignOp::Assign => {
                state.scope_mut().set(&name, value.clone())?;
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
    fn eval(&self, state: &mut RuntimeState) -> Result<Value> {
        todo!()
    }
}

impl Evaluate for Update {
    fn eval(&self, state: &mut RuntimeState) -> Result<Value> {
        todo!()
    }
}

impl Evaluate for Binary {
    fn eval(&self, state: &mut RuntimeState) -> Result<Value> {
        let lhs = self.lhs().eval(state)?;
        let rhs = self.rhs().eval(state)?;
        match self.op() {
            BinaryOp::Arithmetic(arithmetic_op) => match arithmetic_op {
                // TODO this isn't right
                ArithmeticOp::Add => match (lhs.to_number(), rhs.to_number()) {
                    (Some(lhs), Some(rhs)) => Ok((lhs + rhs).into()),
                    _ => todo!(),
                },
                ArithmeticOp::Sub => todo!(),
                ArithmeticOp::Div => todo!(),
                ArithmeticOp::Mul => todo!(),
                ArithmeticOp::Exp => todo!(),
                ArithmeticOp::Mod => todo!(),
            },
            BinaryOp::Bitwise(bitwise_op) => todo!(),
            BinaryOp::Relational(relational_op) => todo!(),
            BinaryOp::Logical(logical_op) => todo!(),
            BinaryOp::Comma => todo!(),
        }
    }
}

impl Function {
    /// Call the function with the given arguments, and return its return value
    fn call(
        &self,
        args: Vec<Value>,
        state: &mut RuntimeState,
    ) -> Result<Value> {
        // Start with the function's captured scope
        let scope = self.scope().child();
        // Add args to scope
        // TODO add args to scope

        // TODO use guard pattern for this instead
        state.push_scope(scope);
        let return_value = match self.body().exec(state)? {
            Some(Terminate::Return {
                return_value: Some(return_value),
            }) => return_value,
            _ => Value::Undefined,
        };
        state.pop_scope();

        Ok(return_value)
    }
}
