//! Expression evaluation

use crate::{
    runtime::RuntimeState,
    value::{Function, Number, Value, ValueKind},
    Error, Result,
};
use boa_ast::{
    expression::{
        access::{PropertyAccess, PropertyAccessField},
        literal::{ArrayLiteral, Literal, ObjectLiteral, TemplateLiteral},
        operator::{
            binary::{ArithmeticOp, BinaryOp},
            Assign, Binary, Unary, Update,
        },
        Optional, Spread,
    },
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
                let name = state.resolve_sym(identifier.sym())?;
                state.scope.get(name)
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
            Expression::FunctionExpression(function_expression) => {
                Ok(ValueKind::Function(Function {
                    name: None, // TODO
                    parameters: function_expression.parameters().clone(),
                    body: function_expression.body().clone(),
                })
                .into())
            }
            Expression::ArrowFunction(arrow_function) => {
                Ok(ValueKind::Function(Function {
                    name: None, // TODO
                    parameters: arrow_function.parameters().clone(),
                    body: arrow_function.body().clone(),
                })
                .into())
            }
            Expression::Call(call) => {
                let function = call.function().eval(state)?;
                let args = call
                    .args()
                    .iter()
                    .map(|arg| arg.eval(state))
                    .collect::<Result<_>>()?;
                state.call(&function, args)
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
            Self::Null => Ok(ValueKind::Null.into()),
            Self::Undefined => Ok(ValueKind::Undefined.into()),
            Self::Bool(b) => Ok(ValueKind::Boolean(*b).into()),
            Self::String(sym) => state.resolve_sym(*sym).map(Value::from),
            Self::Num(f) => Ok(ValueKind::Number(Number::Float(*f)).into()),
            Self::Int(i) => {
                Ok(ValueKind::Number(Number::Int(*i as i64)).into())
            }
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
        // TODO handle spread
        // Collect into a vec, then pull the slice out. Since the iterator is
        // a known size, this should allocate only as much capacity as we need
        let vec = self
            .as_ref()
            .iter()
            // Empty array elements are a goofy feature, and don't serve any
            // purpose with immutable semantics
            .map(|element| {
                element
                    .as_ref()
                    .ok_or(Error::Unsupported {
                        name: "empty array elements",
                        help: "Use `undefined` instead",
                    })?
                    .eval(state)
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(ValueKind::Array(vec.into()).into())
    }
}

impl Evaluate for ObjectLiteral {
    fn eval(&self, state: &mut RuntimeState) -> Result<Value> {
        todo!()
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
                        state.resolve_sym(*symbol)?.into()
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
        todo!()
    }
}

impl Evaluate for Assign {
    fn eval(&self, state: &mut RuntimeState) -> Result<Value> {
        todo!()
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
