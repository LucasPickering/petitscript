mod scope;

use crate::{
    error::Result,
    runtime::scope::Scope,
    value::{Function, Number, Value, ValueKind},
    Error,
};
use boa_ast::{
    declaration::LexicalDeclaration,
    expression::{
        access::{PropertyAccess, PropertyAccessField},
        literal::Literal,
        Identifier,
    },
    Declaration, Expression, Script, Statement, StatementList,
    StatementListItem,
};
use boa_interner::{Interner, Sym};
use std::ops::Deref;

pub struct Runtime {
    script: Script,
    state: RuntimeState,
}

impl Runtime {
    /// TODO
    pub fn new(interner: Interner, script: Script) -> Runtime {
        Self {
            script,
            state: RuntimeState::new(interner),
        }
    }

    /// TODO
    pub fn run(&mut self) -> Result<()> {
        // TODO what is strict mode?
        self.state.exec_all(self.script.statements())?;
        Ok(())
    }
}

/// TODO
#[derive(Debug)]
struct RuntimeState {
    scope: Scope,
    stack: Vec<StackFrame>,
    interner: Interner,
}

impl RuntimeState {
    fn new(interner: Interner) -> Self {
        Self {
            scope: Scope::new(),
            stack: Vec::new(),
            interner,
        }
    }

    /// TODO
    fn exec_all(
        &mut self,
        statements: &StatementList,
    ) -> Result<Option<Terminate>> {
        for statement in statements.statements() {
            let terminate = match statement {
                StatementListItem::Statement(statement) => {
                    self.exec(statement)?
                }
                StatementListItem::Declaration(declaration) => {
                    self.declare(declaration)?;
                    None
                }
            };
            if let Some(terminate) = terminate {
                return Ok(Some(terminate));
            }
        }
        Ok(None)
    }

    /// TODO
    fn exec(&mut self, statement: &Statement) -> Result<Option<Terminate>> {
        match statement {
            Statement::Block(block) => self.exec_all(block.statement_list()),
            Statement::Empty => Ok(None),
            Statement::Expression(expression) => {
                self.eval(expression)?;
                Ok(None)
            }
            Statement::If(if_statement) => {
                if self.eval(if_statement.cond())?.to_bool() {
                    self.exec(if_statement.body())
                } else if let Some(statement) = if_statement.else_node() {
                    self.exec(statement)
                } else {
                    Ok(None)
                }
            }
            Statement::DoWhileLoop(do_while_loop) => {
                loop {
                    self.exec(do_while_loop.body())?;
                    if !self.eval(do_while_loop.cond())?.to_bool() {
                        break;
                    }
                }
                Ok(None)
            }
            Statement::WhileLoop(while_loop) => {
                while self.eval(while_loop.condition())?.to_bool() {
                    self.exec(while_loop.body())?;
                }
                Ok(None)
            }
            Statement::ForLoop(for_loop) => todo!(),
            Statement::ForInLoop(for_in_loop) => todo!(),
            Statement::ForOfLoop(for_of_loop) => todo!(),
            Statement::Switch(switch) => todo!(),
            // TODO support labels
            Statement::Continue(_) => Ok(Some(Terminate::LoopIteration)),
            Statement::Break(_) => Ok(Some(Terminate::Loop)),
            Statement::Return(ret) => {
                let mut frame = self.stack.pop().ok_or(Error::IllegalReturn)?;
                if let Some(expression) = ret.target() {
                    frame.return_value = self.eval(expression)?;
                }
                // Exit the function
                Ok(Some(Terminate::Function))
            }
            Statement::Labelled(labelled) => todo!(),
            Statement::Throw(throw) => todo!(),
            Statement::Try(_) => todo!(),

            // ===== UNSUPPORTED =====
            // All AST nodes below are unsupported in our language
            Statement::Var(_) => Err(Error::Unsupported {
                name: "`var`",
                help: "Use `let` or `const` instead",
            }),
            Statement::With(_) => Err(Error::Unsupported {
                name: "`with`",
                help: "With blocks are deprecated in ECMAScript",
            }),
        }
    }

    /// Evaluate an expression
    fn eval(&mut self, expression: &Expression) -> Result<Value> {
        match expression {
            Expression::Identifier(identifier) => todo!(),
            Expression::Literal(literal) => match literal {
                Literal::Null => Ok(ValueKind::Null.into()),
                Literal::Undefined => Ok(ValueKind::Undefined.into()),
                Literal::Bool(b) => Ok(ValueKind::Boolean(*b).into()),
                Literal::String(sym) => todo!(),
                Literal::Num(f) => {
                    Ok(ValueKind::Number(Number::Float(*f)).into())
                }
                Literal::Int(i) => {
                    Ok(ValueKind::Number(Number::Int(*i as i64)).into())
                }
                Literal::BigInt(big_int) => todo!(),
            },
            Expression::TemplateLiteral(template_literal) => todo!(),
            Expression::ArrayLiteral(array_literal) => todo!(),
            Expression::ObjectLiteral(object_literal) => todo!(),
            Expression::Spread(spread) => todo!(),
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
                let function = self.eval(call.function())?;
                let args = call
                    .args()
                    .iter()
                    .map(|arg| self.eval(arg))
                    .collect::<Result<_>>()?;
                self.call(&function, args)
            }
            Expression::PropertyAccess(access) => match access {
                PropertyAccess::Simple(access) => {
                    let value = self.eval(access.target())?;
                    Ok(self.access(&value, access.field())?.clone())
                }
                PropertyAccess::Private(_) => todo!("not allowed"),
                PropertyAccess::Super(_) => todo!("not allowed"),
            },
            Expression::Optional(access) => todo!(),
            Expression::Assign(assign) => todo!(),
            Expression::Unary(unary) => todo!(),
            Expression::Update(update) => todo!(),
            Expression::Binary(binary) => todo!(),
            Expression::BinaryInPrivate(binary_in_private) => todo!(),
            Expression::Conditional(conditional) => {
                if self.eval(conditional.condition())?.to_bool() {
                    self.eval(conditional.if_true())
                } else {
                    self.eval(conditional.if_false())
                }
            }
            Expression::Parenthesized(parenthesized) => {
                self.eval(parenthesized.expression())
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
            | Expression::SuperCall(_) => todo!("not allowed"),

            _ => todo!(),
        }
    }

    /// TODO
    fn declare(&mut self, declaration: &Declaration) -> Result<()> {
        match declaration {
            Declaration::FunctionDeclaration(function_declaration) => todo!(),
            Declaration::Lexical(lexical_declaration) => {
                let (variables, mutable) = match lexical_declaration {
                    LexicalDeclaration::Const(variable_list) => {
                        (variable_list, false)
                    }
                    LexicalDeclaration::Let(variable_list) => {
                        (variable_list, true)
                    }
                };
                self.scope.declare_all(variables, mutable);
                Ok(())
            }
            // ===== UNSUPPORTED =====
            // All AST nodes below are unsupported in our language
            Declaration::ClassDeclaration(_) => todo!("not allowed"),
            Declaration::AsyncFunctionDeclaration(_)
            | Declaration::AsyncGeneratorDeclaration(_) => {
                Err(Error::Unsupported {
                    name: "`async`",
                    help: "All operations must be synchronous",
                })
            }
            Declaration::GeneratorDeclaration(_) => todo!("not allowed"),
        }
    }

    /// TODO
    fn access<'a>(
        &mut self,
        value: &'a Value,
        access: &PropertyAccessField,
    ) -> Result<&'a Value> {
        let key: Value = match access {
            PropertyAccessField::Const(symbol) => {
                self.resolve_sym(*symbol).into()
            }
            PropertyAccessField::Expr(expression) => self.eval(expression)?,
        };
        value.get(&key)
    }

    /// Call a function and return its return value
    fn call(&mut self, function: &Value, args: Vec<Value>) -> Result<Value> {
        let ValueKind::Function(function) = function.deref() else {
            todo!("error")
        };

        // Create a new stack frame and execute the fn body. Function is
        // responsible for setting the return value in its frame
        self.stack.push(StackFrame::new(self.scope.child()));
        // TODO add args to scope
        self.exec_all(function.body.statement_list())?;
        let frame = self.stack.pop().expect("Just pushed to stack");
        Ok(frame.return_value)
    }

    /// TODO
    fn resolve_sym(&self, symbol: Sym) -> &str {
        self.interner.resolve_expect(symbol).utf8().expect("TODO")
    }
}

/// TODO
#[derive(Debug)]
struct StackFrame {
    scope: Scope,
    /// Slot where the return value is stored imperatively. This is EAX!
    /// Default return value is `undefined`.
    return_value: Value,
}

impl StackFrame {
    fn new(global_scope: Scope) -> Self {
        Self {
            scope: global_scope.child(),
            return_value: ValueKind::Undefined.into(),
        }
    }
}

/// TODO
#[derive(Copy, Clone, Debug)]
#[must_use]
enum Terminate {
    LoopIteration,
    Loop,
    Function,
}
