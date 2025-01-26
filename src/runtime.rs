mod eval;
mod scope;

use crate::{
    error::Result,
    runtime::{eval::Evaluate, scope::Scope},
    value::Value,
    Error,
};
use boa_ast::{
    declaration::{Binding, LexicalDeclaration, VariableList},
    Declaration, Script, Statement, StatementList, StatementListItem,
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
                // Expression may have side effects, so evaluate it and throw
                // away the outcome
                expression.eval(self)?;
                Ok(None)
            }
            Statement::If(if_statement) => {
                if if_statement.cond().eval(self)?.to_bool() {
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
                    if !do_while_loop.cond().eval(self)?.to_bool() {
                        break;
                    }
                }
                Ok(None)
            }
            Statement::WhileLoop(while_loop) => {
                while while_loop.condition().eval(self)?.to_bool() {
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
                    frame.return_value = expression.eval(self)?;
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
                self.declare_all(variables, mutable)?;
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

    // TODO
    fn declare_all(
        &mut self,
        variables: &VariableList,
        mutable: bool,
    ) -> Result<()> {
        for variable in variables.as_ref() {
            let value = variable
                .init()
                .map(|expr| expr.eval(self))
                .transpose()?
                .unwrap_or_default();
            match variable.binding() {
                Binding::Identifier(identifier) => {
                    let name = self.resolve_sym(identifier.sym())?.to_owned();
                    self.scope.declare(name, value, mutable);
                }
                Binding::Pattern(pattern) => todo!(),
            }
        }
        Ok(())
    }

    /// TODO
    fn resolve_sym(&self, symbol: Sym) -> Result<&str> {
        self.interner
            .resolve_expect(symbol)
            .utf8()
            .ok_or_else(|| todo!())
    }

    /// Call a function and return its return value
    fn call(&mut self, function: &Value, args: Vec<Value>) -> Result<Value> {
        let Value::Function(function) = function.deref() else {
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
            return_value: Value::Undefined,
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
