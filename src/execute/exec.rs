//! Statement execution

use crate::{
    Exports,
    ast::{
        Block, Declaration, ExportDeclaration, ExportDeclarationKind,
        FunctionBody, If, ImportDeclaration, ImportModule, ImportModuleKind,
        Module, Node, Statement, StatementKind, Variable,
    },
    error::{RuntimeError, RuntimeErrorKind},
    execute::{Fiber, eval::Evaluate},
    util::OptionExt,
    value::Value,
};

/// Execute an imperative step in a program
pub trait Execute: Node {
    /// Outcome of the execution
    ///
    /// Execution does *not* emit a value within the semantics of PS. The
    /// [Execute::Output] return type facilitates control flow (e.g. `return`).
    type Output<'process>;

    /// Execute the statement, modifying state appropriately
    fn exec<'process>(
        &self,
        state: &mut Fiber<'process>,
    ) -> Result<Self::Output<'process>, RuntimeError>;
}

impl Execute for Module {
    type Output<'process> = ();

    fn exec(&self, state: &mut Fiber) -> Result<(), RuntimeError> {
        exec_all(state, &self.statements)?;
        Ok(())
    }
}

impl Execute for Statement {
    /// We can break out of a loop iter, loop, or fn from here
    type Output<'process> = Option<Return>;

    fn exec(&self, state: &mut Fiber) -> Result<Option<Return>, RuntimeError> {
        match &self.kind {
            StatementKind::Empty => Ok(None),
            StatementKind::Block(block) => {
                // A block gets a new lexical scope
                state.with_subscope(|state| exec_all(state, &block.statements))
            }
            StatementKind::Expression(expression) => {
                // Expression may have side effects, so evaluate it and throw
                // away the outcome
                expression.eval(state)?;
                Ok(None)
            }
            StatementKind::Declaration(declaration) => {
                declaration.exec(state)?;
                Ok(None)
            }

            StatementKind::If(if_statement) => if_statement.exec(state),
            StatementKind::Return(expression) => {
                let return_value = if let Some(expression) = expression {
                    Some(expression.eval(state)?)
                } else {
                    None
                };
                Ok(Some(Return(return_value)))
            }

            StatementKind::Import(import) => {
                import.exec(state)?;
                Ok(None)
            }
            StatementKind::Export(export) => {
                export.exec(state)?;
                Ok(None)
            }
        }
    }
}

impl Execute for Block {
    type Output<'process> = Option<Return>;

    fn exec(&self, state: &mut Fiber) -> Result<Option<Return>, RuntimeError> {
        for statement in &self.statements {
            if let Some(terminate) = statement.exec(state)? {
                return Ok(Some(terminate));
            }
        }
        Ok(None)
    }
}

impl Execute for ImportDeclaration {
    type Output<'process> = ();

    fn exec(&self, state: &mut Fiber) -> Result<(), RuntimeError> {
        let mut exports = self.module.exec(state)?;

        if let Some(name) = &self.default {
            let value = exports.default.ok_or_else(|| {
                state.trace_error(
                    RuntimeErrorKind::UnknownImport { identifier: None },
                    name,
                )
            })?;
            state.scope_mut().declare(name.as_str(), value);
        }
        for named in &self.named {
            let value = exports
                .named
                .swap_remove(named.imported.as_str())
                .ok_or_else(|| {
                    state.trace_error(
                        RuntimeErrorKind::UnknownImport {
                            identifier: Some(named.imported.to_string()),
                        },
                        named,
                    )
                })?;
            state.scope_mut().declare(named.local.as_str(), value);
        }

        Ok(())
    }
}

impl Execute for ImportModule {
    type Output<'process> = Exports;

    /// Resolve a module name/path into its exports. For native modules this
    /// just grabs the exports from the registry. For paths, it will execute
    /// the external program
    fn exec(&self, state: &mut Fiber) -> Result<Exports, RuntimeError> {
        match &self.kind {
            ImportModuleKind::Native(name) => state
                .process()
                .get_native_module(name)
                // The clone is not strictly necessary, but the consuming logic
                // gets a lot more complicated if we don't
                .cloned()
                .map_err(|error| state.trace_error(error, self)),
            ImportModuleKind::Local(ast) => {
                let mut fiber = state.process().create_fiber();
                ast.exec(&mut fiber)?;
                let exports = fiber.into_exports().unwrap();
                Ok(exports)
            }
        }
    }
}

impl Execute for ExportDeclaration {
    type Output<'process> = ();

    fn exec(&self, state: &mut Fiber) -> Result<(), RuntimeError> {
        match &self.kind {
            ExportDeclarationKind::Declaration(declaration) => {
                for name in declaration.exec(state)? {
                    state.export(name).map_err(|error| {
                        state.trace_error(error, declaration)
                    })?;
                }
                Ok(())
            }
            ExportDeclarationKind::DefaultExpression(expression) => {
                let value = expression.eval(state)?;
                state
                    .export_default(value)
                    .map_err(|error| state.trace_error(error, self))
            }
        }
    }
}

impl Execute for If {
    type Output<'process> = Option<Return>;

    fn exec(&self, state: &mut Fiber) -> Result<Option<Return>, RuntimeError> {
        if self.condition.eval(state)?.to_bool() {
            self.body.exec(state)
        } else if let Some(statement) = &self.else_body {
            statement.exec(state)
        } else {
            Ok(None)
        }
    }
}

impl Execute for Declaration {
    /// Return the list of declared names
    type Output<'process> = Vec<String>;

    fn exec(&self, state: &mut Fiber) -> Result<Vec<String>, RuntimeError> {
        // Track the list of declared names
        let mut declared = Vec::with_capacity(self.variables.len());
        for variable in &self.variables {
            declared.extend(variable.exec(state)?);
        }

        Ok(declared)
    }
}

impl Execute for Variable {
    /// Return the list of declared names
    type Output<'process> = Vec<String>;

    fn exec<'process>(
        &self,
        state: &mut Fiber<'process>,
    ) -> Result<Self::Output<'process>, RuntimeError> {
        let value = self
            .init
            .as_ref()
            .try_map(|init| init.eval(state))?
            .unwrap_or(Value::Undefined);
        state.bind(&self.binding, value)
    }
}

impl Execute for FunctionBody {
    /// Function return value
    type Output<'process> = Value;

    fn exec<'process>(
        &self,
        state: &mut Fiber<'process>,
    ) -> Result<Self::Output<'process>, RuntimeError> {
        match self {
            Self::Expression(expression) => expression.eval(state),
            Self::Block(block) => match block.exec(state)? {
                Some(Return(Some(return_value))) => Ok(return_value),
                _ => Ok(Value::Undefined),
            },
        }
    }
}

/// Return from the current function, optionally with a return value
#[derive(Debug)]
#[must_use]
pub struct Return(Option<Value>);

/// Execute all nodes in a sequence, in order
fn exec_all(
    state: &mut Fiber,
    statements: &[Statement],
) -> Result<Option<Return>, RuntimeError> {
    for statement in statements {
        let output = statement.exec(state)?;
        if let Some(output) = output {
            return Ok(Some(output));
        }
    }
    Ok(None)
}
