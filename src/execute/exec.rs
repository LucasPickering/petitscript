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
    type Output<'world>;

    /// Execute the statement, modifying state appropriately
    fn exec<'world>(
        &self,
        fiber: &mut Fiber<'world>,
    ) -> Result<Self::Output<'world>, RuntimeError>;
}

impl Execute for Module {
    type Output<'world> = ();

    fn exec(&self, fiber: &mut Fiber) -> Result<(), RuntimeError> {
        exec_all(fiber, &self.statements)?;
        Ok(())
    }
}

impl Execute for Statement {
    /// We can break out of a loop iter, loop, or fn from here
    type Output<'world> = Option<Return>;

    fn exec(&self, fiber: &mut Fiber) -> Result<Option<Return>, RuntimeError> {
        match &self.kind {
            StatementKind::Empty => Ok(None),
            StatementKind::Block(block) => {
                // A block gets a new lexical scope
                fiber.with_subscope(|state| exec_all(state, &block.statements))
            }
            StatementKind::Expression(expression) => {
                // Expression may have side effects, so evaluate it and throw
                // away the outcome
                expression.eval(fiber)?;
                Ok(None)
            }
            StatementKind::Declaration(declaration) => {
                declaration.exec(fiber)?;
                Ok(None)
            }

            StatementKind::If(if_statement) => if_statement.exec(fiber),
            StatementKind::Return(expression) => {
                let return_value = if let Some(expression) = expression {
                    Some(expression.eval(fiber)?)
                } else {
                    None
                };
                Ok(Some(Return(return_value)))
            }

            StatementKind::Import(import) => {
                import.exec(fiber)?;
                Ok(None)
            }
            StatementKind::Export(export) => {
                export.exec(fiber)?;
                Ok(None)
            }
        }
    }
}

impl Execute for Block {
    type Output<'world> = Option<Return>;

    fn exec(&self, fiber: &mut Fiber) -> Result<Option<Return>, RuntimeError> {
        for statement in &self.statements {
            if let Some(terminate) = statement.exec(fiber)? {
                return Ok(Some(terminate));
            }
        }
        Ok(None)
    }
}

impl Execute for ImportDeclaration {
    type Output<'world> = ();

    fn exec(&self, fiber: &mut Fiber) -> Result<(), RuntimeError> {
        let mut exports = self.module.exec(fiber)?;

        if let Some(name) = &self.default {
            let value = exports.default.ok_or_else(|| {
                fiber.trace_error(
                    RuntimeErrorKind::UnknownImport { identifier: None },
                    name,
                )
            })?;
            fiber.scope_mut().declare(name.as_str(), value);
        }
        for named in &self.named {
            let value = exports
                .named
                .swap_remove(named.imported.as_str())
                .ok_or_else(|| {
                    fiber.trace_error(
                        RuntimeErrorKind::UnknownImport {
                            identifier: Some(named.imported.to_string()),
                        },
                        named,
                    )
                })?;
            fiber.scope_mut().declare(named.local.as_str(), value);
        }

        Ok(())
    }
}

impl Execute for ImportModule {
    type Output<'world> = Exports;

    /// Resolve a module name/path into its exports
    ///
    /// For native modules this just grabs the exports from the registry. For
    /// paths, it will execute the external program.
    fn exec(&self, fiber: &mut Fiber) -> Result<Exports, RuntimeError> {
        match &self.kind {
            ImportModuleKind::Native(name) => fiber
                .world()
                .get_native_module(name)
                // The clone is not strictly necessary, but the consuming logic
                // gets a lot more complicated if we don't
                .cloned()
                .map_err(|error| fiber.trace_error(error, self)),
            ImportModuleKind::Local { source, .. } => {
                let module = fiber
                    .program()
                    .get_module(*source)
                    .expect("TODO error - miscompiled program");
                // Spawn another fiber
                let mut fiber = fiber.world().create_fiber();
                module.exec(&mut fiber)?;
                let exports = fiber.into_exports().unwrap();
                Ok(exports)
            }
        }
    }
}

impl Execute for ExportDeclaration {
    type Output<'world> = ();

    fn exec(&self, fiber: &mut Fiber) -> Result<(), RuntimeError> {
        match &self.kind {
            ExportDeclarationKind::Declaration(declaration) => {
                for name in declaration.exec(fiber)? {
                    fiber.export(name).map_err(|error| {
                        fiber.trace_error(error, declaration)
                    })?;
                }
                Ok(())
            }
            ExportDeclarationKind::DefaultExpression(expression) => {
                let value = expression.eval(fiber)?;
                fiber
                    .export_default(value)
                    .map_err(|error| fiber.trace_error(error, self))
            }
        }
    }
}

impl Execute for If {
    type Output<'world> = Option<Return>;

    fn exec(&self, fiber: &mut Fiber) -> Result<Option<Return>, RuntimeError> {
        if self.condition.eval(fiber)?.to_bool() {
            self.body.exec(fiber)
        } else if let Some(statement) = &self.else_body {
            statement.exec(fiber)
        } else {
            Ok(None)
        }
    }
}

impl Execute for Declaration {
    /// Return the list of declared names
    type Output<'world> = Vec<String>;

    fn exec(&self, fiber: &mut Fiber) -> Result<Vec<String>, RuntimeError> {
        // Track the list of declared names
        let mut declared = Vec::with_capacity(self.variables.len());
        for variable in &self.variables {
            declared.extend(variable.exec(fiber)?);
        }

        Ok(declared)
    }
}

impl Execute for Variable {
    /// Return the list of declared names
    type Output<'world> = Vec<String>;

    fn exec<'world>(
        &self,
        fiber: &mut Fiber<'world>,
    ) -> Result<Self::Output<'world>, RuntimeError> {
        let value = self
            .init
            .as_ref()
            .try_map(|init| init.eval(fiber))?
            .unwrap_or(Value::Undefined);
        fiber.bind(&self.binding, value)
    }
}

impl Execute for FunctionBody {
    /// Function return value
    type Output<'world> = Value;

    fn exec<'world>(
        &self,
        fiber: &mut Fiber<'world>,
    ) -> Result<Self::Output<'world>, RuntimeError> {
        match self {
            Self::Expression(expression) => expression.eval(fiber),
            Self::Block(block) => match block.exec(fiber)? {
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
    fiber: &mut Fiber,
    statements: &[Statement],
) -> Result<Option<Return>, RuntimeError> {
    for statement in statements {
        let output = statement.exec(fiber)?;
        if let Some(output) = output {
            return Ok(Some(output));
        }
    }
    Ok(None)
}
