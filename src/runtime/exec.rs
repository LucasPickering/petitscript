//! Statement execution

use crate::{
    ast::{
        Block, Declaration, ExportDeclaration, FunctionDeclaration, Identifier,
        ImportDeclaration, Label, LexicalDeclaration, Statement,
    },
    error::RuntimeResult,
    runtime::{eval::Evaluate, state::RuntimeState},
    value::{Function, Value},
};

/// TODO
pub trait Execute {
    type Output;

    fn exec(&self, state: &mut RuntimeState) -> RuntimeResult<Self::Output>;
}

impl<T, O> Execute for [T]
where
    T: Execute<Output = Option<O>>,
{
    type Output = Option<O>;

    fn exec(&self, state: &mut RuntimeState) -> RuntimeResult<Option<O>> {
        for statement in self {
            let output = statement.exec(state)?;
            if let Some(output) = output {
                return Ok(Some(output));
            }
        }
        Ok(None)
    }
}

impl Execute for Statement {
    /// We can break out of a loop iter, loop, or fn from here
    type Output = Option<Terminate>;

    fn exec(
        &self,
        state: &mut RuntimeState,
    ) -> RuntimeResult<Option<Terminate>> {
        match self {
            Self::Block(block) => {
                // A block gets a new lexical scope
                state.with_subscope(|state| block.statements.exec(state))
            }
            Self::Expression(expression) => {
                // Expression may have side effects, so evaluate it and throw
                // away the outcome
                expression.eval(state)?;
                Ok(None)
            }
            Self::Declaration(declaration) => {
                declaration.exec(state)?;
                Ok(None)
            }

            Self::If(if_statement) => {
                if if_statement.condition.eval(state)?.to_bool() {
                    if_statement.body.exec(state)
                } else if let Some(statement) = &if_statement.else_body {
                    statement.exec(state)
                } else {
                    Ok(None)
                }
            }
            Self::DoWhileLoop(do_while_loop) => {
                loop {
                    do_while_loop.body.exec(state)?;
                    if !do_while_loop.condition.eval(state)?.to_bool() {
                        break;
                    }
                }
                Ok(None)
            }
            Self::WhileLoop(while_loop) => {
                dbg!(while_loop);
                while while_loop.condition.eval(state)?.to_bool() {
                    while_loop.body.exec(state)?;
                }
                Ok(None)
            }
            Self::ForLoop(_) => todo!(),
            Self::ForOfLoop(_) => todo!(),

            Self::Continue(label) => Ok(Some(Terminate::Continue {
                label: label.clone(),
            })),
            Self::Break(label) => Ok(Some(Terminate::Break {
                label: label.clone(),
            })),
            Self::Return(expression) => {
                let return_value = expression
                    .as_ref()
                    .map(|expression| expression.eval(state))
                    .transpose()?;
                Ok(Some(Terminate::Return { return_value }))
            }

            Self::Import(import) => {
                import.exec(state)?;
                Ok(None)
            }
            Self::Export(export) => {
                export.exec(state)?;
                Ok(None)
            }
        }
    }
}

impl Execute for Block {
    type Output = Option<Terminate>;

    fn exec(
        &self,
        state: &mut RuntimeState,
    ) -> RuntimeResult<Option<Terminate>> {
        for statement in &self.statements {
            if let Some(terminate) = statement.exec(state)? {
                return Ok(Some(terminate));
            }
        }
        Ok(None)
    }
}

impl Execute for ImportDeclaration {
    type Output = ();

    fn exec(&self, _: &mut RuntimeState) -> RuntimeResult<()> {
        todo!()
    }
}

impl Execute for ExportDeclaration {
    type Output = ();

    fn exec(&self, state: &mut RuntimeState) -> RuntimeResult<()> {
        match self {
            ExportDeclaration::Reexport { .. } => todo!(),
            ExportDeclaration::Declaration(declaration) => {
                for name in declaration.exec(state)? {
                    state.export(name)?;
                }
                Ok(())
            }
            ExportDeclaration::DefaultFunctionDeclaration(
                function_declaration,
            ) => {
                let name = function_declaration.exec(state)?.expect("TODO");
                // Fetch the function we just declared. Should never fail
                let value = state.scope().get(&name)?;
                state.export_default(value)
            }
            ExportDeclaration::DefaultExpression(expression) => {
                let value = expression.eval(state)?;
                state.export_default(value)
            }
        }
    }
}

impl Execute for Declaration {
    type Output = Vec<String>;

    fn exec(&self, state: &mut RuntimeState) -> RuntimeResult<Self::Output> {
        match self {
            Self::Lexical(lexical_declaration) => {
                lexical_declaration.exec(state)
            }
            Self::Function(function_declaration) => {
                let name = function_declaration.exec(state)?.expect("TODO");
                Ok(vec![name])
            }
        }
    }
}

impl Execute for LexicalDeclaration {
    /// Return the list of declared names
    type Output = Vec<String>;

    fn exec(&self, state: &mut RuntimeState) -> RuntimeResult<Vec<String>> {
        // Track the list of declared names
        let mut declared = Vec::with_capacity(self.variables.len());
        for variable in &self.variables {
            let value = variable
                .init
                .as_ref()
                .map(|expr| expr.eval(state))
                .transpose()?
                .unwrap_or_default();
            let names = state.scope_mut().bind(
                &variable.binding,
                value,
                self.mutable,
            )?;
            declared.extend(names);
        }

        Ok(declared)
    }
}

impl Execute for FunctionDeclaration {
    /// Emit declared name
    type Output = Option<String>;

    fn exec(&self, state: &mut RuntimeState) -> RuntimeResult<Self::Output> {
        let name = self.name.as_ref().map(Identifier::to_str).map(String::from);
        let scope = state.scope_mut();
        let function = Function::new(
            name.clone(),
            self.parameters.clone(),
            self.body.clone(),
            scope.clone(),
        );

        if let Some(name) = &name {
            scope.declare(name.clone(), function.into(), false);
        }
        Ok(name)
    }
}

/// TODO
#[derive(Debug)]
#[must_use]
pub enum Terminate {
    /// Skip to the entire of the current loop iteration
    Continue {
        /// Name the loop to skip to the end of
        label: Option<Label>,
    },
    /// Break out of the current loop
    Break {
        /// Name the loop to skip to break
        label: Option<Label>,
    },
    /// Return from the current function, optionally with a return value
    Return { return_value: Option<Value> },
}
