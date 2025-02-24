//! Statement execution

use crate::{
    ast::{
        Ast, Block, Declaration, DoWhileLoop, ExportDeclaration, ForLoop,
        ForOfLoop, FunctionDeclaration, If, ImportDeclaration,
        LexicalDeclaration, Spanned, Statement, WhileLoop,
    },
    error::ResultExt,
    execute::{eval::Evaluate, ThreadState},
    value::Value,
    Error,
};

/// TODO
pub trait Execute {
    type Output;

    fn exec(&self, state: &mut ThreadState) -> Result<Self::Output, Error>;
}

impl<T, O> Execute for [T]
where
    T: Execute<Output = Option<O>>,
{
    type Output = Option<O>;

    fn exec(&self, state: &mut ThreadState<'_>) -> Result<Option<O>, Error> {
        for statement in self {
            let output = statement.exec(state)?;
            if let Some(output) = output {
                return Ok(Some(output));
            }
        }
        Ok(None)
    }
}

impl Execute for Ast {
    type Output = ();

    fn exec(&self, state: &mut ThreadState) -> Result<Self::Output, Error> {
        self.statements.exec(state)?;
        Ok(())
    }
}

impl Execute for Spanned<Statement> {
    /// We can break out of a loop iter, loop, or fn from here
    type Output = Option<Terminate>;

    fn exec(
        &self,
        state: &mut ThreadState<'_>,
    ) -> Result<Option<Terminate>, Error> {
        match &self.data {
            Statement::Empty => Ok(None),
            Statement::Block(block) => {
                // A block gets a new lexical scope
                state.with_subscope(|state| block.statements.exec(state))
            }
            Statement::Expression(expression) => {
                // Expression may have side effects, so evaluate it and throw
                // away the outcome
                expression.eval(state)?;
                Ok(None)
            }
            Statement::Declaration(declaration) => {
                declaration.exec(state)?;
                Ok(None)
            }

            Statement::If(if_statement) => if_statement.exec(state),
            Statement::DoWhileLoop(do_while_loop) => do_while_loop.exec(state),
            Statement::WhileLoop(while_loop) => while_loop.exec(state),
            Statement::ForLoop(for_loop) => for_loop.exec(state),
            Statement::ForOfLoop(for_of_loop) => for_of_loop.exec(state),

            Statement::Continue => Ok(Some(Terminate::Continue)),
            Statement::Break => Ok(Some(Terminate::Break)),
            Statement::Return(expression) => {
                let return_value = if let Some(expression) = expression {
                    Some(expression.eval(state)?)
                } else {
                    None
                };
                Ok(Some(Terminate::Return { return_value }))
            }

            Statement::Import(import) => {
                import.exec(state)?;
                Ok(None)
            }
            Statement::Export(export) => {
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
        state: &mut ThreadState<'_>,
    ) -> Result<Option<Terminate>, Error> {
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

    fn exec(&self, _: &mut ThreadState<'_>) -> Result<(), Error> {
        todo!()
    }
}

impl Execute for Spanned<ExportDeclaration> {
    type Output = ();

    fn exec(&self, state: &mut ThreadState<'_>) -> Result<(), Error> {
        match &self.data {
            ExportDeclaration::Reexport { .. } => todo!(),
            ExportDeclaration::Declaration(declaration) => {
                for name in declaration.exec(state)? {
                    state.export(name).spanned_err(declaration.span)?;
                }
                Ok(())
            }
            ExportDeclaration::DefaultFunctionDeclaration(
                function_declaration,
            ) => {
                let name = function_declaration.exec(state)?.expect("TODO");
                // Fetch the function we just declared. Should never fail
                let value = state
                    .scope()
                    .get(name.as_str())
                    .spanned_err(function_declaration.span)?;
                state.export_default(value).spanned_err(self.span)
            }
            ExportDeclaration::DefaultExpression(expression) => {
                let value = expression.eval(state)?;
                state.export_default(value).spanned_err(self.span)
            }
        }
    }
}

impl Execute for Declaration {
    /// Return the list of declared names
    type Output = Vec<String>;

    fn exec(&self, state: &mut ThreadState<'_>) -> Result<Self::Output, Error> {
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

impl Execute for If {
    type Output = Option<Terminate>;

    fn exec(&self, state: &mut ThreadState) -> Result<Self::Output, Error> {
        if self.condition.eval(state)?.to_bool() {
            self.body.exec(state)
        } else if let Some(statement) = &self.else_body {
            statement.exec(state)
        } else {
            Ok(None)
        }
    }
}

impl Execute for DoWhileLoop {
    type Output = Option<Terminate>;

    fn exec(&self, state: &mut ThreadState) -> Result<Self::Output, Error> {
        loop {
            self.body.exec(state)?;
            if !self.condition.eval(state)?.to_bool() {
                break;
            }
        }
        Ok(None)
    }
}

impl Execute for WhileLoop {
    type Output = Option<Terminate>;

    fn exec(&self, state: &mut ThreadState) -> Result<Self::Output, Error> {
        while self.condition.eval(state)?.to_bool() {
            self.body.exec(state)?;
        }
        Ok(None)
    }
}

impl Execute for ForLoop {
    type Output = Option<Terminate>;

    fn exec(&self, _state: &mut ThreadState) -> Result<Self::Output, Error> {
        todo!()
    }
}

impl Execute for ForOfLoop {
    type Output = Option<Terminate>;

    fn exec(&self, _state: &mut ThreadState) -> Result<Self::Output, Error> {
        todo!()
    }
}

impl Execute for Spanned<LexicalDeclaration> {
    /// Return the list of declared names
    type Output = Vec<String>;

    fn exec(&self, state: &mut ThreadState<'_>) -> Result<Self::Output, Error> {
        // Track the list of declared names
        let mut declared = Vec::with_capacity(self.variables.len());
        for variable in &self.variables {
            let value = if let Some(init) = &variable.init {
                init.eval(state)?
            } else {
                Value::Undefined
            };
            let names = state
                .scope_mut()
                .bind(&variable.binding, value, self.mutable)
                .spanned_err(self.span)?;
            declared.extend(names);
        }

        Ok(declared)
    }
}

impl Execute for Spanned<FunctionDeclaration> {
    /// Emit declared name
    type Output = Option<String>;

    fn exec(&self, state: &mut ThreadState<'_>) -> Result<Self::Output, Error> {
        let function = self
            .pointer
            .eval(state)?
            .try_into_function()
            .expect("TODO should always return fn");
        let name = function.name().map(String::from);

        // Bind the name to the function pointer
        if let Some(name) = &name {
            state
                .scope_mut()
                .declare(name.as_str(), function.into(), false);
        }
        Ok(name)
    }
}

/// TODO
#[derive(Debug)]
#[must_use]
pub enum Terminate {
    /// Skip to the entire of the current loop iteration
    Continue,
    /// Break out of the current loop
    Break,
    /// Return from the current function, optionally with a return value
    Return { return_value: Option<Value> },
}
