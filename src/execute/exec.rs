//! Statement execution

use crate::{
    ast::{
        source::Spanned, Ast, Block, Declaration, ExportDeclaration,
        FunctionDeclaration, ImportDeclaration, LexicalDeclaration, Statement,
    },
    error::{ResultExt, RuntimeError},
    execute::{eval::Evaluate, ThreadState},
    value::Value,
};

/// TODO
pub trait Execute {
    type Output;

    /// TODO. Errors are returned wrapped with the source span of the AST node
    /// from which the error originated. We use `Spanned<RuntimeError>` instead
    /// of `Error::Runtime` because we don't have the context needed here to
    /// qualify the spans.
    fn exec(
        &self,
        state: &mut ThreadState,
    ) -> Result<Self::Output, Spanned<RuntimeError>>;
}

impl<T, O> Execute for [T]
where
    T: Execute<Output = Option<O>>,
{
    type Output = Option<O>;

    fn exec(
        &self,
        state: &mut ThreadState<'_>,
    ) -> Result<Option<O>, Spanned<RuntimeError>> {
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

    fn exec(
        &self,
        state: &mut ThreadState,
    ) -> Result<Self::Output, Spanned<RuntimeError>> {
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
    ) -> Result<Option<Terminate>, Spanned<RuntimeError>> {
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
    ) -> Result<Option<Terminate>, Spanned<RuntimeError>> {
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

    fn exec(
        &self,
        _: &mut ThreadState<'_>,
    ) -> Result<(), Spanned<RuntimeError>> {
        todo!()
    }
}

impl Execute for Spanned<ExportDeclaration> {
    type Output = ();

    fn exec(
        &self,
        state: &mut ThreadState<'_>,
    ) -> Result<(), Spanned<RuntimeError>> {
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

    fn exec(
        &self,
        state: &mut ThreadState<'_>,
    ) -> Result<Self::Output, Spanned<RuntimeError>> {
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

impl Execute for Spanned<LexicalDeclaration> {
    /// Return the list of declared names
    type Output = Vec<String>;

    fn exec(
        &self,
        state: &mut ThreadState<'_>,
    ) -> Result<Self::Output, Spanned<RuntimeError>> {
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
                .bind(&variable.binding, value)
                .spanned_err(self.span)?;
            declared.extend(names);
        }

        Ok(declared)
    }
}

impl Execute for Spanned<FunctionDeclaration> {
    /// Emit declared name
    type Output = Option<String>;

    fn exec(
        &self,
        state: &mut ThreadState<'_>,
    ) -> Result<Self::Output, Spanned<RuntimeError>> {
        let function = self
            .pointer
            .eval(state)?
            .try_into_function()
            .expect("TODO should always return fn");
        let name = function.name().map(String::from);

        // Bind the name to the function pointer
        if let Some(name) = &name {
            state.scope_mut().declare(name, function);
        }
        Ok(name)
    }
}

/// TODO
#[derive(Debug)]
#[must_use]
pub enum Terminate {
    /// Return from the current function, optionally with a return value
    Return { return_value: Option<Value> },
}
