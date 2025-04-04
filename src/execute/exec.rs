//! Statement execution

use crate::{
    ast::{
        source::Spanned, Block, Declaration, DoWhileLoop, ExportDeclaration,
        ForOfLoop, FunctionDeclaration, If, ImportDeclaration, ImportModule,
        LexicalDeclaration, Module, Statement, WhileLoop,
    },
    error::{ResultExt, RuntimeError},
    execute::{eval::Evaluate, ThreadState},
    value::Value,
    Exports,
};
use std::borrow::Cow;

/// TODO
pub trait Execute {
    type Output<'process>;

    /// TODO. Errors are returned wrapped with the source span of the AST node
    /// from which the error originated. We use `Spanned<RuntimeError>` instead
    /// of `Error::Runtime` because we don't have the context needed here to
    /// qualify the spans.
    fn exec<'process>(
        &self,
        state: &mut ThreadState<'process>,
    ) -> Result<Self::Output<'process>, Spanned<RuntimeError>>;
}

impl<T, O> Execute for [T]
where
    T: for<'process> Execute<Output<'process> = Option<O>>,
{
    type Output<'process> = Option<O>;

    fn exec(
        &self,
        state: &mut ThreadState,
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

impl Execute for Module {
    type Output<'process> = ();

    fn exec(
        &self,
        state: &mut ThreadState,
    ) -> Result<(), Spanned<RuntimeError>> {
        self.statements.exec(state)?;
        Ok(())
    }
}

impl Execute for Spanned<Statement> {
    /// We can break out of a loop iter, loop, or fn from here
    type Output<'process> = Option<Terminate>;

    fn exec(
        &self,
        state: &mut ThreadState,
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

            Statement::If(if_statement) => if_statement.exec(state),
            Statement::DoWhileLoop(do_while_loop) => do_while_loop.exec(state),
            Statement::WhileLoop(while_loop) => while_loop.exec(state),
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
    type Output<'process> = Option<Terminate>;

    fn exec(
        &self,
        state: &mut ThreadState,
    ) -> Result<Option<Terminate>, Spanned<RuntimeError>> {
        for statement in &self.statements {
            if let Some(terminate) = statement.exec(state)? {
                return Ok(Some(terminate));
            }
        }
        Ok(None)
    }
}

impl Execute for Spanned<ImportDeclaration> {
    type Output<'process> = ();

    fn exec(
        &self,
        state: &mut ThreadState,
    ) -> Result<(), Spanned<RuntimeError>> {
        let module_specifier = match &self.data {
            ImportDeclaration::Named { module, .. }
            | ImportDeclaration::Namespace { module, .. } => module,
        };

        let exports = module_specifier.exec(state)?;

        let scope = state.scope_mut();
        match &self.data {
            ImportDeclaration::Named { default, named, .. } => {
                if let Some(name) = default {
                    scope.declare(
                        name.as_str(),
                        // TODO only clone as needed
                        exports.default.clone().ok_or_else(|| todo!())?,
                    );
                }
                for named in named {
                    let name =
                        named.rename.as_ref().unwrap_or(&named.identifier);
                    scope.declare(
                        name.as_str(),
                        // TODO only clone as needed
                        exports
                            .named
                            .get(named.identifier.as_str())
                            .expect("TODO")
                            .clone(),
                    );
                }
            }
            ImportDeclaration::Namespace { .. } => todo!(),
        }

        Ok(())
    }
}

impl Execute for Spanned<ImportModule> {
    type Output<'process> = Cow<'process, Exports>;

    /// Resolve a module name/path into its exports. For native modules this
    /// just grabs the exports from the registry. For paths, it will execute
    /// the external program
    fn exec<'process>(
        &self,
        state: &mut ThreadState<'process>,
    ) -> Result<Cow<'process, Exports>, Spanned<RuntimeError>> {
        match &self.data {
            ImportModule::Native(name) => state
                .process()
                .native_module(name)
                .map(Cow::Borrowed)
                .spanned_err(self.span),
            ImportModule::Local(ast) => {
                let mut thread_state = state.process().chungus();
                ast.exec(&mut thread_state)?;
                let exports = thread_state.into_exports().unwrap();
                Ok(Cow::Owned(exports))
            }
        }
    }
}

impl Execute for Spanned<ExportDeclaration> {
    type Output<'process> = ();

    fn exec(
        &self,
        state: &mut ThreadState,
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
    type Output<'process> = Vec<String>;

    fn exec(
        &self,
        state: &mut ThreadState,
    ) -> Result<Vec<String>, Spanned<RuntimeError>> {
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
    type Output<'process> = Option<Terminate>;

    fn exec(
        &self,
        state: &mut ThreadState,
    ) -> Result<Option<Terminate>, Spanned<RuntimeError>> {
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
    type Output<'process> = Option<Terminate>;

    fn exec(
        &self,
        state: &mut ThreadState,
    ) -> Result<Option<Terminate>, Spanned<RuntimeError>> {
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
    type Output<'process> = Option<Terminate>;

    fn exec(
        &self,
        state: &mut ThreadState,
    ) -> Result<Option<Terminate>, Spanned<RuntimeError>> {
        while self.condition.eval(state)?.to_bool() {
            self.body.exec(state)?;
        }
        Ok(None)
    }
}

impl Execute for ForOfLoop {
    type Output<'process> = Option<Terminate>;

    fn exec(
        &self,
        _state: &mut ThreadState,
    ) -> Result<Option<Terminate>, Spanned<RuntimeError>> {
        todo!()
    }
}

impl Execute for Spanned<LexicalDeclaration> {
    /// Return the list of declared names
    type Output<'process> = Vec<String>;

    fn exec(
        &self,
        state: &mut ThreadState,
    ) -> Result<Vec<String>, Spanned<RuntimeError>> {
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
    type Output<'process> = Option<String>;

    fn exec(
        &self,
        state: &mut ThreadState,
    ) -> Result<Option<String>, Spanned<RuntimeError>> {
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
    /// Skip to the entire of the current loop iteration
    Continue,
    /// Break out of the current loop
    Break,
    /// Return from the current function, optionally with a return value
    Return { return_value: Option<Value> },
}
