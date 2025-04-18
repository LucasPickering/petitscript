//! Statement execution

use crate::{
    ast::{
        Block, Declaration, DoWhileLoop, ExportDeclaration, ForOfLoop,
        FunctionBody, FunctionDeclaration, If, ImportDeclaration, ImportModule,
        LexicalDeclaration, Module, Node, Statement, WhileLoop,
    },
    error::TracedError,
    execute::{eval::Evaluate, ThreadState},
    value::Value,
    Exports,
};
use std::borrow::Cow;

// TODO normalize whether we use Node<T> or just T on all impls

/// TODO
pub trait Execute {
    type Output<'process>;

    /// TODO. Errors are returned wrapped with the source span of the AST node
    /// from which the error originated. We use `Traced<RuntimeError,Span>`
    /// instead of `Error::Runtime` because we don't have the context needed
    /// here to qualify the spans.
    fn exec<'process>(
        &self,
        state: &mut ThreadState<'process>,
    ) -> Result<Self::Output<'process>, TracedError>;
}

impl<T, O> Execute for [T]
where
    T: for<'process> Execute<Output<'process> = Option<O>>,
{
    type Output<'process> = Option<O>;

    fn exec(&self, state: &mut ThreadState) -> Result<Option<O>, TracedError> {
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

    fn exec(&self, state: &mut ThreadState) -> Result<(), TracedError> {
        self.statements.exec(state)?;
        Ok(())
    }
}

impl Execute for Node<Statement> {
    /// We can break out of a loop iter, loop, or fn from here
    type Output<'process> = Option<Terminate>;

    fn exec(
        &self,
        state: &mut ThreadState,
    ) -> Result<Option<Terminate>, TracedError> {
        match &self.node() {
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
    ) -> Result<Option<Terminate>, TracedError> {
        for statement in &self.statements {
            if let Some(terminate) = statement.exec(state)? {
                return Ok(Some(terminate));
            }
        }
        Ok(None)
    }
}

impl Execute for Node<ImportDeclaration> {
    type Output<'process> = ();

    fn exec(&self, state: &mut ThreadState) -> Result<(), TracedError> {
        let exports = self.module.exec(state)?;

        let scope = state.scope_mut();
        if let Some(name) = &self.default {
            scope.declare(
                name.as_str(),
                // TODO only clone as needed
                exports.default.clone().ok_or_else(|| todo!())?,
            );
        }
        for named in &self.named {
            let name = named.rename.as_ref().unwrap_or(&named.identifier);
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

        Ok(())
    }
}

impl Execute for Node<ImportModule> {
    type Output<'process> = Cow<'process, Exports>;

    /// Resolve a module name/path into its exports. For native modules this
    /// just grabs the exports from the registry. For paths, it will execute
    /// the external program
    fn exec<'process>(
        &self,
        state: &mut ThreadState<'process>,
    ) -> Result<Cow<'process, Exports>, TracedError> {
        match &self.node() {
            ImportModule::Native(name) => state
                .process()
                .native_module(name)
                .map(Cow::Borrowed)
                .map_err(|error| state.trace_error(error, self.id())),
            ImportModule::Local(ast) => {
                let mut thread_state = state.process().create_thread();
                ast.exec(&mut thread_state)?;
                let exports = thread_state.into_exports().unwrap();
                Ok(Cow::Owned(exports))
            }
        }
    }
}

impl Execute for Node<ExportDeclaration> {
    type Output<'process> = ();

    fn exec(&self, state: &mut ThreadState) -> Result<(), TracedError> {
        match &self.node() {
            ExportDeclaration::Reexport { .. } => todo!(),
            ExportDeclaration::Declaration(declaration) => {
                for name in declaration.exec(state)? {
                    state.export(name).map_err(|error| {
                        state.trace_error(error, declaration.id())
                    })?;
                }
                Ok(())
            }
            ExportDeclaration::DefaultFunctionDeclaration(
                function_declaration,
            ) => {
                let name = function_declaration.exec(state)?.expect("TODO");
                // Fetch the function we just declared. Should never fail
                let value =
                    state.scope().get(name.as_str()).map_err(|error| {
                        state.trace_error(error, function_declaration.id())
                    })?;
                state
                    .export_default(value)
                    .map_err(|error| state.trace_error(error, self.id()))
            }
            ExportDeclaration::DefaultExpression(expression) => {
                let value = expression.eval(state)?;
                state
                    .export_default(value)
                    .map_err(|error| state.trace_error(error, self.id()))
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
    ) -> Result<Vec<String>, TracedError> {
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
    ) -> Result<Option<Terminate>, TracedError> {
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
    ) -> Result<Option<Terminate>, TracedError> {
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
    ) -> Result<Option<Terminate>, TracedError> {
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
    ) -> Result<Option<Terminate>, TracedError> {
        todo!()
    }
}

impl Execute for Node<LexicalDeclaration> {
    /// Return the list of declared names
    type Output<'process> = Vec<String>;

    fn exec(
        &self,
        state: &mut ThreadState,
    ) -> Result<Vec<String>, TracedError> {
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
                .map_err(|error| state.trace_error(error, self.id()))?;
            declared.extend(names);
        }

        Ok(declared)
    }
}

impl Execute for Node<FunctionDeclaration> {
    /// Emit declared name
    type Output<'process> = Option<String>;

    fn exec(
        &self,
        state: &mut ThreadState,
    ) -> Result<Option<String>, TracedError> {
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

impl Execute for FunctionBody {
    /// Function return value
    type Output<'process> = Value;

    fn exec<'process>(
        &self,
        state: &mut ThreadState<'process>,
    ) -> Result<Self::Output<'process>, TracedError> {
        match self {
            Self::Expression(expression) => expression.eval(state),
            Self::Block(block) => match block.exec(state)? {
                Some(Terminate::Return {
                    return_value: Some(return_value),
                }) => Ok(return_value),
                _ => Ok(Value::Undefined),
            },
        }
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
