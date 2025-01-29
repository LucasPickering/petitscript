//! Statement execution

use crate::{
    runtime::{eval::Evaluate, RuntimeState},
    value::{Function, Value},
    Error, Result,
};
use boa_ast::{
    declaration::{ExportDeclaration, ImportDeclaration, LexicalDeclaration},
    function::FunctionDeclaration,
    statement::Block,
    Declaration, ModuleItem, Statement, StatementListItem,
};

/// TODO
pub trait Execute {
    type Output;

    fn exec(&self, state: &mut RuntimeState) -> Result<Self::Output>;
}

impl<T, O> Execute for [T]
where
    T: Execute<Output = Option<O>>,
{
    type Output = Option<O>;

    fn exec(&self, state: &mut RuntimeState) -> Result<Option<O>> {
        for statement in self {
            let output = statement.exec(state)?;
            if let Some(output) = output {
                return Ok(Some(output));
            }
        }
        Ok(None)
    }
}

impl Execute for ModuleItem {
    type Output = Option<Terminate>;

    fn exec(&self, state: &mut RuntimeState) -> Result<Option<Terminate>> {
        match self {
            Self::ImportDeclaration(import) => {
                import.exec(state)?;
                Ok(None)
            }
            Self::ExportDeclaration(export) => {
                export.exec(state)?;
                Ok(None)
            }
            Self::StatementListItem(item) => item.exec(state),
        }
    }
}

impl Execute for StatementListItem {
    type Output = Option<Terminate>;

    fn exec(&self, state: &mut RuntimeState) -> Result<Option<Terminate>> {
        match self {
            Self::Statement(statement) => statement.exec(state),
            Self::Declaration(declaration) => {
                declaration.exec(state)?;
                Ok(None)
            }
        }
    }
}

impl Execute for Statement {
    /// We can break out of a loop iter, loop, or fn from here
    type Output = Option<Terminate>;

    fn exec(&self, state: &mut RuntimeState) -> Result<Option<Terminate>> {
        match self {
            Self::Empty => Ok(None),
            Self::Block(block) => {
                // A block gets a new lexical scope
                state.with_subscope(|state| block.exec(state))
            }
            Self::Expression(expression) => {
                // Expression may have side effects, so evaluate it and throw
                // away the outcome
                expression.eval(state)?;
                Ok(None)
            }
            Self::If(if_statement) => {
                if if_statement.cond().eval(state)?.to_bool() {
                    if_statement.body().exec(state)
                } else if let Some(statement) = if_statement.else_node() {
                    statement.exec(state)
                } else {
                    Ok(None)
                }
            }
            Self::DoWhileLoop(do_while_loop) => {
                loop {
                    do_while_loop.body().exec(state)?;
                    if !do_while_loop.cond().eval(state)?.to_bool() {
                        break;
                    }
                }
                Ok(None)
            }
            Self::WhileLoop(while_loop) => {
                dbg!(while_loop);
                while while_loop.condition().eval(state)?.to_bool() {
                    while_loop.body().exec(state)?;
                }
                Ok(None)
            }
            Self::ForLoop(_) => todo!(),
            Self::ForInLoop(_) => todo!(),
            Self::ForOfLoop(_) => todo!(),
            Self::Switch(_) => todo!(),
            Self::Continue(labelled) => Ok(Some(Terminate::Continue {
                label: labelled
                    .label()
                    .map(|label| state.resolver().resolve(label).to_owned()),
            })),
            Self::Break(labelled) => Ok(Some(Terminate::Break {
                label: labelled
                    .label()
                    .map(|label| state.resolver().resolve(label).to_owned()),
            })),
            Self::Return(ret) => {
                let return_value = ret
                    .target()
                    .map(|expression| expression.eval(state))
                    .transpose()?;
                Ok(Some(Terminate::Return { return_value }))
            }
            Self::Labelled(_) => todo!(),
            Self::Throw(_) => todo!(),
            Self::Try(_) => todo!(),

            // ===== UNSUPPORTED =====
            // All AST nodes below are unsupported in our language
            Self::Var(_) => Err(Error::Unsupported {
                name: "`var`",
                help: "Use `let` or `const` instead",
            }),
            Self::With(_) => Err(Error::Unsupported {
                name: "`with`",
                help: "With blocks are deprecated in ECMAScript",
            }),
        }
    }
}

impl Execute for Block {
    type Output = Option<Terminate>;

    fn exec(&self, state: &mut RuntimeState) -> Result<Option<Terminate>> {
        for statement in self.statement_list().statements() {
            if let Some(terminate) = statement.exec(state)? {
                return Ok(Some(terminate));
            }
        }
        Ok(None)
    }
}

impl Execute for ImportDeclaration {
    type Output = ();

    fn exec(&self, _: &mut RuntimeState) -> Result<()> {
        todo!()
    }
}

impl Execute for ExportDeclaration {
    type Output = ();

    fn exec(&self, state: &mut RuntimeState) -> Result<()> {
        match self {
            ExportDeclaration::ReExport { .. } => todo!(),
            ExportDeclaration::List(_) => todo!(),
            ExportDeclaration::Declaration(declaration) => {
                for name in declaration.exec(state)? {
                    state.export(name)?;
                }
                Ok(())
            }
            ExportDeclaration::DefaultFunctionDeclaration(
                function_declaration,
            ) => {
                let name = function_declaration.exec(state)?;
                // Fetch the function we just declared. Should never fail
                let value = state.scope().get(&name)?;
                state.export_default(value)
            }
            ExportDeclaration::DefaultAssignmentExpression(expression) => {
                let value = expression.eval(state)?;
                state.export_default(value)
            }

            ExportDeclaration::VarStatement(_)
            | ExportDeclaration::DefaultGeneratorDeclaration(_)
            | ExportDeclaration::DefaultAsyncFunctionDeclaration(_)
            | ExportDeclaration::DefaultAsyncGeneratorDeclaration(_)
            | ExportDeclaration::DefaultClassDeclaration(_) => {
                todo!("not allowed")
            }
        }
    }
}

impl Execute for Declaration {
    /// Return the list of declared names
    type Output = Vec<String>;

    fn exec(&self, state: &mut RuntimeState) -> Result<Vec<String>> {
        match self {
            Self::FunctionDeclaration(function) => {
                let name = function.exec(state)?;
                Ok(vec![name])
            }
            Self::Lexical(lexical_declaration) => {
                let (variables, mutable) = match lexical_declaration {
                    LexicalDeclaration::Const(variable_list) => {
                        (variable_list, false)
                    }
                    LexicalDeclaration::Let(variable_list) => {
                        (variable_list, true)
                    }
                };

                // Track the list of declared names
                let mut declared = Vec::with_capacity(variables.as_ref().len());
                for variable in variables.as_ref() {
                    let value = variable
                        .init()
                        .map(|expr| expr.eval(state))
                        .transpose()?
                        .unwrap_or_default();
                    let resolver = state.resolver();
                    let names = state.scope_mut().bind(
                        resolver,
                        variable.binding(),
                        value,
                        mutable,
                    )?;
                    declared.extend(names);
                }

                Ok(declared)
            }
            // ===== UNSUPPORTED =====
            // All AST nodes below are unsupported in our language
            Self::ClassDeclaration(_)
            | Self::AsyncFunctionDeclaration(_)
            | Self::AsyncGeneratorDeclaration(_) => Err(Error::Unsupported {
                name: "`async`",
                help: "All operations must be synchronous",
            }),
            Self::GeneratorDeclaration(_) => todo!("not allowed"),
        }
    }
}

impl Execute for FunctionDeclaration {
    /// Emit declared name
    type Output = String;

    fn exec(&self, state: &mut RuntimeState) -> Result<Self::Output> {
        let name = state.resolver().resolve(self.name().sym()).to_owned();
        // TODO make sure we aren't capturing names declared after the fn
        let scope = state.scope_mut();
        let function = Function::new(
            Some(name.clone()),
            self.parameters().clone(),
            self.body().clone(),
            scope.clone(),
        );
        scope.declare(name.clone(), function.into(), false);
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
        label: Option<String>,
    },
    /// Break out of the current loop
    Break {
        /// Name the loop to skip to break
        label: Option<String>,
    },
    /// Return from the current function, optionally with a return value
    Return { return_value: Option<Value> },
}
