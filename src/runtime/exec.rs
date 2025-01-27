//! Statement execution

use crate::{
    runtime::{eval::Evaluate, RuntimeState},
    Error, Result,
};
use boa_ast::{
    declaration::{ExportDeclaration, ImportDeclaration, LexicalDeclaration},
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
            Self::Block(block) => block.exec(state),
            Self::Empty => Ok(None),
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
                while while_loop.condition().eval(state)?.to_bool() {
                    while_loop.body().exec(state)?;
                }
                Ok(None)
            }
            Self::ForLoop(for_loop) => todo!(),
            Self::ForInLoop(for_in_loop) => todo!(),
            Self::ForOfLoop(for_of_loop) => todo!(),
            Self::Switch(switch) => todo!(),
            // TODO support labels
            Self::Continue(_) => Ok(None),
            Self::Break(_) => Ok(None),
            Self::Return(ret) => {
                let mut frame =
                    state.stack.pop().ok_or(Error::IllegalReturn)?;
                if let Some(expression) = ret.target() {
                    frame.return_value = expression.eval(state)?;
                }
                // Exit the function
                Ok(None)
            }
            Self::Labelled(labelled) => todo!(),
            Self::Throw(throw) => todo!(),
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

    fn exec(&self, state: &mut RuntimeState) -> Result<()> {
        todo!()
    }
}

impl Execute for ExportDeclaration {
    type Output = ();

    fn exec(&self, state: &mut RuntimeState) -> Result<()> {
        match self {
            ExportDeclaration::ReExport { kind, specifier } => todo!(),
            ExportDeclaration::List(_) => todo!(),
            ExportDeclaration::Declaration(declaration) => {
                for declared in declaration.exec(state)? {
                    // TODO error on duplicate export
                    state.export_names.push(declared);
                }
                Ok(())
            }
            ExportDeclaration::DefaultFunctionDeclaration(
                function_declaration,
            ) => todo!(),
            ExportDeclaration::DefaultAssignmentExpression(expression) => {
                todo!()
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
            Self::FunctionDeclaration(function_declaration) => todo!(),
            Self::Lexical(lexical_declaration) => {
                let (variables, mutable) = match lexical_declaration {
                    LexicalDeclaration::Const(variable_list) => {
                        (variable_list, false)
                    }
                    LexicalDeclaration::Let(variable_list) => {
                        (variable_list, true)
                    }
                };
                state.declare_all(variables, mutable)
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

/// TODO
#[derive(Debug)]
#[must_use]
pub enum Terminate {
    LoopIteration { label: Option<String> },
    Loop { label: Option<String> },
    Function,
}
