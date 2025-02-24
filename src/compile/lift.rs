use crate::{
    ast::{
        ArrayElement, Ast, Declaration, ExportDeclaration, Expression,
        FunctionDefinition, FunctionPointer, ImportDeclaration, Literal,
        ObjectProperty, Spanned, Statement, Variable,
    },
    compile::ProgramId,
    error::RuntimeError,
    function::Function,
};
use rslint_parser::syntax::program::import_decl;
use std::{
    array,
    collections::HashMap,
    hash::Hash,
    mem,
    ops::DerefMut,
    sync::{
        atomic::{AtomicU64, Ordering},
        Arc,
    },
};

#[derive(Debug)]
pub struct FunctionRegistry(HashMap<FunctionId, Arc<FunctionDefinition>>);

impl FunctionRegistry {
    /// Lifting is the process of moving all function definitions in the AST
    /// into a single registry, and replacing the original definitions with
    /// IDs into the registry. This is akin to moving all functions into the
    /// .text section of a binary and generating pointers.
    pub fn lift(program_id: ProgramId, ast: &mut Ast) -> Self {
        let mut registry = Self(HashMap::new());
        ast.lift(program_id, &mut registry);
        registry
    }

    /// Look up a function definition by its ID. This is analagous to
    /// derefencing a function pointer into the .text section. This returns
    /// an `Arc` so the lifetime can be detached from the program if necessary
    pub fn get(
        &self,
        function: &Function,
    ) -> Result<&Arc<FunctionDefinition>, RuntimeError> {
        self.0.get(&function.id()).ok_or_else(|| {
            RuntimeError::UnknownFunction {
                function: function.clone(),
            }
        })
    }
}

/// A unique identifier for a function definition. This ID provides the
/// following guarantees:
/// - Globally unique, so two identical functions in different programs will
///   _not_ share the same ID
/// - Composed of primitives so it can be serialized and deserialized, and still
///   reference the original function
/// - Stable across processes of the same program. This allows you to serialize
///   a function ID, then recreate its process later and still use the ID to
///   call the function, provided the process's program hasn't changed at all
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct FunctionId {
    /// A globally unique ID for the process in which this function originated.
    /// It can only be invoked in this process
    program_id: ProgramId,
    /// An ID for this function, unique only within the scope of its process
    function_id: u64,
}

impl FunctionId {
    /// TODO
    fn new(program_id: ProgramId) -> Self {
        static NEXT_ID: AtomicU64 = AtomicU64::new(0);
        // TODO is this the correct ordering?
        let function_id = NEXT_ID.fetch_add(1, Ordering::SeqCst);

        Self {
            program_id,
            function_id,
        }
    }
}

/// Lift functions for a single AST node type
trait Lift {
    /// Walk this entire AST and move any function definitions it contains into
    /// the registry
    fn lift(&mut self, program_id: ProgramId, registry: &mut FunctionRegistry);
}

impl Lift for Ast {
    fn lift(&mut self, program_id: ProgramId, registry: &mut FunctionRegistry) {
        self.statements.lift(program_id, registry)
    }
}

impl Lift for Statement {
    fn lift(&mut self, program_id: ProgramId, registry: &mut FunctionRegistry) {
        match self {
            Self::Declaration(declaration) => {
                declaration.lift(program_id, registry)
            }
            Self::Expression(expression) => {
                expression.lift(program_id, registry)
            }
            Self::Block(block) => block.statements.lift(program_id, registry),
            Self::If(if_statement) => {
                if_statement.condition.lift(program_id, registry);
                if_statement.body.lift(program_id, registry);
                if_statement.else_body.lift(program_id, registry);
            }
            Self::ForLoop(for_loop) => {
                for_loop.initializer.lift(program_id, registry);
                for_loop.condition.lift(program_id, registry);
                for_loop.update.lift(program_id, registry);
                for_loop.body.lift(program_id, registry);
            }
            Self::ForOfLoop(for_of_loop) => todo!(),
            Self::WhileLoop(while_loop) => {
                while_loop.condition.lift(program_id, registry);
            }
            Self::DoWhileLoop(do_while_loop) => {
                do_while_loop.condition.lift(program_id, registry);
            }
            Self::Return(expression) => expression.lift(program_id, registry),
            Self::Import(import) => import.lift(program_id, registry),
            Self::Export(export) => export.lift(program_id, registry),
            Self::Empty | Self::Break | Self::Continue => {}
        }
    }
}

impl Lift for Declaration {
    fn lift(&mut self, program_id: ProgramId, registry: &mut FunctionRegistry) {
        match self {
            Self::Lexical(declaration) => {
                declaration.variables.lift(program_id, registry)
            }
            Self::Function(declaration) => {
                declaration.pointer.lift(program_id, registry)
            }
        }
    }
}

impl Lift for Expression {
    fn lift(&mut self, program_id: ProgramId, registry: &mut FunctionRegistry) {
        match self {
            Self::Parenthesized(expression) => {
                expression.lift(program_id, registry)
            }
            Self::Literal(literal) => literal.lift(program_id, registry),
            Self::Template(_) => todo!(),
            Self::Identifier(_) => {}
            Self::Call(call) => {
                call.function.lift(program_id, registry);
            }
            Self::Property(property) => {
                property.expression.lift(program_id, registry)
            }
            Self::OptionalProperty(property) => {
                property.expression.lift(program_id, registry)
            }
            Self::ArrowFunction(function) => {
                function.lift(program_id, registry);
            }
            Self::Unary(unary) => {
                unary.expression.lift(program_id, registry);
            }
            Self::Binary(binary) => {
                binary.lhs.lift(program_id, registry);
                binary.rhs.lift(program_id, registry);
            }
            Self::Ternary(ternary) => {
                ternary.condition.lift(program_id, registry);
                ternary.true_expression.lift(program_id, registry);
                ternary.false_expression.lift(program_id, registry);
            }
            Self::Assign(assign) => todo!(),
        }
    }
}

impl Lift for Variable {
    fn lift(&mut self, program_id: ProgramId, registry: &mut FunctionRegistry) {
        self.init.lift(program_id, registry);
    }
}

impl Lift for FunctionPointer {
    fn lift(&mut self, program_id: ProgramId, registry: &mut FunctionRegistry) {
        // The actual lifting!
        match self {
            Self::Inline(definition) => {
                let id = FunctionId::new(program_id);
                let name = definition.name.clone();
                let Self::Inline(definition) =
                    mem::replace(self, Self::Lifted { id, name })
                else {
                    unreachable!()
                };
                registry.0.insert(id, Arc::new(definition.data));
            }
            Self::Lifted { id, .. } => {
                // Compiler bug!
                panic!("Function {id:?} has already been lifted!")
            }
        }
    }
}

impl Lift for Literal {
    fn lift(&mut self, program_id: ProgramId, registry: &mut FunctionRegistry) {
        match self {
            Self::Null => {}
            Self::Undefined => {}
            Self::Boolean(_) => {}
            Self::Float(_) => {}
            Self::Int(_) => {}
            Self::String(_) => {}
            Self::Array(array_literal) => {
                array_literal.elements.lift(program_id, registry)
            }
            Self::Object(object_literal) => {
                object_literal.properties.lift(program_id, registry)
            }
        }
    }
}

impl Lift for ArrayElement {
    fn lift(&mut self, program_id: ProgramId, registry: &mut FunctionRegistry) {
        match self {
            Self::Expression(expression) | Self::Spread(expression) => {
                expression.lift(program_id, registry)
            }
        }
    }
}

impl Lift for ObjectProperty {
    fn lift(&mut self, program_id: ProgramId, registry: &mut FunctionRegistry) {
        match self {
            Self::Property { expression, .. } => {
                expression.lift(program_id, registry)
            }
            Self::Identifier(_) => {}
            Self::Spread(expression) => expression.lift(program_id, registry),
        }
    }
}

impl Lift for ImportDeclaration {
    fn lift(&mut self, program_id: ProgramId, registry: &mut FunctionRegistry) {
        todo!()
    }
}

impl Lift for ExportDeclaration {
    fn lift(&mut self, program_id: ProgramId, registry: &mut FunctionRegistry) {
        match self {
            Self::Reexport {} => todo!(),
            Self::Declaration(declaration) => {
                declaration.lift(program_id, registry)
            }
            Self::DefaultFunctionDeclaration(spanned) => todo!(),
            Self::DefaultExpression(spanned) => todo!(),
        }
    }
}

impl<T: Lift> Lift for Box<T> {
    fn lift(&mut self, program_id: ProgramId, registry: &mut FunctionRegistry) {
        self.deref_mut().lift(program_id, registry);
    }
}

impl<T: Lift> Lift for Spanned<T> {
    fn lift(&mut self, program_id: ProgramId, registry: &mut FunctionRegistry) {
        self.data.lift(program_id, registry);
    }
}

impl<T: Lift> Lift for Box<[T]> {
    fn lift(&mut self, program_id: ProgramId, registry: &mut FunctionRegistry) {
        self.iter_mut()
            .for_each(|element| element.lift(program_id, registry));
    }
}

impl<T: Lift> Lift for Option<T> {
    fn lift(&mut self, program_id: ProgramId, registry: &mut FunctionRegistry) {
        self.iter_mut()
            .for_each(|element| element.lift(program_id, registry));
    }
}
