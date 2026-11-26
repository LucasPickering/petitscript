//! AST to stack machine compilation

use crate::{
    Program, Value,
    ast::{
        ArrayLiteral, BinaryOperator, Binding, BindingKind, Block, Declaration,
        ExportDeclaration, ExportDeclarationKind, Expression, ExpressionKind,
        Identifier, If, ImportDeclaration, Literal, Module, Node,
        ObjectLiteral, ObjectPropertyKind, PropertyName, PropertyNameKind,
        Statement, StatementKind, Variable,
    },
    value::Number,
};

pub struct Compiler {
    /// Compiled set of instructions
    ///
    /// This is the final result of compilation.
    instructions: Vec<Instruction>,
    /// List of bindings accessible in the current scope
    ///
    /// TODO explain how this correponds to slots on the stack
    ///
    /// Invariant: for each element, `depth` is greater than or equal to
    /// `depth` of the previous element. This means for any depth `n`, its
    /// locals will be contiguous in this list.
    locals: Vec<Local>,
    /// Scope depth of the AST node currently being compiled
    ///
    /// `0` for the global scope and increasing by `1` for each block/function
    /// body (each pair of `{}`).
    current_scope: u16,
}

impl Compiler {
    /// Compile a parsed program into a stack machine program
    pub fn compile(program: &Program) -> Vec<Instruction> {
        let mut compiler = Self {
            instructions: vec![],
            locals: vec![],
            current_scope: 0,
        };
        program.root().compile(&mut compiler);
        compiler.instructions
    }

    /// Declare an identifier for a local on the stack
    fn declare(&mut self, identifier: Identifier) {
        // TODO check if local already exists at this scope - shadowing illegal
        self.locals.push(Local {
            identifier,
            depth: self.current_scope,
        });
    }

    /// Declare a new nested lexical scope
    fn enter_scope(&mut self) {
        self.current_scope += 1;
    }

    /// Exit the current lexical scope, freeing its locals
    ///
    /// ## Panics
    ///
    /// Panics if already in the global scope.
    fn exit_scope(&mut self) {
        self.current_scope = self
            .current_scope
            .checked_sub(1)
            .expect("Cannot exit global scope");

        // Free the locals from this scope. Each scope is contiguous, so find
        // where the current starts
        let index = self
            .locals
            .iter()
            .position(|local| local.depth > self.current_scope)
            .unwrap_or(self.locals.len());
        let len = self.locals.drain(index..).count();
        for _ in 0..len {
            self.push(Instruction::Pop);
        }
    }

    /// Get the slot index of a local variable by identifier
    ///
    /// This gets the *innermost* (highest) stack slot matching the identifier,
    /// so that inner scopes shadow outer scopes.
    ///
    /// ## Panics
    ///
    /// Panics if the identifier is not in the locals.
    fn local_slot(&self, identifier: &Identifier) -> usize {
        // The index of the local in the locals list is equal to its index in
        // the value stack
        self.locals
            .iter()
            .rposition(|local| &local.identifier == identifier)
            .expect("TODO handle unknown ref - defer error to runtime")
    }

    /// Push an instruction onto the program
    fn push(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }
}

/// TODO
pub enum Instruction {
    /// Pop the top two values and apply an operation to them
    ///
    /// Top value is the **right-hand** side, second value is left-hand side.
    /// Result is pushed back onto the stack.
    BinaryOperation(BinaryOperator),
    /// Move the top value to the default export
    ///
    /// Error if there is already a default export. Because `export default`
    /// applies to an expression rather than a bound name, this consumes a
    /// temporary from the stack.
    ExportDefault,
    /// Copy a local to a named export
    ///
    /// Error if the name is already exported.
    ExportNamed {
        /// Name to export: `None` for default, `Some` for named exports
        name: Identifier,
        /// Stack slot containing the value to export
        slot: usize,
    },
    /// Pop the active value
    ///
    /// The popped value is thrown away.
    Pop,
    /// Push a value onto the stack
    Push(Value),
    /// Pop `len` values off the stack, put them in an array, and push it back
    PushArray {
        /// Number of elements in the array literal
        len: usize,
        /// Indexes of elements that should be spread
        ///
        /// The corresponding values must be arrays, or there will be a runtime
        /// error.
        spreads: Box<[usize]>,
    },
    /// Pop `len` values off the stack, put them in an object, and push it back
    ///
    /// TODO explain values
    PushObject {
        /// Number of property elements in the input array
        len_properties: usize,
        /// Number of *values* pushed onto the stack for this object
        ///
        /// Key:value properties push two values (key+value), while spreads
        /// push just one value. This will be in the range `[len_properties,
        /// 2*len_properties]`.
        len_values: usize,
        /// Indexes of properties that should be spread
        ///
        /// The corresponding values must be objects, or there will be a
        /// runtime error.
        spreads: Box<[usize]>,
    },
    /// Copy the value of a local onto the stack
    PushReference {
        /// Stack slot containing the local value
        slot: usize,
    },
}

/// TODO
struct Local {
    /// TODO
    identifier: Identifier,
    /// TODO
    depth: u16,
}

/// TODO
trait Compile: Node {
    /// TODO
    fn compile(&self, compiler: &mut Compiler);
}

impl Compile for Module {
    fn compile(&self, compiler: &mut Compiler) {
        for statement in &self.statements {
            statement.compile(compiler);
        }
    }
}

impl Compile for Statement {
    fn compile(&self, compiler: &mut Compiler) {
        match &self.kind {
            StatementKind::Block(block) => block.compile(compiler),
            StatementKind::Declaration(declaration) => {
                declaration.compile(compiler);
            }
            StatementKind::Empty => {}
            StatementKind::Export(export_declaration) => {
                export_declaration.compile(compiler);
            }
            StatementKind::Expression(expression) => {
                expression.compile(compiler);
                // We don't need the resolved value, so toss it
                compiler.push(Instruction::Pop);
            }
            StatementKind::If(if_statement) => {
                if_statement.compile(compiler);
            }
            StatementKind::Import(import_declaration) => {
                import_declaration.compile(compiler);
            }
            StatementKind::Return(_) => todo!(),
        }
    }
}

impl Compile for Block {
    fn compile(&self, compiler: &mut Compiler) {
        compiler.enter_scope();
        for statement in &self.statements {
            statement.compile(compiler);
        }
        compiler.exit_scope();
    }
}

impl Compile for Declaration {
    fn compile(&self, compiler: &mut Compiler) {
        for variable in &self.variables {
            variable.compile(compiler);
        }
    }
}

impl Compile for Variable {
    fn compile(&self, compiler: &mut Compiler) {
        // First put the value on the stack
        self.init.compile(compiler);
        // Then bind it
        self.binding.compile(compiler);
    }
}

impl Compile for Binding {
    fn compile(&self, compiler: &mut Compiler) {
        match &self.kind {
            BindingKind::Identifier(identifier) => {
                compiler.declare(identifier.identifier.clone());
            }
            BindingKind::Object { .. } => todo!("object binding"),
            BindingKind::Array { .. } => todo!("array binding"),
        }
    }
}

impl Compile for ExportDeclaration {
    fn compile(&self, compiler: &mut Compiler) {
        // TODO disallow at depth > 0
        match &self.kind {
            ExportDeclarationKind::Declaration(declaration) => {
                // Collect the set of names to export by checking what's added
                // to the locals list
                let start_len = compiler.locals.len();
                declaration.compile(compiler);
                for slot in start_len..compiler.locals.len() {
                    let identifier = &compiler.locals[slot].identifier;
                    compiler.push(Instruction::ExportNamed {
                        name: identifier.clone(),
                        slot,
                    });
                }
            }
            ExportDeclarationKind::DefaultExpression(expression) => {
                expression.compile(compiler);
                compiler.push(Instruction::ExportDefault);
            }
        }
    }
}

impl Compile for Expression {
    fn compile(&self, compiler: &mut Compiler) {
        match &self.kind {
            ExpressionKind::Parenthesized(expression) => {
                expression.compile(compiler);
            }
            ExpressionKind::Literal(literal) => {
                let value = match literal {
                    Literal::Null => Value::Null,
                    Literal::Undefined => Value::Undefined,
                    Literal::Boolean(b) => Value::Boolean(*b),
                    Literal::String(s) => Value::String(s.as_str().into()),
                    Literal::Float(f) => Value::Number(Number::Float(*f)),
                    Literal::Int(i) => Value::Number(Number::Int(*i)),
                    Literal::Array(array_literal) => {
                        return array_literal.compile(compiler);
                    }
                    Literal::Object(object_literal) => {
                        return object_literal.compile(compiler);
                    }
                };
                compiler.push(Instruction::Push(value));
            }
            ExpressionKind::Template(_) => todo!(),
            ExpressionKind::Identifier(identifier) => {
                let slot = compiler.local_slot(&identifier.identifier);
                compiler.push(Instruction::PushReference { slot });
            }
            ExpressionKind::Call(_) => todo!(),
            ExpressionKind::Property(_) => todo!(),
            ExpressionKind::OptionalProperty(_) => todo!(),
            ExpressionKind::Function(_) => todo!(),
            ExpressionKind::Unary(_) => todo!(),
            ExpressionKind::Binary(binary_operation) => {
                // Push the two operands on the stack
                binary_operation.lhs.compile(compiler);
                binary_operation.rhs.compile(compiler);
                compiler.push(Instruction::BinaryOperation(
                    binary_operation.operator,
                ));
            }
            ExpressionKind::Ternary(_) => todo!(),
        }
    }
}

impl Compile for ArrayLiteral {
    fn compile(&self, compiler: &mut Compiler) {
        // Evaluate each element
        let mut spreads = vec![];
        for (i, element) in self.elements.iter().enumerate() {
            element.expression.compile(compiler);
            if element.spread {
                spreads.push(i);
            }
        }
        compiler.push(Instruction::PushArray {
            len: self.elements.len(),
            spreads: spreads.into(),
        });
    }
}

impl Compile for ObjectLiteral {
    fn compile(&self, compiler: &mut Compiler) {
        // TODO explain
        let mut len_values = 0;
        let mut spreads = vec![];
        for (i, property) in self.properties.iter().enumerate() {
            match &property.kind {
                ObjectPropertyKind::Property {
                    property,
                    expression,
                } => {
                    // Each of these should push one value
                    property.compile(compiler);
                    expression.compile(compiler);
                    len_values += 2;
                }
                ObjectPropertyKind::Spread(expression) => {
                    expression.compile(compiler);
                    spreads.push(i);
                    len_values += 1;
                }
            }
        }
        compiler.push(Instruction::PushObject {
            len_properties: self.properties.len(),
            len_values,
            spreads: spreads.into(),
        });
    }
}

impl Compile for PropertyName {
    fn compile(&self, compiler: &mut Compiler) {
        match &self.kind {
            PropertyNameKind::Identifier(identifier) => {
                compiler.push(Instruction::Push(identifier.as_str().into()));
            }
            PropertyNameKind::Expression(expression) => {
                expression.compile(compiler);
            }
        }
    }
}

impl Compile for If {
    fn compile(&self, _compiler: &mut Compiler) {
        todo!()
    }
}

impl Compile for ImportDeclaration {
    fn compile(&self, _compiler: &mut Compiler) {
        todo!()
    }
}
