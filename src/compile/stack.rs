//! Compile AST to stack machine instructions

use crate::{
    Program, Value,
    ast::{
        ArrayLiteral, BinaryOperator, Binding, BindingKind, Block, Declaration,
        ExportDeclaration, ExportDeclarationKind, Expression, ExpressionKind,
        Identifier, IdentifierNode, If, ImportDeclaration, Literal, Module,
        Node, NodeId, ObjectLiteral, ObjectPropertyKind, PropertyName,
        PropertyNameKind, Statement, StatementKind, Variable,
    },
    error::{ParseError, ParseErrorKind, ParseErrors},
    source::SpanTable,
    value::Number,
};

/// TODO
pub struct Compiler {
    /// Scope depth of the AST node currently being compiled
    ///
    /// `0` for the global scope and increasing by `1` for each block/function
    /// body (each pair of `{}`).
    current_scope: u16,
    /// Error(s) that have occurred during compilation
    ///
    /// If this is non-empty at the end of compilation, the compilation will
    /// fail.
    errors: Vec<ParseError>,
    /// Compiled set of instructions
    ///
    /// This is the built progressively during compilation and will become the
    /// final output. It is **append-only**.
    instructions: Vec<Instruction>,
    /// List of bindings accessible in the current scope
    ///
    /// TODO explain how this correponds to slots on the stack
    ///
    /// Invariant: for each element, `depth` is greater than or equal to
    /// `depth` of the previous element. This means for any depth `n`, its
    /// locals will be contiguous in this list.
    locals: Vec<Local>,
    /// TODO
    spans: SpanTable,
}

impl Compiler {
    /// Compile a parsed program into a stack machine program
    ///
    /// TODO explain errors
    pub fn compile(program: Program) -> Result<Vec<Instruction>, ParseErrors> {
        let mut compiler = Self {
            current_scope: 0,
            errors: vec![],
            instructions: vec![],
            locals: vec![],
            spans: program.spans,
        };
        program.modules.root.compile(&mut compiler);
        if compiler.errors.is_empty() {
            Ok(compiler.instructions)
        } else {
            Err(ParseErrors {
                errors: compiler.errors,
                source_table: program.sources,
            })
        }
    }

    /// Backpatch the offset of a jump instruction to jump to the end of the
    /// instruction list
    ///
    /// Sometimes jumps have to be inserted before the code they intend to
    /// jump over. They're initially inserted with an offset of 0, then
    /// backpatched once the subsequent code has been generated. This should be
    /// called immediately after generating the code to skip, so the jump offset
    /// will point to the next generated instruction.
    fn backpatch_jump(&mut self, index: usize) {
        let len = self.instructions.len();
        match &mut self.instructions[index] {
            Instruction::Jump(offset) | Instruction::JumpIfNot(offset) => {
                *offset = (len - index) as i16;
            }
            instruction => panic!(
                "Cannot backpatch instruction {instruction:?} at {index} (not a jump)"
            ),
        }
    }

    /// Declare an identifier for a local on the stack
    ///
    /// Return an error if the identifier is already declared in the current
    /// scope.
    fn declare(
        &mut self,
        identifier: &IdentifierNode,
    ) -> Result<(), ParseErrorKind> {
        // If the identifier already exists *at the same scope*, that's illegal
        if let Some(other) = self.locals.iter().find(|l| {
            l.identifier == identifier.identifier
                && l.scope == self.current_scope
        }) {
            Err(ParseErrorKind::DuplicateBinding {
                identifier: identifier.identifier.clone(),
                other: self.spans.get(other.node_id).expect("TODO"),
            })
        } else {
            self.locals.push(Local {
                identifier: identifier.identifier.clone(),
                scope: self.current_scope,
                node_id: identifier.id(),
            });
            Ok(())
        }
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
            .position(|local| local.scope > self.current_scope)
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

    /// Generate and register a compile error at a node
    fn register_error(&mut self, node: &impl Node, error: ParseErrorKind) {
        let span = self.spans.get(node.id()).expect("TODO how handle");
        self.errors.push(ParseError { error, span });
    }
}

/// TODO
#[derive(Debug)]
pub enum Instruction {
    /// Pop the top two values and apply an operation to them
    ///
    /// Top value is the **right-hand** side, second value is left-hand side.
    /// Result is pushed back onto the stack.
    BinaryOperation(BinaryOperator),
    /// Move the active value to the default export
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
    /// Jump a fixed number of instructions backward or forward
    ///
    /// Negative for backward, positive for forward. The program counter will
    /// *not* be automatically incremented after a jump, unless the offset is 0.
    Jump(i16),
    /// Jump a if the active value is falsy
    ///
    /// This will **pop the active value**. See [`Instruction::Jump`].
    JumpIfNot(i16),
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
#[derive(Debug)]
struct Local {
    /// TODO
    identifier: Identifier,
    /// TODO
    scope: u16,
    /// ID of the declaring AST node
    ///
    /// This should be the ID of an [`IdentifierNode`], *not* the wrapping
    /// declaration.
    node_id: NodeId,
}

/// TODO
trait Compile: Node + Sized {
    /// Compile this node into instructions
    ///
    /// If an error occurs during compilation, it will be registered in the
    /// compiler. Node types implement [`Compile::compile_impl`], which fails
    /// fast if a child has an error. This registers the error instead of
    /// propagating it so parent and sibling nodes can keep compiling.
    fn compile(&self, compiler: &mut Compiler) {
        if let Err(error) = self.compile_impl(compiler) {
            compiler.register_error(self, error);
        }
    }

    /// Compile this node into instructions, return any errors
    fn compile_impl(
        &self,
        compiler: &mut Compiler,
    ) -> Result<(), ParseErrorKind>;
}

impl Compile for Module {
    fn compile_impl(
        &self,
        compiler: &mut Compiler,
    ) -> Result<(), ParseErrorKind> {
        for statement in &self.statements {
            statement.compile(compiler);
        }
        Ok(())
    }
}

impl Compile for Statement {
    fn compile_impl(
        &self,
        compiler: &mut Compiler,
    ) -> Result<(), ParseErrorKind> {
        match &self.kind {
            StatementKind::Block(block) => {
                block.compile(compiler);
            }
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
        Ok(())
    }
}

impl Compile for Block {
    fn compile_impl(
        &self,
        compiler: &mut Compiler,
    ) -> Result<(), ParseErrorKind> {
        compiler.enter_scope();
        for statement in &self.statements {
            statement.compile(compiler);
        }
        compiler.exit_scope();
        Ok(())
    }
}

impl Compile for Declaration {
    fn compile_impl(
        &self,
        compiler: &mut Compiler,
    ) -> Result<(), ParseErrorKind> {
        for variable in &self.variables {
            variable.compile(compiler);
        }
        Ok(())
    }
}

impl Compile for Variable {
    fn compile_impl(
        &self,
        compiler: &mut Compiler,
    ) -> Result<(), ParseErrorKind> {
        // First put the value on the stack
        self.init.compile(compiler);
        // Then bind it
        self.binding.compile(compiler);
        Ok(())
    }
}

impl Compile for Binding {
    fn compile_impl(
        &self,
        compiler: &mut Compiler,
    ) -> Result<(), ParseErrorKind> {
        match &self.kind {
            BindingKind::Identifier(identifier) => compiler.declare(identifier),
            BindingKind::Object { .. } => todo!("object binding"),
            BindingKind::Array { .. } => todo!("array binding"),
        }
    }
}

impl Compile for ExportDeclaration {
    fn compile_impl(
        &self,
        compiler: &mut Compiler,
    ) -> Result<(), ParseErrorKind> {
        // Exports are only allowed at the root scope, but the parser will
        // reject any other exports so we don't have to check here
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
        Ok(())
    }
}

impl Compile for Expression {
    fn compile_impl(
        &self,
        compiler: &mut Compiler,
    ) -> Result<(), ParseErrorKind> {
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
                    // These two do NOT get the push instruction at the bottom
                    Literal::Array(array_literal) => {
                        array_literal.compile(compiler);
                        return Ok(());
                    }
                    Literal::Object(object_literal) => {
                        object_literal.compile(compiler);
                        return Ok(());
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
                // TODO some binary operators must short-circuit
                binary_operation.lhs.compile(compiler);
                binary_operation.rhs.compile(compiler);
                compiler.push(Instruction::BinaryOperation(
                    binary_operation.operator,
                ));
            }
            ExpressionKind::Ternary(_) => todo!(),
        }
        Ok(())
    }
}

impl Compile for ArrayLiteral {
    fn compile_impl(
        &self,
        compiler: &mut Compiler,
    ) -> Result<(), ParseErrorKind> {
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
        Ok(())
    }
}

impl Compile for ObjectLiteral {
    fn compile_impl(
        &self,
        compiler: &mut Compiler,
    ) -> Result<(), ParseErrorKind> {
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
        Ok(())
    }
}

impl Compile for PropertyName {
    fn compile_impl(
        &self,
        compiler: &mut Compiler,
    ) -> Result<(), ParseErrorKind> {
        match &self.kind {
            PropertyNameKind::Identifier(identifier) => {
                compiler.push(Instruction::Push(identifier.as_str().into()));
            }
            PropertyNameKind::Expression(expression) => {
                expression.compile(compiler);
            }
        }
        Ok(())
    }
}

impl Compile for If {
    fn compile_impl(
        &self,
        compiler: &mut Compiler,
    ) -> Result<(), ParseErrorKind> {
        // https://craftinginterpreters.com/jumping-back-and-forth.html
        //
        // PUSH <condition>
        // JUMP_IF_NOT --------+
        // ...then body...     |
        // JUMP -----------+   |
        // ...else body... | <-+
        // ...rest... <----+
        self.condition.compile(compiler);

        // We'll need to backpatch this offset
        let if_index = compiler.instructions.len();
        compiler.push(Instruction::JumpIfNot(0));

        self.body.compile(compiler);

        if let Some(else_body) = &self.else_body {
            // Add a jump from the end of the `then` to skip the `else`
            let else_index = compiler.instructions.len();
            compiler.push(Instruction::Jump(0));

            // Initial jump must point to *after* the second jump
            compiler.backpatch_jump(if_index);

            else_body.compile(compiler);
            compiler.backpatch_jump(else_index);
        } else {
            compiler.backpatch_jump(if_index);
        }

        Ok(())
    }
}

impl Compile for ImportDeclaration {
    fn compile_impl(
        &self,
        _compiler: &mut Compiler,
    ) -> Result<(), ParseErrorKind> {
        todo!()
    }
}
