//! AST to stack machine compilation

use crate::{
    Program, Value,
    ast::{
        ArrayLiteral, BinaryOperator, Block, Declaration, ExportDeclaration,
        ExportDeclarationKind, Expression, ExpressionKind, Identifier, If,
        ImportDeclaration, Literal, Module, Node, ObjectLiteral,
        ObjectPropertyKind, PropertyName, PropertyNameKind, Statement,
        StatementKind,
    },
    value::Number,
};

pub struct Compiler {
    /// Compiled set of instructions
    ///
    /// This is the final result of compilation.
    instructions: Vec<Instruction>,
}

impl Compiler {
    /// Compile a parsed program into a stack machine program
    pub fn compile(program: &Program) -> Vec<Instruction> {
        let mut compiler = Self {
            instructions: vec![],
        };
        program.root().compile(&mut compiler);
        compiler.instructions
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
    /// Move the top value to the machine's exports
    ///
    /// Error if the name (or default) is already exported.
    Export(
        /// `None` for default, `Some` for named exports
        Option<Identifier>,
    ),
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
        // TODO declare a new scope
        for statement in &self.statements {
            statement.compile(compiler);
        }
    }
}

impl Compile for Declaration {
    fn compile(&self, _compiler: &mut Compiler) {
        todo!()
    }
}

impl Compile for ExportDeclaration {
    fn compile(&self, compiler: &mut Compiler) {
        match &self.kind {
            ExportDeclarationKind::Declaration(_) => todo!(),
            ExportDeclarationKind::DefaultExpression(expression) => {
                expression.compile(compiler);
                compiler.push(Instruction::Export(None));
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
            ExpressionKind::Identifier(_) => todo!(),
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
