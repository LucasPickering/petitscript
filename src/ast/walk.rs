use crate::ast::{
    ArrayElement, ArrayLiteral, ArrayPatternElement, Ast, BinaryOperation,
    Binding, Declaration, DoWhileLoop, ExportDeclaration, Expression,
    ForOfLoop, FunctionCall, FunctionDeclaration, FunctionDefinition,
    FunctionParameter, FunctionPointer, Identifier, If, ImportDeclaration,
    LexicalDeclaration, Literal, ObjectLiteral, ObjectPatternElement,
    ObjectProperty, OptionalPropertyAccess, PropertyAccess, PropertyName,
    Spanned, Statement, TemplateLiteral, TernaryConditional, UnaryOperation,
    Variable, WhileLoop,
};
use std::ops::DerefMut;

/// Walk over an AST node and all its children, apply
///
/// This never needs to be implemented outside this file. Instead, implement
/// [AstVisitor] and override the methods as needed to visit/modify each
/// individual AST node. The implementations of `Walk` will take care of moving
/// down the tree for you.
pub trait Walk {
    /// Walk down the AST, calling the given visitor for each individual node.
    /// The node will be visited _before_ walking down to its children.
    fn walk(&mut self, visitor: &mut dyn AstVisitor);
}

impl Walk for Ast {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_ast(self);
        self.statements.walk(visitor)
    }
}

impl Walk for Statement {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_statement(self);
        match self {
            Self::Declaration(declaration) => declaration.walk(visitor),
            Self::Expression(expression) => expression.walk(visitor),
            Self::Block(block) => block.statements.walk(visitor),
            Self::If(if_statement) => if_statement.walk(visitor),
            Self::ForOfLoop(for_of_loop) => for_of_loop.walk(visitor),
            Self::WhileLoop(while_loop) => while_loop.walk(visitor),
            Self::DoWhileLoop(do_while_loop) => do_while_loop.walk(visitor),
            Self::Return(expression) => expression.walk(visitor),
            Self::Import(import) => import.walk(visitor),
            Self::Export(export) => export.walk(visitor),
            Self::Empty | Self::Break | Self::Continue => {}
        }
    }
}

impl Walk for Declaration {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_declaration(self);
        match self {
            Self::Lexical(declaration) => declaration.walk(visitor),
            Self::Function(declaration) => declaration.walk(visitor),
        }
    }
}

impl Walk for LexicalDeclaration {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_lexical_declaration(self);
        self.variables.walk(visitor);
    }
}

impl Walk for FunctionDeclaration {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_function_declaration(self);
        self.pointer.walk(visitor)
    }
}

impl Walk for Expression {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_expression(self);
        match self {
            Self::Parenthesized(expression) => expression.walk(visitor),
            Self::Literal(literal) => literal.walk(visitor),
            Self::Template(template) => template.walk(visitor),
            Self::Identifier(identifier) => identifier.walk(visitor),
            Self::Call(call) => call.walk(visitor),
            Self::Property(property) => property.walk(visitor),
            Self::OptionalProperty(property) => property.walk(visitor),
            Self::ArrowFunction(function) => function.walk(visitor),
            Self::Unary(unary) => unary.walk(visitor),
            Self::Binary(binary) => binary.walk(visitor),
            Self::Ternary(ternary) => ternary.walk(visitor),
        }
    }
}

impl Walk for Variable {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_variable(self);
        self.init.walk(visitor);
    }
}

impl Walk for Binding {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_binding(self);
        match self {
            Self::Identifier(identifier) => identifier.walk(visitor),
            Self::Object(elements) => elements.walk(visitor),
            Self::Array(elements) => elements.walk(visitor),
        }
    }
}

impl Walk for ArrayPatternElement {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_array_pattern_element(self);
        todo!()
    }
}

impl Walk for ObjectPatternElement {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_object_pattern_element(self);
        match self {
            Self::Identifier { identifier, init } => {
                identifier.walk(visitor);
                init.walk(visitor);
            }
            Self::Mapped { key, value, init } => {
                key.walk(visitor);
                value.walk(visitor);
                init.walk(visitor);
            }
            Self::Rest { binding, init } => {
                binding.walk(visitor);
                init.walk(visitor);
            }
        }
    }
}

impl Walk for If {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_if(self);
        self.condition.walk(visitor);
        self.body.walk(visitor);
        self.else_body.walk(visitor);
    }
}

impl Walk for ForOfLoop {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_for_of_loop(self);
        self.binding.walk(visitor);
        self.iterable.walk(visitor);
        self.body.walk(visitor);
    }
}

impl Walk for WhileLoop {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_while_loop(self);
        self.condition.walk(visitor);
    }
}

impl Walk for DoWhileLoop {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_do_while_loop(self);
        self.condition.walk(visitor);
    }
}

impl Walk for FunctionPointer {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_function_pointer(self);
        match self {
            Self::Inline(definition) => definition.walk(visitor),
            Self::Lifted(_) => {}
        }
    }
}

impl Walk for FunctionDefinition {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_function_definition(self);
        self.name.walk(visitor);
        self.parameters.walk(visitor);
        self.body.walk(visitor);
    }
}

impl Walk for FunctionParameter {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_function_parameter(self);
        self.variable.walk(visitor);
    }
}

impl Walk for PropertyName {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_property_name(self);
        match self {
            Self::Literal(identifier) => identifier.walk(visitor),
            Self::Expression(expression) => expression.walk(visitor),
        }
    }
}

impl Walk for PropertyAccess {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_property_access(self);
        self.property.walk(visitor);
        self.expression.walk(visitor);
    }
}

impl Walk for OptionalPropertyAccess {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_optional_property(self);
        self.property.walk(visitor);
        self.expression.walk(visitor);
    }
}

impl Walk for Literal {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_literal(self);
        match self {
            Self::Null => {}
            Self::Undefined => {}
            Self::Boolean(_) => {}
            Self::Float(_) => {}
            Self::Int(_) => {}
            Self::String(_) => {}
            Self::Array(array_literal) => array_literal.walk(visitor),
            Self::Object(object_literal) => object_literal.walk(visitor),
        }
    }
}

impl Walk for TemplateLiteral {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_template(self);
        // TODO
    }
}

impl Walk for Identifier {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_identifier(self);
    }
}

impl Walk for ArrayLiteral {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_array_literal(self);
        self.elements.walk(visitor)
    }
}

impl Walk for ArrayElement {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_array_element(self);
        match self {
            Self::Expression(expression) | Self::Spread(expression) => {
                expression.walk(visitor)
            }
        }
    }
}

impl Walk for ObjectLiteral {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_object_literal(self);
        self.properties.walk(visitor);
    }
}

impl Walk for ObjectProperty {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_object_property(self);
        match self {
            Self::Property { expression, .. } => expression.walk(visitor),
            Self::Identifier(_) => {}
            Self::Spread(expression) => expression.walk(visitor),
        }
    }
}

impl Walk for FunctionCall {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_call(self);
        self.function.walk(visitor);
        self.arguments.walk(visitor);
    }
}

impl Walk for ImportDeclaration {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_import(self);
        // TODO
    }
}

impl Walk for ExportDeclaration {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_export(self);
        match self {
            Self::Reexport {} => todo!(),
            Self::Declaration(declaration) => declaration.walk(visitor),
            Self::DefaultFunctionDeclaration(declaration) => {
                declaration.walk(visitor)
            }
            Self::DefaultExpression(expression) => expression.walk(visitor),
        }
    }
}

impl Walk for UnaryOperation {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_unary(self);
        self.expression.walk(visitor);
    }
}

impl Walk for BinaryOperation {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_binary(self);
        self.lhs.walk(visitor);
        self.rhs.walk(visitor);
    }
}

impl Walk for TernaryConditional {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_ternary(self);
        self.condition.walk(visitor);
        self.true_expression.walk(visitor);
        self.false_expression.walk(visitor);
    }
}

impl<T: Walk> Walk for Box<T> {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        self.deref_mut().walk(visitor);
    }
}

impl<T: Walk> Walk for Spanned<T> {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        self.data.walk(visitor);
    }
}

impl<T: Walk> Walk for Box<[T]> {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        self.iter_mut().for_each(|element| element.walk(visitor));
    }
}

impl<T: Walk> Walk for Option<T> {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        self.iter_mut().for_each(|element| element.walk(visitor));
    }
}

/// A trait for applying actions to individual nodes of an AST. This should be
/// used in conjunction with [Walk] to apply an action to _all_ nodes of the
/// AST. This trait defines one method for each AST node type, which all do
/// nothing by default. Override individual functions as needed to apply your
/// visitor actions.
///
/// Each `visit` method is called on a node _before_ traversing that node's
/// children. That means you can apply any modifications that may affect the
/// AST walk (e.g. adding or remove child nodes), and the *new* value will be
/// the one that gets walked.
pub trait AstVisitor {
    fn visit_array_element(&mut self, _: &mut ArrayElement) {}

    fn visit_array_literal(&mut self, _: &mut ArrayLiteral) {}

    fn visit_array_pattern_element(&mut self, _: &mut ArrayPatternElement) {}

    fn visit_ast(&mut self, _: &mut Ast) {}

    fn visit_binary(&mut self, _: &mut BinaryOperation) {}

    fn visit_binding(&mut self, _: &mut Binding) {}

    fn visit_call(&mut self, _: &mut FunctionCall) {}

    fn visit_declaration(&mut self, _: &mut Declaration) {}

    fn visit_do_while_loop(&mut self, _: &mut DoWhileLoop) {}

    fn visit_export(&mut self, _: &mut ExportDeclaration) {}

    fn visit_expression(&mut self, _: &mut Expression) {}

    fn visit_for_of_loop(&mut self, _: &mut ForOfLoop) {}

    fn visit_function_declaration(&mut self, _: &mut FunctionDeclaration) {}

    fn visit_function_definition(&mut self, _: &mut FunctionDefinition) {}

    fn visit_function_parameter(&mut self, _: &mut FunctionParameter) {}

    fn visit_function_pointer(&mut self, _: &mut FunctionPointer) {}

    fn visit_identifier(&mut self, _: &mut Identifier) {}

    fn visit_if(&mut self, _: &mut If) {}

    fn visit_import(&mut self, _: &mut ImportDeclaration) {}

    fn visit_lexical_declaration(&mut self, _: &mut LexicalDeclaration) {}

    fn visit_literal(&mut self, _: &mut Literal) {}

    fn visit_object_literal(&mut self, _: &mut ObjectLiteral) {}

    fn visit_object_pattern_element(&mut self, _: &mut ObjectPatternElement) {}

    fn visit_object_property(&mut self, _: &mut ObjectProperty) {}

    fn visit_optional_property(&mut self, _: &mut OptionalPropertyAccess) {}

    fn visit_property_access(&mut self, _: &mut PropertyAccess) {}

    fn visit_property_name(&mut self, _: &mut PropertyName) {}

    fn visit_statement(&mut self, _: &mut Statement) {}

    fn visit_template(&mut self, _: &mut TemplateLiteral) {}

    fn visit_ternary(&mut self, _: &mut TernaryConditional) {}

    fn visit_unary(&mut self, _: &mut UnaryOperation) {}

    fn visit_variable(&mut self, _: &mut Variable) {}

    fn visit_while_loop(&mut self, _: &mut WhileLoop) {}
}
