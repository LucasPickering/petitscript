use crate::ast::{
    ArrayElement, ArrayLiteral, ArrayPatternElement, BinaryOperation, Binding,
    Block, Declaration, DoWhileLoop, ExportDeclaration, Expression, ForOfLoop,
    FunctionBody, FunctionCall, FunctionDeclaration, FunctionDefinition,
    FunctionParameter, FunctionPointer, Identifier, If, ImportDeclaration,
    ImportModule, ImportNamed, LexicalDeclaration, Literal, Module, Node,
    ObjectLiteral, ObjectPatternElement, ObjectProperty,
    OptionalPropertyAccess, PropertyAccess, PropertyName, Statement,
    TemplateChunk, TemplateLiteral, TernaryConditional, UnaryOperation,
    Variable, WhileLoop,
};
use std::ops::DerefMut;

/// Walk over an AST node and all its children, apply
///
/// This never needs to be implemented outside this file. Instead, implement
/// [AstVisitor] and override the methods as needed to visit/modify each
/// individual AST node. The implementations of `Walk` will take care of moving
/// down the tree for you.
///
/// Until you're part of this Turbo Team, WALK SLOWLY.
pub trait Walk {
    /// Walk down the AST, calling the given visitor for each individual node.
    /// The node will be visited _before_ walking down to its children.
    fn walk(&mut self, visitor: &mut dyn AstVisitor);
}

impl Walk for Module {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_ast(self);
        self.statements.walk(visitor);
        visitor.exit_ast(self);
    }
}

impl Walk for Statement {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_statement(self);
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
        visitor.exit_statement(self);
    }
}

impl Walk for Block {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_block(self);
        self.statements.walk(visitor);
        visitor.exit_block(self);
    }
}

impl Walk for Declaration {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_declaration(self);
        match self {
            Self::Lexical(declaration) => declaration.walk(visitor),
            Self::Function(declaration) => declaration.walk(visitor),
        }
        visitor.exit_declaration(self);
    }
}

impl Walk for LexicalDeclaration {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_lexical_declaration(self);
        self.variables.walk(visitor);
        visitor.exit_lexical_declaration(self);
    }
}

impl Walk for FunctionDeclaration {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_function_declaration(self);
        self.pointer.walk(visitor);
        visitor.exit_function_declaration(self);
    }
}

impl Walk for Expression {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_expression(self);
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
        visitor.exit_expression(self);
    }
}

impl Walk for Variable {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_variable(self);
        self.init.walk(visitor);
        visitor.exit_variable(self);
    }
}

impl Walk for Binding {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_binding(self);
        match self {
            Self::Identifier(identifier) => identifier.walk(visitor),
            Self::Object(elements) => elements.walk(visitor),
            Self::Array(elements) => elements.walk(visitor),
        }
        visitor.exit_binding(self);
    }
}

impl Walk for ArrayPatternElement {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_array_pattern_element(self);
        // TODO
        visitor.exit_array_pattern_element(self);
    }
}

impl Walk for ObjectPatternElement {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_object_pattern_element(self);
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
        visitor.exit_object_pattern_element(self);
    }
}

impl Walk for If {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_if(self);
        self.condition.walk(visitor);
        self.body.walk(visitor);
        self.else_body.walk(visitor);
        visitor.exit_if(self);
    }
}

impl Walk for ForOfLoop {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_for_of_loop(self);
        self.binding.walk(visitor);
        self.iterable.walk(visitor);
        self.body.walk(visitor);
        visitor.exit_for_of_loop(self);
    }
}

impl Walk for WhileLoop {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_while_loop(self);
        self.condition.walk(visitor);
        visitor.exit_while_loop(self);
    }
}

impl Walk for DoWhileLoop {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_do_while_loop(self);
        self.condition.walk(visitor);
        visitor.exit_do_while_loop(self);
    }
}

impl Walk for FunctionPointer {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_function_pointer(self);
        match self {
            Self::Inline(definition) => definition.walk(visitor),
            Self::Lifted(_) => {}
        }
        visitor.exit_function_pointer(self);
    }
}

impl Walk for FunctionDefinition {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_function_definition(self);
        self.name.walk(visitor);
        self.parameters.walk(visitor);
        self.body.walk(visitor);
        visitor.exit_function_definition(self);
    }
}

impl Walk for FunctionBody {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_function_body(self);
        match self {
            Self::Expression(expression) => expression.walk(visitor),
            Self::Block(block) => block.walk(visitor),
        }
        visitor.exit_function_body(self);
    }
}

impl Walk for FunctionParameter {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_function_parameter(self);
        self.variable.walk(visitor);
        visitor.exit_function_parameter(self);
    }
}

impl Walk for PropertyName {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_property_name(self);
        match self {
            Self::Literal(identifier) => identifier.walk(visitor),
            Self::Expression(expression) => expression.walk(visitor),
        }
        visitor.exit_property_name(self);
    }
}

impl Walk for PropertyAccess {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_property_access(self);
        self.property.walk(visitor);
        self.expression.walk(visitor);
        visitor.exit_property_access(self);
    }
}

impl Walk for OptionalPropertyAccess {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_optional_property(self);
        self.property.walk(visitor);
        self.expression.walk(visitor);
        visitor.exit_optional_property(self);
    }
}

impl Walk for Literal {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_literal(self);
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
        visitor.exit_literal(self);
    }
}

impl Walk for TemplateLiteral {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_template(self);
        self.chunks.walk(visitor);
        visitor.exit_template(self);
    }
}

impl Walk for TemplateChunk {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_template_chunk(self);
        match self {
            TemplateChunk::Literal(_) => {} // Nothing to walk here
            TemplateChunk::Expression(expression) => expression.walk(visitor),
        }
        visitor.exit_template_chunk(self);
    }
}

impl Walk for Identifier {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_identifier(self);
    }
}

impl Walk for ArrayLiteral {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_array_literal(self);
        self.elements.walk(visitor);
        visitor.exit_array_literal(self);
    }
}

impl Walk for ArrayElement {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_array_element(self);
        match self {
            Self::Expression(expression) | Self::Spread(expression) => {
                expression.walk(visitor)
            }
        }
        visitor.exit_array_element(self);
    }
}

impl Walk for ObjectLiteral {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_object_literal(self);
        self.properties.walk(visitor);
        visitor.exit_object_literal(self);
    }
}

impl Walk for ObjectProperty {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_object_property(self);
        match self {
            Self::Property { expression, .. } => expression.walk(visitor),
            Self::Identifier(_) => {}
            Self::Spread(expression) => expression.walk(visitor),
        }
        visitor.exit_object_property(self);
    }
}

impl Walk for FunctionCall {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_function_call(self);
        self.function.walk(visitor);
        self.arguments.walk(visitor);
        visitor.exit_function_call(self);
    }
}

impl Walk for ImportDeclaration {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_import(self);
        self.default.walk(visitor);
        self.named.walk(visitor);
        self.module.walk(visitor);
        visitor.exit_import(self);
    }
}

impl Walk for ImportNamed {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_import_named(self);
        self.identifier.walk(visitor);
        self.rename.walk(visitor);
        visitor.exit_import_named(self);
    }
}

impl Walk for ImportModule {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_import_module(self);
        match self {
            ImportModule::Native(_) => {
                // Nothing to do here. We could add a visit_module_name but
                // I don't think it would have any value
            }
            // woo baby a whole nested module!!
            ImportModule::Local(module) => module.walk(visitor),
        }
        visitor.exit_import_module(self);
    }
}

impl Walk for ExportDeclaration {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_export(self);
        match self {
            Self::Reexport {} => todo!(),
            Self::Declaration(declaration) => declaration.walk(visitor),
            Self::DefaultFunctionDeclaration(declaration) => {
                declaration.walk(visitor)
            }
            Self::DefaultExpression(expression) => expression.walk(visitor),
        }
        visitor.exit_export(self);
    }
}

impl Walk for UnaryOperation {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_unary(self);
        self.expression.walk(visitor);
        visitor.exit_unary(self);
    }
}

impl Walk for BinaryOperation {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_binary(self);
        self.lhs.walk(visitor);
        self.rhs.walk(visitor);
        visitor.exit_binary(self);
    }
}

impl Walk for TernaryConditional {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_ternary(self);
        self.condition.walk(visitor);
        self.true_expression.walk(visitor);
        self.false_expression.walk(visitor);
        visitor.exit_ternary(self);
    }
}

impl<T: Walk> Walk for Box<T> {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        self.deref_mut().walk(visitor);
    }
}

impl<T: Walk> Walk for Node<T> {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        self.data_mut().walk(visitor);
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
///
/// For each node type, this trait provides one method called *before*
/// traversing that node's children (`enter_*`), and another called after
/// (`exit_*`). Each `visit` method is called on a node _before_ traversing that
/// node's children. For nodes with no children, only a `visit_*` method is
/// provided.
///
/// It would be great to make this a generic trait on the node type, like
/// `AstVisitor<T>` and implement it only for the node types you want to visit,
/// but that requires specialization in order to provide default impls for all
/// other node types.
pub trait AstVisitor {
    fn enter_array_element(&mut self, _: &mut ArrayElement) {}
    fn exit_array_element(&mut self, _: &mut ArrayElement) {}

    fn enter_array_literal(&mut self, _: &mut ArrayLiteral) {}
    fn exit_array_literal(&mut self, _: &mut ArrayLiteral) {}

    fn enter_array_pattern_element(&mut self, _: &mut ArrayPatternElement) {}
    fn exit_array_pattern_element(&mut self, _: &mut ArrayPatternElement) {}

    fn enter_ast(&mut self, _: &mut Module) {}
    fn exit_ast(&mut self, _: &mut Module) {}

    fn enter_binary(&mut self, _: &mut BinaryOperation) {}
    fn exit_binary(&mut self, _: &mut BinaryOperation) {}

    fn enter_binding(&mut self, _: &mut Binding) {}
    fn exit_binding(&mut self, _: &mut Binding) {}

    fn enter_block(&mut self, _: &mut Block) {}
    fn exit_block(&mut self, _: &mut Block) {}

    fn enter_declaration(&mut self, _: &mut Declaration) {}
    fn exit_declaration(&mut self, _: &mut Declaration) {}

    fn enter_do_while_loop(&mut self, _: &mut DoWhileLoop) {}
    fn exit_do_while_loop(&mut self, _: &mut DoWhileLoop) {}

    fn enter_export(&mut self, _: &mut ExportDeclaration) {}
    fn exit_export(&mut self, _: &mut ExportDeclaration) {}

    fn enter_expression(&mut self, _: &mut Expression) {}
    fn exit_expression(&mut self, _: &mut Expression) {}

    fn enter_for_of_loop(&mut self, _: &mut ForOfLoop) {}
    fn exit_for_of_loop(&mut self, _: &mut ForOfLoop) {}

    fn enter_function_body(&mut self, _: &mut FunctionBody) {}
    fn exit_function_body(&mut self, _: &mut FunctionBody) {}

    fn enter_function_call(&mut self, _: &mut FunctionCall) {}
    fn exit_function_call(&mut self, _: &mut FunctionCall) {}

    fn enter_function_declaration(&mut self, _: &mut FunctionDeclaration) {}
    fn exit_function_declaration(&mut self, _: &mut FunctionDeclaration) {}

    fn enter_function_definition(&mut self, _: &mut FunctionDefinition) {}
    fn exit_function_definition(&mut self, _: &mut FunctionDefinition) {}

    fn enter_function_parameter(&mut self, _: &mut FunctionParameter) {}
    fn exit_function_parameter(&mut self, _: &mut FunctionParameter) {}

    fn enter_function_pointer(&mut self, _: &mut FunctionPointer) {}
    fn exit_function_pointer(&mut self, _: &mut FunctionPointer) {}

    fn visit_identifier(&mut self, _: &mut Identifier) {}

    fn enter_if(&mut self, _: &mut If) {}
    fn exit_if(&mut self, _: &mut If) {}

    fn enter_import(&mut self, _: &mut ImportDeclaration) {}
    fn exit_import(&mut self, _: &mut ImportDeclaration) {}

    fn enter_import_named(&mut self, _: &mut ImportNamed) {}
    fn exit_import_named(&mut self, _: &mut ImportNamed) {}

    fn enter_import_module(&mut self, _: &mut ImportModule) {}
    fn exit_import_module(&mut self, _: &mut ImportModule) {}

    fn enter_lexical_declaration(&mut self, _: &mut LexicalDeclaration) {}
    fn exit_lexical_declaration(&mut self, _: &mut LexicalDeclaration) {}

    fn enter_literal(&mut self, _: &mut Literal) {}
    fn exit_literal(&mut self, _: &mut Literal) {}

    fn enter_object_literal(&mut self, _: &mut ObjectLiteral) {}
    fn exit_object_literal(&mut self, _: &mut ObjectLiteral) {}

    fn enter_object_pattern_element(&mut self, _: &mut ObjectPatternElement) {}
    fn exit_object_pattern_element(&mut self, _: &mut ObjectPatternElement) {}

    fn enter_object_property(&mut self, _: &mut ObjectProperty) {}
    fn exit_object_property(&mut self, _: &mut ObjectProperty) {}

    fn enter_optional_property(&mut self, _: &mut OptionalPropertyAccess) {}
    fn exit_optional_property(&mut self, _: &mut OptionalPropertyAccess) {}

    fn enter_property_access(&mut self, _: &mut PropertyAccess) {}
    fn exit_property_access(&mut self, _: &mut PropertyAccess) {}

    fn enter_property_name(&mut self, _: &mut PropertyName) {}
    fn exit_property_name(&mut self, _: &mut PropertyName) {}

    fn enter_statement(&mut self, _: &mut Statement) {}
    fn exit_statement(&mut self, _: &mut Statement) {}

    fn enter_template(&mut self, _: &mut TemplateLiteral) {}
    fn exit_template(&mut self, _: &mut TemplateLiteral) {}

    fn enter_template_chunk(&mut self, _: &mut TemplateChunk) {}
    fn exit_template_chunk(&mut self, _: &mut TemplateChunk) {}

    fn enter_ternary(&mut self, _: &mut TernaryConditional) {}
    fn exit_ternary(&mut self, _: &mut TernaryConditional) {}

    fn enter_unary(&mut self, _: &mut UnaryOperation) {}
    fn exit_unary(&mut self, _: &mut UnaryOperation) {}

    fn enter_variable(&mut self, _: &mut Variable) {}
    fn exit_variable(&mut self, _: &mut Variable) {}

    fn enter_while_loop(&mut self, _: &mut WhileLoop) {}
    fn exit_while_loop(&mut self, _: &mut WhileLoop) {}
}
