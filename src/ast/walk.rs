use crate::ast::{
    Argument, ArrayElement, ArrayLiteral, ArrayPatternElement,
    ArrayPatternElementKind, BinaryOperation, Binding, BindingKind, Block,
    Declaration, ExportDeclaration, ExportDeclarationKind, Expression,
    ExpressionKind, FunctionBody, FunctionCall, FunctionDefinition,
    FunctionParameter, FunctionParameterRest, FunctionParameters, Identifier,
    If, ImportDeclaration, ImportModule, ImportNamed, Literal, Module,
    ObjectLiteral, ObjectPatternElement, ObjectProperty, ObjectPropertyKind,
    OptionalPropertyAccess, PropertyAccess, PropertyName, PropertyNameKind,
    Statement, StatementKind, TemplateChunk, TemplateChunkKind,
    TemplateLiteral, TernaryConditional, UnaryOperation, Variable,
};
use std::ops::DerefMut;

/// A trait for applying actions to individual nodes of an AST
///
/// This should be used in conjunction with [`Walk`] to apply an action to _all_
/// nodes of the AST. This trait defines one method for each AST node type,
/// which all do nothing by default. Override individual functions as needed to
/// apply your visitor actions.
///
/// For each [`Node`](crate::ast::Node) type, this trait provides one method
/// called *before* traversing that node's children (`enter_*`), and another
/// called after (`exit_*`). Identifiers specifically have no children, so only
/// [`AstVisitor::visit_identifier`] is provided.
///
/// It would be great to make this a generic trait on the node type, like
/// `AstVisitor<T>` and implement it only for the node types you want to visit,
/// but that requires specialization in order to provide default impls for all
/// other node types.
pub trait AstVisitor {
    /// Called before walking down an [`Argument`]
    fn enter_argument(&mut self, _: &mut Argument) {}
    /// Called after walking down an [`Argument`]
    fn exit_argument(&mut self, _: &mut Argument) {}

    /// Called before walking down an [`ArrayElement`]
    fn enter_array_element(&mut self, _: &mut ArrayElement) {}
    /// Called after walking down an [`ArrayElement`]
    fn exit_array_element(&mut self, _: &mut ArrayElement) {}

    /// Called before walking down an [`ArrayLiteral`]
    fn enter_array_literal(&mut self, _: &mut ArrayLiteral) {}
    /// Called after walking down an [`ArrayLiteral`]
    fn exit_array_literal(&mut self, _: &mut ArrayLiteral) {}

    /// Called before walking down an [`ArrayPatternElement`]
    fn enter_array_pattern_element(&mut self, _: &mut ArrayPatternElement) {}
    /// Called after walking down an [`ArrayPatternElement`]
    fn exit_array_pattern_element(&mut self, _: &mut ArrayPatternElement) {}

    /// Called before walking down a [`BinaryOperation`]
    fn enter_binary(&mut self, _: &mut BinaryOperation) {}
    /// Called after walking down a [`BinaryOperation`]
    fn exit_binary(&mut self, _: &mut BinaryOperation) {}

    /// Called before walking down a [`Binding`]
    fn enter_binding(&mut self, _: &mut Binding) {}
    /// Called after walking down a [`Binding`]
    fn exit_binding(&mut self, _: &mut Binding) {}

    /// Called before walking down a [`Block`]
    fn enter_block(&mut self, _: &mut Block) {}
    /// Called after walking down a [`Block`]
    fn exit_block(&mut self, _: &mut Block) {}

    /// Called before walking down a [`Declaration`]
    fn enter_declaration(&mut self, _: &mut Declaration) {}
    /// Called after walking down a [`Declaration`]
    fn exit_declaration(&mut self, _: &mut Declaration) {}

    /// Called before walking down an [`ExportDeclaration`]
    fn enter_export(&mut self, _: &mut ExportDeclaration) {}
    /// Called after walking down an [`ExportDeclaration`]
    fn exit_export(&mut self, _: &mut ExportDeclaration) {}

    /// Called before walking down an [`Expression`]
    fn enter_expression(&mut self, _: &mut Expression) {}
    /// Called after walking down an [`Expression`]
    fn exit_expression(&mut self, _: &mut Expression) {}

    /// Called before walking down a [`FunctionBody`]
    fn enter_function_body(&mut self, _: &mut FunctionBody) {}
    /// Called after walking down a [`FunctionBody`]
    fn exit_function_body(&mut self, _: &mut FunctionBody) {}

    /// Called before walking down a [`FunctionCall`]
    fn enter_function_call(&mut self, _: &mut FunctionCall) {}
    /// Called after walking down a [`FunctionCall`]
    fn exit_function_call(&mut self, _: &mut FunctionCall) {}

    /// Called before walking down a [`FunctionDefinition`]
    fn enter_function_definition(&mut self, _: &mut FunctionDefinition) {}
    /// Called after walking down a [`FunctionDefinition`]
    fn exit_function_definition(&mut self, _: &mut FunctionDefinition) {}

    /// Called before walking down a [`FunctionParameters`]
    fn enter_function_parameters(&mut self, _: &mut FunctionParameters) {}
    /// Called after walking down a [`FunctionParameters`]
    fn exit_function_parameters(&mut self, _: &mut FunctionParameters) {}

    /// Called before walking down a [`FunctionParameter`]
    fn enter_function_parameter(&mut self, _: &mut FunctionParameter) {}
    /// Called after walking down a [`FunctionParameter`]
    fn exit_function_parameter(&mut self, _: &mut FunctionParameter) {}

    /// Called before walking down a [`FunctionParameterRest`]
    fn enter_function_parameter_rest(&mut self, _: &mut FunctionParameterRest) {
    }
    /// Called after walking down a [`FunctionParameterRest`]
    fn exit_function_parameter_rest(&mut self, _: &mut FunctionParameterRest) {}

    /// Called before walking down an [`If`]
    fn enter_if(&mut self, _: &mut If) {}
    /// Called after walking down an [`If`]
    fn exit_if(&mut self, _: &mut If) {}

    /// Called before walking down an [`ImportDeclaration`]
    fn enter_import(&mut self, _: &mut ImportDeclaration) {}
    /// Called after walking down an [`ImportDeclaration`]
    fn exit_import(&mut self, _: &mut ImportDeclaration) {}

    /// Called before walking down an [`ImportNamed`]
    fn enter_import_named(&mut self, _: &mut ImportNamed) {}
    /// Called after walking down an [`ImportNamed`]
    fn exit_import_named(&mut self, _: &mut ImportNamed) {}

    /// Called before walking down an [`ImportModule`]
    fn enter_import_module(&mut self, _: &mut ImportModule) {}
    /// Called after walking down an [`ImportModule`]
    fn exit_import_module(&mut self, _: &mut ImportModule) {}

    /// Called before walking down a [`Literal`]
    fn enter_literal(&mut self, _: &mut Literal) {}
    /// Called after walking down a [`Literal`]
    fn exit_literal(&mut self, _: &mut Literal) {}

    /// Called before walking down a [`Module`]
    fn enter_module(&mut self, _: &mut Module) {}
    /// Called after walking down a [`Module`]
    fn exit_module(&mut self, _: &mut Module) {}

    /// Called before walking down an [`ObjectLiteral`]
    fn enter_object_literal(&mut self, _: &mut ObjectLiteral) {}
    /// Called after walking down an [`ObjectLiteral`]
    fn exit_object_literal(&mut self, _: &mut ObjectLiteral) {}

    /// Called before walking down an [`ObjectPatternElement`]
    fn enter_object_pattern_element(&mut self, _: &mut ObjectPatternElement) {}
    /// Called after walking down an [`ObjectPatternElement`]
    fn exit_object_pattern_element(&mut self, _: &mut ObjectPatternElement) {}

    /// Called before walking down an [`ObjectProperty`]
    fn enter_object_property(&mut self, _: &mut ObjectProperty) {}
    /// Called after walking down an [`ObjectProperty`]
    fn exit_object_property(&mut self, _: &mut ObjectProperty) {}

    /// Called before walking down an [`OptionalPropertyAccess`]
    fn enter_optional_property(&mut self, _: &mut OptionalPropertyAccess) {}
    /// Called after walking down an [`OptionalPropertyAccess`]
    fn exit_optional_property(&mut self, _: &mut OptionalPropertyAccess) {}

    /// Called before walking down a [`PropertyAccess`]
    fn enter_property_access(&mut self, _: &mut PropertyAccess) {}
    /// Called after walking down a [`PropertyAccess`]
    fn exit_property_access(&mut self, _: &mut PropertyAccess) {}

    /// Called before walking down a [`PropertyName`]
    fn enter_property_name(&mut self, _: &mut PropertyName) {}
    /// Called after walking down a [`PropertyName`]
    fn exit_property_name(&mut self, _: &mut PropertyName) {}

    /// Called before walking down a [`Statement`]
    fn enter_statement(&mut self, _: &mut Statement) {}
    /// Called after walking down a [`Statement`]
    fn exit_statement(&mut self, _: &mut Statement) {}

    /// Called before walking down a [`TemplateLiteral`]
    fn enter_template(&mut self, _: &mut TemplateLiteral) {}
    /// Called after walking down a [`TemplateLiteral`]
    fn exit_template(&mut self, _: &mut TemplateLiteral) {}

    /// Called before walking down a [`TemplateChunk`]
    fn enter_template_chunk(&mut self, _: &mut TemplateChunk) {}
    /// Called after walking down a [`TemplateChunk`]
    fn exit_template_chunk(&mut self, _: &mut TemplateChunk) {}

    /// Called before walking down a [`TernaryConditional`]
    fn enter_ternary(&mut self, _: &mut TernaryConditional) {}
    /// Called after walking down a [`TernaryConditional`]
    fn exit_ternary(&mut self, _: &mut TernaryConditional) {}

    /// Called before walking down a [`UnaryOperation`]
    fn enter_unary(&mut self, _: &mut UnaryOperation) {}
    /// Called after walking down a [`UnaryOperation`]
    fn exit_unary(&mut self, _: &mut UnaryOperation) {}

    /// Called before walking down a [`Variable`]
    fn enter_variable(&mut self, _: &mut Variable) {}
    /// Called after walking down a [`Variable`]
    fn exit_variable(&mut self, _: &mut Variable) {}

    /// Called when walking over an [`Identifier`]
    ///
    /// Identifiers can be used in 3 positions; see [`IdentifierPosition`].
    fn visit_identifier(
        &mut self,
        _identifier: &mut Identifier,
        _position: IdentifierPosition,
    ) {
    }
}

/// Walk over an AST node and all its children, applying some transformation
///
/// This never needs to be implemented outside this file. Instead, implement
/// [`AstVisitor`] and override the methods as needed to visit/modify each
/// individual AST node. The implementations of `Walk` will take care of moving
/// down the tree for you.
///
/// > Until you're part of this Turbo Team, WALK SLOWLY.
pub trait Walk {
    /// Walk down the AST, calling the given visitor for each individual node
    ///
    /// At the start of each `walk()` call, the node will call its corresponding
    /// `enter_` method of [`AstVisitor`]. After walking through all children,
    /// it will call the `exit_` method.
    fn walk(&mut self, visitor: &mut dyn AstVisitor);
}

impl Walk for Module {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_module(self);
        self.statements.walk(visitor);
        visitor.exit_module(self);
    }
}

impl Walk for Statement {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_statement(self);
        match &mut self.kind {
            StatementKind::Declaration(declaration) => {
                declaration.walk(visitor);
            }
            StatementKind::Expression(expression) => expression.walk(visitor),
            StatementKind::Block(block) => block.statements.walk(visitor),
            StatementKind::If(if_statement) => if_statement.walk(visitor),
            StatementKind::Return(expression) => expression.walk(visitor),
            StatementKind::Import(import) => import.walk(visitor),
            StatementKind::Export(export) => export.walk(visitor),
            StatementKind::Empty => {}
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
        self.variables.walk(visitor);
        visitor.exit_declaration(self);
    }
}

impl Walk for Expression {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_expression(self);
        match &mut self.kind {
            ExpressionKind::Parenthesized(expression) => {
                expression.walk(visitor);
            }
            ExpressionKind::Literal(literal) => literal.walk(visitor),
            ExpressionKind::Template(template) => template.walk(visitor),
            ExpressionKind::Identifier(identifier) => visitor
                .visit_identifier(identifier, IdentifierPosition::Reference),
            ExpressionKind::Call(call) => call.walk(visitor),
            ExpressionKind::Property(property) => property.walk(visitor),
            ExpressionKind::OptionalProperty(property) => {
                property.walk(visitor);
            }
            ExpressionKind::Function(function) => function.walk(visitor),
            ExpressionKind::Unary(unary) => unary.walk(visitor),
            ExpressionKind::Binary(binary) => binary.walk(visitor),
            ExpressionKind::Ternary(ternary) => ternary.walk(visitor),
        }
        visitor.exit_expression(self);
    }
}

impl Walk for Variable {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_variable(self);
        self.binding.walk(visitor);
        self.init.walk(visitor);
        visitor.exit_variable(self);
    }
}

impl Walk for Binding {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_binding(self);
        match &mut self.kind {
            BindingKind::Identifier(identifier) => visitor
                .visit_identifier(identifier, IdentifierPosition::Declaration),
            BindingKind::Object { properties, rest } => {
                properties.walk(visitor);
                rest.walk(visitor);
            }
            BindingKind::Array { elements, rest } => {
                elements.walk(visitor);
                rest.walk(visitor);
            }
        }
        visitor.exit_binding(self);
    }
}

impl Walk for ArrayPatternElement {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_array_pattern_element(self);
        match &mut self.kind {
            ArrayPatternElementKind::Identifier { binding, init } => {
                binding.walk(visitor);
                init.walk(visitor);
            }
            ArrayPatternElementKind::Skip => {}
        }
        visitor.exit_array_pattern_element(self);
    }
}

impl Walk for ObjectPatternElement {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_object_pattern_element(self);
        self.key.walk(visitor);
        self.binding.walk(visitor);
        self.init.walk(visitor);
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

impl Walk for FunctionDefinition {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_function_definition(self);
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

impl Walk for FunctionParameters {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_function_parameters(self);
        self.parameters.walk(visitor);
        self.rest.walk(visitor);
        visitor.exit_function_parameters(self);
    }
}

impl Walk for FunctionParameter {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_function_parameter(self);
        self.binding.walk(visitor);
        self.init.walk(visitor);
        visitor.exit_function_parameter(self);
    }
}

impl Walk for FunctionParameterRest {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_function_parameter_rest(self);
        self.binding.walk(visitor);
        visitor.exit_function_parameter_rest(self);
    }
}

impl Walk for PropertyName {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_property_name(self);
        match &mut self.kind {
            PropertyNameKind::Identifier(identifier) => visitor
                .visit_identifier(identifier, IdentifierPosition::Property),
            PropertyNameKind::Expression(expression) => {
                expression.walk(visitor);
            }
        }
        visitor.exit_property_name(self);
    }
}

impl Walk for PropertyAccess {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_property_access(self);
        self.property.walk(visitor);
        self.object.walk(visitor);
        visitor.exit_property_access(self);
    }
}

impl Walk for OptionalPropertyAccess {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_optional_property(self);
        self.property.walk(visitor);
        self.object.walk(visitor);
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
        match &mut self.kind {
            TemplateChunkKind::Literal(_) => {} // Nothing to walk here
            TemplateChunkKind::Expression(expression) => {
                expression.walk(visitor);
            }
        }
        visitor.exit_template_chunk(self);
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
        self.expression.walk(visitor);
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
        match &mut self.kind {
            ObjectPropertyKind::Property {
                property,
                expression,
            } => {
                property.walk(visitor);
                expression.walk(visitor);
            }
            ObjectPropertyKind::Spread(expression) => expression.walk(visitor),
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

impl Walk for Argument {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_argument(self);
        match self {
            Self::Expression(expression) | Self::Spread(expression) => {
                expression.walk(visitor);
            }
        }
        visitor.exit_argument(self);
    }
}

impl Walk for ImportDeclaration {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_import(self);
        // Default identifier is a declaration
        if let Some(identifier) = &mut self.default {
            visitor
                .visit_identifier(identifier, IdentifierPosition::Declaration);
        }
        self.named.walk(visitor);
        self.module.walk(visitor);
        visitor.exit_import(self);
    }
}

impl Walk for ImportNamed {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_import_named(self);
        visitor
            .visit_identifier(&mut self.imported, IdentifierPosition::Property);
        visitor
            .visit_identifier(&mut self.local, IdentifierPosition::Declaration);
        visitor.exit_import_named(self);
    }
}

impl Walk for ImportModule {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_import_module(self);
        // Nothing to do here. We could add a visit_module_name but I don't
        // think it would have any value
        visitor.exit_import_module(self);
    }
}

impl Walk for ExportDeclaration {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.enter_export(self);
        match &mut self.kind {
            ExportDeclarationKind::Declaration(declaration) => {
                declaration.walk(visitor);
            }
            ExportDeclarationKind::DefaultExpression(expression) => {
                expression.walk(visitor);
            }
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

impl<T: Walk> Walk for Box<[T]> {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        self.iter_mut().for_each(|element| element.walk(visitor));
    }
}

impl<T: Walk> Walk for Option<T> {
    fn walk(&mut self, visitor: &mut dyn AstVisitor) {
        if let Some(element) = self {
            element.walk(visitor);
        }
    }
}

/// Syntactic position in which an [`Identifier`] is used
#[derive(Copy, Clone, Debug)]
pub enum IdentifierPosition {
    /// Binding position declaring a new name
    ///
    /// Such as `x` in `const x = 1`, `(x) => {...}`.
    Declaration,
    /// Expression position
    ///
    /// Such as `x` in `x + 1` or `const o = { x }`.
    Reference,
    /// Property name for an object
    ///
    /// Such as `x` in `o.x`, `const { x: x1 } = o`, or `import { x: x1 } from
    /// 'source'`.
    ///
    /// These usages do *not* interact with the local scope at all.
    Property,
}
