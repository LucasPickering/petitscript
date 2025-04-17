//! Generate source code from AST nodes

use crate::ast::{
    ArrayElement, ArrayLiteral, ArrayPatternElement, BinaryOperation,
    BinaryOperator, Binding, Block, Declaration, DoWhileLoop,
    ExportDeclaration, Expression, ForOfLoop, FunctionCall,
    FunctionDeclaration, FunctionDefinition, FunctionParameter,
    FunctionPointer, Identifier, If, ImportDeclaration, ImportModule,
    ImportNamed, LexicalDeclaration, Literal, Module, NativeModuleName,
    ObjectLiteral, ObjectPatternElement, ObjectProperty,
    OptionalPropertyAccess, PropertyAccess, PropertyName, Statement,
    TemplateChunk, TemplateLiteral, TernaryConditional, UnaryOperation,
    UnaryOperator, Variable, WhileLoop,
};
use std::fmt::{self, Display};

impl Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for statement in &self.statements {
            writeln!(f, "{statement}")?;
        }
        Ok(())
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, ";"),
            Self::Block(block) => write!(f, "{block}"),
            Self::Expression(expression) => write!(f, "{expression}"),
            Self::Declaration(declaration) => write!(f, "{declaration}"),
            Self::If(if_) => write!(f, "{if_}"),
            Self::ForOfLoop(for_of_loop) => write!(f, "{for_of_loop}"),
            Self::WhileLoop(while_loop) => write!(f, "{while_loop}"),
            Self::DoWhileLoop(do_while_loop) => write!(f, "{do_while_loop}"),
            Self::Return(expression) => {
                if let Some(expression) = expression {
                    write!(f, "return {expression};")
                } else {
                    write!(f, "return;")
                }
            }
            Self::Break => write!(f, "break;"),
            Self::Continue => write!(f, "continue;"),
            Self::Import(import) => write!(f, "{import}"),
            Self::Export(export) => write!(f, "{export}"),
        }
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{{")?;
        for statement in &self.statements {
            writeln!(f, "{statement}")?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Declaration::Lexical(lexical) => write!(f, "{lexical}"),
            Declaration::Function(function) => write!(f, "{function}"),
        }
    }
}

impl Display for LexicalDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "const {};", CommaSeparated(&self.variables))
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.binding)?;
        if let Some(init) = &self.init {
            write!(f, " = {init}")?;
        }
        Ok(())
    }
}

impl Display for FunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl Display for FunctionPointer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Source code generation is meant to be run on hand-crafted ASTs,
            // so the function def should still be inline
            Self::Inline(definition) => write!(f, "{definition}"),
            Self::Lifted(id) => write!(f, "TODO"),
        }
    }
}

impl Display for FunctionDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl Display for FunctionParameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.varargs {
            write!(f, "...")?;
        }
        write!(f, "{}", self.variable)
    }
}

impl Display for If {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "if ({condition}) {body}",
            condition = self.condition,
            body = self.body,
        )?;
        if let Some(else_body) = &self.else_body {
            write!(f, "{else_body}")?;
        }
        Ok(())
    }
}

impl Display for ForOfLoop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "for (const {binding} of {iterable}) {body}",
            binding = self.binding,
            iterable = self.iterable,
            body = self.body,
        )
    }
}

impl Display for WhileLoop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "while ({condition}) {body}",
            condition = self.condition,
            body = self.body,
        )
    }
}

impl Display for DoWhileLoop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "do {body} while ({condition})",
            body = self.body,
            condition = self.condition,
        )
    }
}

impl Display for ImportDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "import ")?;
        if let Some(default) = &self.default {
            write!(f, "{default}")?;
        }
        if !self.named.is_empty() {
            write!(f, "{{ {} }}", CommaSeparated(&self.named))?;
        }
        write!(f, " from {};", &self.module)
    }
}

impl Display for ImportNamed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.identifier)?;
        if let Some(rename) = &self.rename {
            write!(f, " as {rename}")?;
        }
        Ok(())
    }
}

impl Display for ImportModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ImportModule::Native(name) => write!(f, "'{name}'"),
            ImportModule::Local(path) => write!(f, "'{path}'"),
        }
    }
}

impl Display for NativeModuleName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Display for ExportDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Reexport {} => write!(f, "TODO"),
            Self::Declaration(declaration) => write!(f, "export {declaration}"),
            Self::DefaultFunctionDeclaration(function_declaration) => {
                write!(f, "TODO")
            }
            Self::DefaultExpression(expression) => {
                write!(f, "export default {expression};")
            }
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parenthesized(expression) => {
                write!(f, "({expression})")
            }
            Self::Literal(literal) => write!(f, "{literal}"),
            Self::Template(template) => write!(f, "{template}"),
            Self::Identifier(identifier) => write!(f, "{identifier}"),
            Self::Call(call) => write!(f, "{call}"),
            Self::Property(property) => write!(f, "{property}"),
            Self::OptionalProperty(property) => write!(f, "{property}"),
            Self::ArrowFunction(function) => write!(f, "{function}"),
            Self::Unary(operation) => write!(f, "{operation}"),
            Self::Binary(operation) => write!(f, "{operation}"),
            Self::Ternary(operation) => write!(f, "{operation}"),
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::Undefined => write!(f, "undefined"),
            Self::Boolean(b) => write!(f, "{b}"),
            Self::Float(float) => write!(f, "{float}"),
            Self::Int(int) => write!(f, "{int}"),
            // TODO escape correctly here
            Self::String(s) => write!(f, "\"{s}\""),
            Self::Array(array_literal) => write!(f, "{array_literal}"),
            Self::Object(object_literal) => write!(f, "{object_literal}"),
        }
    }
}

impl Display for TemplateLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl Display for TemplateChunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]", CommaSeparated(&self.elements))
    }
}

impl Display for ArrayElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Expression(expression) => write!(f, "{expression}"),
            Self::Spread(expression) => write!(f, "...{expression}"),
        }
    }
}

impl Display for ObjectLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{ {} }}", CommaSeparated(&self.properties))
    }
}

impl Display for ObjectProperty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Property {
                property,
                expression,
            } => write!(f, "{property}: {expression}"),
            Self::Identifier(identifier) => write!(f, "{identifier}"),
            Self::Spread(expression) => write!(f, "...{expression}"),
        }
    }
}

impl Display for FunctionCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{function}({arguments})",
            function = self.function,
            arguments = CommaSeparated(&self.arguments),
        )
    }
}

impl Display for PropertyAccess {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl Display for OptionalPropertyAccess {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{expression}?.{property}",
            expression = self.expression,
            property = self.property,
        )
    }
}

impl Display for UnaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{operator}{expression}",
            operator = self.operator,
            expression = self.expression
        )
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BooleanNot => write!(f, "!"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Typeof => write!(f, "typeof "),
        }
    }
}

impl Display for BinaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{lhs} {operator} {rhs}",
            lhs = self.lhs,
            operator = self.operator,
            rhs = self.rhs,
        )
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Mod => write!(f, "%"),
            Self::BooleanAnd => write!(f, "&&"),
            Self::BooleanOr => write!(f, "||"),
            Self::Equal => write!(f, "==="),
            Self::NotEqual => write!(f, "!=="),
            Self::LessThan => write!(f, "<"),
            Self::GreaterThan => write!(f, ">"),
            Self::LessThanEqual => write!(f, "<="),
            Self::GreaterThanEqual => write!(f, ">="),
            Self::NullishCoalesce => write!(f, "??"),
        }
    }
}

impl Display for TernaryConditional {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{condition} ? {t} : {f}",
            condition = self.condition,
            t = self.true_expression,
            f = self.false_expression
        )
    }
}

impl Display for PropertyName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PropertyName::Literal(identifier) => write!(f, "{identifier}"),
            PropertyName::Expression(expression) => write!(f, "[{expression}]"),
        }
    }
}

impl Display for Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Identifier(identifier) => write!(f, "{identifier}"),
            Self::Object(object) => {
                write!(f, "{{ {} }}", CommaSeparated(object))
            }
            Self::Array(array) => write!(f, "[{}]", CommaSeparated(array)),
        }
    }
}

impl Display for ObjectPatternElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Identifier { identifier, init } => {
                write!(f, "{identifier}")?;
                if let Some(init) = init {
                    write!(f, " = {init}")?;
                }
                Ok(())
            }
            Self::Mapped { key, value, init } => {
                write!(f, "{key}: {value}")?;
                if let Some(init) = init {
                    write!(f, " = {init}")?;
                }
                Ok(())
            }
            Self::Rest { binding, init } => {
                write!(f, "...{binding}")?;
                if let Some(init) = init {
                    write!(f, " = {init}")?;
                }
                Ok(())
            }
        }
    }
}

impl Display for ArrayPatternElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TODO")
    }
}

/// Print a sequence of values, separated by ", "
struct CommaSeparated<'a, T>(&'a [T]);

impl<'a, T: Display> Display for CommaSeparated<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, item) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{item}")?;
        }
        Ok(())
    }
}
