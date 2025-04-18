//! Generate source code from AST nodes

use crate::ast::{
    source::Spanned, ArrayElement, ArrayLiteral, ArrayPatternElement,
    BinaryOperation, BinaryOperator, Binding, Block, Declaration, DoWhileLoop,
    ExportDeclaration, Expression, ForOfLoop, FunctionBody, FunctionCall,
    FunctionDeclaration, FunctionDefinition, FunctionParameter,
    FunctionPointer, Identifier, If, ImportDeclaration, ImportModule,
    ImportNamed, LexicalDeclaration, Literal, Module, NativeModuleName,
    ObjectLiteral, ObjectPatternElement, ObjectProperty,
    OptionalPropertyAccess, PropertyAccess, PropertyName, Statement,
    TemplateChunk, TemplateLiteral, TernaryConditional, UnaryOperation,
    UnaryOperator, Variable, WhileLoop,
};
use std::fmt;

// TODO fix extra newline in lambda bodies

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut indenter = Indenter::new(f, Some("  "));
        DisplayIndent::fmt(self, &mut indenter)
    }
}

/// TODO
struct Indenter<'a, 'f> {
    formatter: &'a mut fmt::Formatter<'f>,
    /// How many levels deep we currently are in an indentation tree
    indent_level: usize,
    /// Indentation string, e.g. `"  "`. `None` for no indentation
    indentation: Option<&'a str>,
}

impl<'a, 'f> Indenter<'a, 'f> {
    fn new(
        formatter: &'a mut fmt::Formatter<'f>,
        indentation: Option<&'a str>,
    ) -> Self {
        Self {
            formatter,
            indent_level: 0,
            indentation,
        }
    }

    fn indentation_enabled(&self) -> bool {
        self.indentation.is_some()
    }

    /// Write the current indentation
    fn indent(&mut self) -> fmt::Result {
        if let Some(indentation) = self.indentation {
            for _ in 0..self.indent_level {
                write!(self.formatter, "{indentation}")?;
            }
        }
        Ok(())
    }
}

trait DisplayIndent {
    /// TODO
    ///
    /// Normally I like full words for variable names, but I cannot spell
    /// `indenter` correctly so I abbreviated it :)
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result;
}

impl DisplayIndent for Module {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        Repeat::new(&self.statements, "").fmt(ind)
    }
}

impl DisplayIndent for Statement {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match self {
            Self::Empty => ";".fmt(ind),
            Self::Block(block) => block.fmt(ind),
            Self::Expression(expression) => (expression, ";").fmt(ind),
            Self::Declaration(declaration) => declaration.fmt(ind),
            Self::If(if_) => if_.fmt(ind),
            Self::ForOfLoop(for_of_loop) => for_of_loop.fmt(ind),
            Self::WhileLoop(while_loop) => while_loop.fmt(ind),
            Self::DoWhileLoop(do_while_loop) => do_while_loop.fmt(ind),
            Self::Return(expression) => {
                if let Some(expression) = expression {
                    ("return ", expression, ";").fmt(ind)
                } else {
                    "return;".fmt(ind)
                }
            }
            Self::Break => "break;".fmt(ind),
            Self::Continue => "continue;".fmt(ind),
            Self::Import(import) => import.fmt(ind),
            Self::Export(export) => export.fmt(ind),
        }?;
        // Statements include their own trailing newline
        Newline.fmt(ind)
    }
}

impl DisplayIndent for Block {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        ("{", Newline, Indent::new(&self.statements, ""), "}").fmt(ind)
    }
}

impl DisplayIndent for Declaration {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match self {
            Declaration::Lexical(lexical) => lexical.fmt(ind),
            Declaration::Function(function) => function.fmt(ind),
        }
    }
}

impl DisplayIndent for LexicalDeclaration {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        ("const ", Repeat::new(&self.variables, ", "), ";").fmt(ind)
    }
}

impl DisplayIndent for Variable {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        (&self.binding, self.init.as_ref().map(|init| (" = ", init))).fmt(ind)
    }
}

impl DisplayIndent for FunctionDeclaration {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        ("const ", &self.name, " = ", &self.pointer, ";").fmt(ind)
    }
}

impl DisplayIndent for FunctionPointer {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match self {
            // Source code generation is meant to be run on hand-crafted ASTs,
            // so the function def should still be inline
            Self::Inline(definition) => definition.fmt(ind),
            // This isn't really useful, but we can't do any better without the
            // function table
            Self::Lifted(_) => "<lifted function>".fmt(ind),
        }
    }
}

impl DisplayIndent for FunctionDefinition {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        // Print as arrow syntax because it's easier
        // TODO use function f(){} syntax? we would need to divide the AST

        (
            "(",
            Repeat::new(&self.parameters, ", "),
            ") => ",
            &self.body,
        )
            .fmt(ind)
    }
}

impl DisplayIndent for FunctionParameter {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        (
            if self.varargs { Some("...") } else { None },
            &self.variable,
        )
            .fmt(ind)
    }
}

impl DisplayIndent for FunctionBody {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match self {
            Self::Expression(expression) => expression.fmt(ind),
            Self::Block(block) => block.fmt(ind),
        }
    }
}

impl DisplayIndent for If {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        ("if (", &self.condition, ") ", &self.body, &self.else_body).fmt(ind)
    }
}

impl DisplayIndent for ForOfLoop {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        (
            "for (const ",
            &self.binding,
            " of ",
            &self.iterable,
            ") ",
            &self.body,
        )
            .fmt(ind)
    }
}

impl DisplayIndent for WhileLoop {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        ("while (", &self.condition, ") ", &self.body).fmt(ind)
    }
}

impl DisplayIndent for DoWhileLoop {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        ("do ", &self.body, "while (", &self.condition, ")").fmt(ind)
    }
}

impl DisplayIndent for ImportDeclaration {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        (
            "import ",
            self.default.as_ref().map(|default| (default, ", ")),
            if self.named.is_empty() {
                None
            } else {
                Some(("{ ", Repeat::new(&self.named, ", "), " } "))
            },
            " from ",
            &self.module,
            ";",
        )
            .fmt(ind)
    }
}

impl DisplayIndent for ImportNamed {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        (
            &self.identifier,
            self.rename.as_ref().map(|rename| ("as ", rename)),
        )
            .fmt(ind)
    }
}

impl DisplayIndent for ImportModule {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match self {
            ImportModule::Native(name) => ("'", &name.0, "'").fmt(ind),
            ImportModule::Local(path) => ("'", path, "'").fmt(ind),
        }
    }
}

impl DisplayIndent for ExportDeclaration {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match self {
            Self::Reexport {} => todo!(),
            Self::Declaration(declaration) => ("export ", declaration).fmt(ind),
            Self::DefaultFunctionDeclaration(_) => {
                todo!()
            }
            Self::DefaultExpression(expression) => {
                ("export default ", expression, ";").fmt(ind)
            }
        }
    }
}

impl DisplayIndent for Expression {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match self {
            Self::Parenthesized(expression) => ("(", expression, ")").fmt(ind),
            Self::Literal(literal) => literal.fmt(ind),
            Self::Template(template) => template.fmt(ind),
            Self::Identifier(identifier) => identifier.fmt(ind),
            Self::Call(call) => call.fmt(ind),
            Self::Property(property) => property.fmt(ind),
            Self::OptionalProperty(property) => property.fmt(ind),
            Self::ArrowFunction(function) => function.fmt(ind),
            Self::Unary(operation) => operation.fmt(ind),
            Self::Binary(operation) => operation.fmt(ind),
            Self::Ternary(operation) => operation.fmt(ind),
        }
    }
}

impl DisplayIndent for Identifier {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        self.0.fmt(ind)
    }
}

impl DisplayIndent for Literal {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match self {
            Self::Null => "null".fmt(ind),
            Self::Undefined => "undefined".fmt(ind),
            Self::Boolean(b) => b.fmt(ind),
            Self::Float(float) => float.fmt(ind),
            Self::Int(int) => int.fmt(ind),
            // TODO escape correctly here
            Self::String(s) => ("\"", s, "\"").fmt(ind),
            Self::Array(array_literal) => array_literal.fmt(ind),
            Self::Object(object_literal) => object_literal.fmt(ind),
        }
    }
}

impl DisplayIndent for TemplateLiteral {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        ("`", Repeat::new(&self.chunks, ""), "`").fmt(ind)
    }
}

impl DisplayIndent for TemplateChunk {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match self {
            // TODO do we need any escaping here?
            Self::Literal(literal) => literal.fmt(ind),
            // TODO escaping here?
            Self::Expression(expression) => ("${", expression, "}").fmt(ind),
        }
    }
}

impl DisplayIndent for ArrayLiteral {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        ("[", Repeat::new(&self.elements, ", "), "]").fmt(ind)
    }
}

impl DisplayIndent for ArrayElement {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match self {
            Self::Expression(expression) => expression.fmt(ind),
            Self::Spread(expression) => ("...", expression).fmt(ind),
        }
    }
}

impl DisplayIndent for ObjectLiteral {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        ("{", Indent::new(&self.properties, ", "), "}").fmt(ind)
    }
}

impl DisplayIndent for ObjectProperty {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match self {
            Self::Property {
                property,
                expression,
            } => (property, ": ", expression).fmt(ind),
            Self::Identifier(identifier) => identifier.fmt(ind),
            Self::Spread(expression) => ("...", expression).fmt(ind),
        }
    }
}

impl DisplayIndent for FunctionCall {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        (&self.function, "(", Repeat::new(&self.arguments, ", "), ")").fmt(ind)
    }
}

impl DisplayIndent for PropertyAccess {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match &self.property.data {
            PropertyName::Literal(literal) => {
                (&self.expression, ".", literal).fmt(ind)
            }
            PropertyName::Expression(expression) => {
                (&self.expression, "[", expression, "]").fmt(ind)
            }
        }
    }
}

impl DisplayIndent for OptionalPropertyAccess {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        (&self.expression, "?.", &self.property).fmt(ind)
    }
}

impl DisplayIndent for UnaryOperation {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        (&self.operator, &self.expression).fmt(ind)
    }
}

impl DisplayIndent for UnaryOperator {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match self {
            Self::BooleanNot => "!".fmt(ind),
            Self::Plus => "+".fmt(ind),
            Self::Minus => "-".fmt(ind),
            Self::Typeof => "typeof ".fmt(ind),
        }
    }
}

impl DisplayIndent for BinaryOperation {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        (&self.lhs, " ", &self.operator, " ", &self.rhs).fmt(ind)
    }
}

impl DisplayIndent for BinaryOperator {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match self {
            Self::Add => "+".fmt(ind),
            Self::Sub => "-".fmt(ind),
            Self::Mul => "*".fmt(ind),
            Self::Div => "/".fmt(ind),
            Self::Mod => "%".fmt(ind),
            Self::BooleanAnd => "&&".fmt(ind),
            Self::BooleanOr => "||".fmt(ind),
            Self::Equal => "===".fmt(ind),
            Self::NotEqual => "!==".fmt(ind),
            Self::LessThan => "<".fmt(ind),
            Self::GreaterThan => ">".fmt(ind),
            Self::LessThanEqual => "<=".fmt(ind),
            Self::GreaterThanEqual => ">=".fmt(ind),
            Self::NullishCoalesce => "??".fmt(ind),
        }
    }
}

impl DisplayIndent for TernaryConditional {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        (
            &self.condition,
            " ? ",
            &self.true_expression,
            " : ",
            &self.false_expression,
        )
            .fmt(ind)
    }
}

impl DisplayIndent for PropertyName {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match self {
            PropertyName::Literal(identifier) => identifier.fmt(ind),
            PropertyName::Expression(expression) => {
                ("[", expression, "]").fmt(ind)
            }
        }
    }
}

impl DisplayIndent for Binding {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match self {
            Self::Identifier(identifier) => identifier.fmt(ind),
            Self::Object(object) => {
                ("{", Repeat::new(object, ", "), "}").fmt(ind)
            }
            Self::Array(array) => ("[", Repeat::new(array, ", "), "]").fmt(ind),
        }
    }
}

impl DisplayIndent for ObjectPatternElement {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match self {
            Self::Identifier { identifier, init } => {
                (identifier, init.as_ref().map(|init| (" = ", init))).fmt(ind)
            }
            Self::Mapped { key, value, init } => {
                (key, ": ", value, init.as_ref().map(|init| (" = ", init)))
                    .fmt(ind)
            }
            Self::Rest { binding, init } => {
                ("...", binding, init.as_ref().map(|init| (" = ", init)))
                    .fmt(ind)
            }
        }
    }
}

impl DisplayIndent for ArrayPatternElement {
    fn fmt(&self, _: &mut Indenter) -> fmt::Result {
        todo!()
    }
}

/// Print a sequence of values separated by a delimiter
struct Repeat<'a, T> {
    items: &'a [T],
    delimiter: &'a str,
}

impl<'a, T> Repeat<'a, T> {
    fn new(items: &'a [T], delimiter: &'a str) -> Self {
        Self { items, delimiter }
    }
}

impl<'a, T: DisplayIndent> DisplayIndent for Repeat<'a, T> {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        for (i, item) in self.items.iter().enumerate() {
            if i > 0 {
                self.delimiter.fmt(ind)?;
            }
            item.fmt(ind)?;
        }
        Ok(())
    }
}

/// Print a newline and subsequent indentation
struct Newline;

impl DisplayIndent for Newline {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        // Every new line should start with indentation
        writeln!(ind.formatter)?;
        ind.indent()
    }
}

/// Print a sequence of values separated by a delimiter. If indentation is
/// enabled, print a newline and indentation before each item. A newline
/// will be printed before the first item and after the last. The delimiter will
/// be printed after each item *except the last*.
struct Indent<'a, T> {
    items: &'a [T],
    delimiter: &'a str,
}

impl<'a, T> Indent<'a, T> {
    fn new(items: &'a [T], delimiter: &'a str) -> Self {
        Self { items, delimiter }
    }
}

impl<'a, T: DisplayIndent> DisplayIndent for Indent<'a, T> {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        // Shortcut to prevent newlines in empty collections/blocks
        if self.items.is_empty() {
            return Ok(());
        }

        ind.indent_level += 1;
        for (i, item) in self.items.iter().enumerate() {
            if ind.indentation_enabled() {
                Newline.fmt(ind)?;
            }
            item.fmt(ind)?;
            if i < self.items.len() - 1 {
                write!(ind.formatter, "{}", self.delimiter)?;
            }
        }
        ind.indent_level -= 1;
        // Add a final newline after the last element. This should be back at
        // the original indentation
        if ind.indentation_enabled() {
            Newline.fmt(ind)?;
        }
        Ok(())
    }
}

impl<T: DisplayIndent> DisplayIndent for &T {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        (*self).fmt(ind)
    }
}

impl<T: DisplayIndent> DisplayIndent for Box<T> {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        (**self).fmt(ind)
    }
}

impl<T: DisplayIndent> DisplayIndent for Spanned<T> {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        self.data.fmt(ind)
    }
}

impl DisplayIndent for bool {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        write!(ind.formatter, "{self}")
    }
}

impl DisplayIndent for i64 {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        write!(ind.formatter, "{self}")
    }
}

impl DisplayIndent for f64 {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        write!(ind.formatter, "{self}")
    }
}

impl DisplayIndent for &str {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        write!(ind.formatter, "{self}")
    }
}

impl DisplayIndent for String {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        write!(ind.formatter, "{self}")
    }
}

impl<T: DisplayIndent> DisplayIndent for Option<T> {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match self {
            Some(value) => value.fmt(ind),
            None => Ok(()),
        }
    }
}

// I was struggling to write the macro to do this so here we go!
impl<T0, T1> DisplayIndent for (T0, T1)
where
    T0: DisplayIndent,
    T1: DisplayIndent,
{
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        self.0.fmt(ind)?;
        self.1.fmt(ind)?;
        Ok(())
    }
}

impl<T0, T1, T2> DisplayIndent for (T0, T1, T2)
where
    T0: DisplayIndent,
    T1: DisplayIndent,
    T2: DisplayIndent,
{
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        self.0.fmt(ind)?;
        self.1.fmt(ind)?;
        self.2.fmt(ind)?;
        Ok(())
    }
}

impl<T0, T1, T2, T3> DisplayIndent for (T0, T1, T2, T3)
where
    T0: DisplayIndent,
    T1: DisplayIndent,
    T2: DisplayIndent,
    T3: DisplayIndent,
{
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        self.0.fmt(ind)?;
        self.1.fmt(ind)?;
        self.2.fmt(ind)?;
        self.3.fmt(ind)?;
        Ok(())
    }
}

impl<T0, T1, T2, T3, T4> DisplayIndent for (T0, T1, T2, T3, T4)
where
    T0: DisplayIndent,
    T1: DisplayIndent,
    T2: DisplayIndent,
    T3: DisplayIndent,
    T4: DisplayIndent,
{
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        self.0.fmt(ind)?;
        self.1.fmt(ind)?;
        self.2.fmt(ind)?;
        self.3.fmt(ind)?;
        self.4.fmt(ind)?;
        Ok(())
    }
}

impl<T0, T1, T2, T3, T4, T5> DisplayIndent for (T0, T1, T2, T3, T4, T5)
where
    T0: DisplayIndent,
    T1: DisplayIndent,
    T2: DisplayIndent,
    T3: DisplayIndent,
    T4: DisplayIndent,
    T5: DisplayIndent,
{
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        self.0.fmt(ind)?;
        self.1.fmt(ind)?;
        self.2.fmt(ind)?;
        self.3.fmt(ind)?;
        self.4.fmt(ind)?;
        self.5.fmt(ind)?;
        Ok(())
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for NativeModuleName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
