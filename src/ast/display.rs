//! Generate source code from AST nodes

use crate::ast::{
    Argument, ArrayElement, ArrayElementKind, ArrayLiteral,
    ArrayPatternElement, ArrayPatternElementKind, BinaryOperation,
    BinaryOperator, Binding, BindingKind, Block, Declaration,
    ExportDeclaration, ExportDeclarationKind, Expression, ExpressionKind,
    FunctionBody, FunctionCall, FunctionDefinition, FunctionParameter,
    Identifier, IdentifierNode, If, ImportDeclaration, ImportModule,
    ImportModuleKind, ImportNamed, Literal, Module, NativeModuleName,
    ObjectLiteral, ObjectPatternElement, ObjectProperty, ObjectPropertyKind,
    OptionalPropertyAccess, PropertyAccess, PropertyName, PropertyNameKind,
    Statement, StatementKind, TemplateChunk, TemplateChunkKind,
    TemplateLiteral, TernaryConditional, UnaryOperation, UnaryOperator,
    Variable,
};
use std::{borrow::Cow, fmt};

/// TODO
struct Indenter<'a, 'f> {
    formatter: &'a mut fmt::Formatter<'f>,
    /// How many levels deep we currently are in an indentation tree
    indent_level: usize,
    /// Indentation string, e.g. `"  "`
    indentation: &'a str,
}

impl<'a, 'f> Indenter<'a, 'f> {
    fn new(
        formatter: &'a mut fmt::Formatter<'f>,
        indentation: &'a str,
    ) -> Self {
        Self {
            formatter,
            indent_level: 0,
            indentation,
        }
    }

    /// Write the current indentation
    fn indent(&mut self) -> fmt::Result {
        for _ in 0..self.indent_level {
            write!(self.formatter, "{}", self.indentation)?;
        }
        Ok(())
    }
}

/// TODO
trait DisplayIndent {
    /// TODO
    ///
    /// Normally I like full words for variable names, but I cannot spell
    /// `indenter` correctly so I abbreviated it :)
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result;
}

impl DisplayIndent for Module {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        Repeat::new(&self.statements, "\n")
            .with_last_delimiter()
            .fmt(ind)
    }
}

impl DisplayIndent for Statement {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match &self.kind {
            StatementKind::Empty => ";".fmt(ind),
            StatementKind::Block(block) => block.fmt(ind),
            StatementKind::Expression(expression) => (expression, ";").fmt(ind),
            StatementKind::Declaration(declaration) => declaration.fmt(ind),
            StatementKind::If(if_) => if_.fmt(ind),
            StatementKind::Return(expression) => {
                if let Some(expression) = expression {
                    ("return ", expression, ";").fmt(ind)
                } else {
                    "return;".fmt(ind)
                }
            }
            StatementKind::Import(import) => import.fmt(ind),
            StatementKind::Export(export) => export.fmt(ind),
        }
    }
}

impl DisplayIndent for Block {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        ("{", Indent::new(&self.statements, ""), "}").fmt(ind)
    }
}

impl DisplayIndent for Declaration {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        ("const ", Repeat::new(&self.variables, ", "), ";").fmt(ind)
    }
}

impl DisplayIndent for Variable {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        (&self.binding, self.init.as_ref().map(|init| (" = ", init))).fmt(ind)
    }
}

impl DisplayIndent for FunctionDefinition {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        // Print as arrow syntax because it's easier
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
            Self::Expression(expression) => {
                if let ExpressionKind::Literal(Literal::Object(
                    object_literal,
                )) = &expression.kind
                {
                    // If the body is just an object literal, we need to wrap it
                    // in parens so it doesn't look like a block
                    ("(", object_literal, ")").fmt(ind)
                } else {
                    expression.fmt(ind)
                }
            }
            Self::Block(block) => block.fmt(ind),
        }
    }
}

impl DisplayIndent for If {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        ("if (", &self.condition, ") ", &self.body, &self.else_body).fmt(ind)
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
        // If the local ident doesn't match the imported, include a rename
        if self.local.as_str() == self.imported.as_str() {
            self.imported.fmt(ind)
        } else {
            (&self.imported, " as ", &self.local).fmt(ind)
        }
    }
}

impl DisplayIndent for ImportModule {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match &self.kind {
            ImportModuleKind::Native(name) => {
                ("'", name.as_str(), "'").fmt(ind)
            }
            ImportModuleKind::Local(path) => ("'", path, "'").fmt(ind),
        }
    }
}

impl DisplayIndent for ExportDeclaration {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match &self.kind {
            ExportDeclarationKind::Declaration(declaration) => {
                ("export ", declaration).fmt(ind)
            }
            ExportDeclarationKind::DefaultExpression(expression) => {
                ("export default ", expression, ";").fmt(ind)
            }
        }
    }
}

impl DisplayIndent for Expression {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match &self.kind {
            ExpressionKind::Parenthesized(expression) => {
                ("(", expression, ")").fmt(ind)
            }
            ExpressionKind::Literal(literal) => literal.fmt(ind),
            ExpressionKind::Template(template) => template.fmt(ind),
            ExpressionKind::Identifier(identifier) => identifier.fmt(ind),
            ExpressionKind::Call(call) => call.fmt(ind),
            ExpressionKind::Property(property) => property.fmt(ind),
            ExpressionKind::OptionalProperty(property) => property.fmt(ind),
            ExpressionKind::Function(function) => function.fmt(ind),
            ExpressionKind::Unary(operation) => operation.fmt(ind),
            ExpressionKind::Binary(operation) => operation.fmt(ind),
            ExpressionKind::Ternary(operation) => operation.fmt(ind),
        }
    }
}

impl DisplayIndent for IdentifierNode {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        self.as_str().fmt(ind)
    }
}

impl DisplayIndent for Identifier {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        self.as_str().fmt(ind)
    }
}

impl DisplayIndent for Literal {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match self {
            Self::Undefined => "undefined".fmt(ind),
            Self::Null => "null".fmt(ind),
            Self::Boolean(b) => b.fmt(ind),
            Self::Float(float) => float.fmt(ind),
            Self::Int(int) => int.fmt(ind),
            Self::String(s) => {
                // If any invalid chars are in the string, use a template
                // literal because they're more forgiving
                // TODO escape with \ instead. Requires fixes to the parser
                if s.contains('"') || s.contains('\n') {
                    // TODO escape backticks
                    ("`", s, "`").fmt(ind)
                } else {
                    ("\"", s, "\"").fmt(ind)
                }
            }
            Self::Array(array_literal) => array_literal.fmt(ind),
            Self::Object(object_literal) => object_literal.fmt(ind),
        }
    }
}

impl DisplayIndent for TemplateLiteral {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        // TODO escape backticks
        ("`", Repeat::new(&self.chunks, ""), "`").fmt(ind)
    }
}

impl DisplayIndent for TemplateChunk {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match &self.kind {
            // TODO do we need any escaping here?
            TemplateChunkKind::Literal(literal) => literal.fmt(ind),
            // TODO escaping here?
            TemplateChunkKind::Expression(expression) => {
                ("${", expression, "}").fmt(ind)
            }
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
        match &self.kind {
            ArrayElementKind::Expression(expression) => expression.fmt(ind),
            ArrayElementKind::Spread(expression) => {
                ("...", expression).fmt(ind)
            }
        }
    }
}

impl DisplayIndent for ObjectLiteral {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        ("{", Indent::new(&self.properties, ","), "}").fmt(ind)
    }
}

impl DisplayIndent for ObjectProperty {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match &self.kind {
            ObjectPropertyKind::Property {
                property,
                expression,
            } => (property, ": ", expression).fmt(ind),
            ObjectPropertyKind::Spread(expression) => {
                ("...", expression).fmt(ind)
            }
        }
    }
}

impl DisplayIndent for FunctionCall {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        (&self.function, "(", Repeat::new(&self.arguments, ", "), ")").fmt(ind)
    }
}

impl DisplayIndent for Argument {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match self {
            Self::Expression(expression) => expression.fmt(ind),
            Self::Spread(expression) => ("...", expression).fmt(ind),
        }
    }
}

impl DisplayIndent for PropertyAccess {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match &self.property.kind {
            PropertyNameKind::Identifier(literal) => {
                (&self.object, ".", literal).fmt(ind)
            }
            PropertyNameKind::Expression(expression) => {
                (&self.object, "[", expression, "]").fmt(ind)
            }
        }
    }
}

impl DisplayIndent for OptionalPropertyAccess {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        (&self.object, "?.", &self.property).fmt(ind)
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
            Self::LogicalAnd => "&&".fmt(ind),
            Self::LogicalOr => "||".fmt(ind),
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
        match &self.kind {
            PropertyNameKind::Identifier(identifier) => identifier.fmt(ind),
            PropertyNameKind::Expression(expression) => {
                ("[", expression, "]").fmt(ind)
            }
        }
    }
}

impl DisplayIndent for Binding {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        /// Helper to format an element OR a `...rest` pattern
        enum OrRest<E, R> {
            Element(E),
            Rest(R),
        }

        impl<E, R> DisplayIndent for OrRest<E, R>
        where
            E: DisplayIndent,
            R: DisplayIndent,
        {
            fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
                match self {
                    Self::Element(element) => element.fmt(ind),
                    Self::Rest(binding) => ("...", binding).fmt(ind),
                }
            }
        }

        match &self.kind {
            BindingKind::Identifier(identifier) => identifier.fmt(ind),
            BindingKind::Object { properties, rest } => {
                // Attach the ...rest to the end of the chain. The Vec
                // allocation is a little lame, but the alternative is to have
                // Repeat accept an iterator. That opens a giant can of worms
                // with the type system, not worth it.
                let properties = &properties
                    .iter()
                    .map(OrRest::Element)
                    .chain(rest.as_ref().map(OrRest::Rest))
                    .collect::<Vec<_>>();
                ("{ ", Repeat::new(properties, ", "), " }").fmt(ind)
            }
            BindingKind::Array { elements, rest } => {
                let elements = &elements
                    .iter()
                    .map(OrRest::Element)
                    .chain(rest.as_ref().map(OrRest::Rest))
                    .collect::<Vec<_>>();
                ("[", Repeat::new(elements, ", "), "]").fmt(ind)
            }
        }
    }
}

impl DisplayIndent for ObjectPatternElement {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        let init = self.init.as_ref().map(|init| (" = ", init));
        match (&self.key.kind, &self.binding.kind) {
            // Use shorthand where available
            (
                PropertyNameKind::Identifier(key),
                BindingKind::Identifier(value),
            ) if key.identifier == value.identifier => (key, init).fmt(ind),
            _ => (&self.key, ": ", &self.binding, init).fmt(ind),
        }
    }
}

impl DisplayIndent for ArrayPatternElement {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        match &self.kind {
            ArrayPatternElementKind::Identifier { binding, init } => {
                let init = init.as_ref().map(|init| (" = ", init));
                (binding, init).fmt(ind)
            }
            // The joiner in the parent will include the , automatically
            ArrayPatternElementKind::Skip => "".fmt(ind),
        }
    }
}

/// Print a sequence of values separated by a delimiter
///
/// By default, the delimiter is *not* emitted after the last element. Call
/// [`Repeat::with_last_delimiter`] to emit it.
struct Repeat<'a, T> {
    items: &'a [T],
    delimiter: &'a str,
    /// Emit the delimiter after the final item?
    include_last_delimiter: bool,
}

impl<'a, T> Repeat<'a, T> {
    fn new(items: &'a [T], delimiter: &'a str) -> Self {
        Self {
            items,
            delimiter,
            include_last_delimiter: false,
        }
    }

    fn with_last_delimiter(mut self) -> Self {
        self.include_last_delimiter = true;
        self
    }
}

impl<T: DisplayIndent> DisplayIndent for Repeat<'_, T> {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        for (i, item) in self.items.iter().enumerate() {
            item.fmt(ind)?;
            if self.include_last_delimiter || i < self.items.len() - 1 {
                self.delimiter.fmt(ind)?;
            }
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

/// Print a sequence of values separated by a delimiter
///
/// Print a newline and indentation before each item. A newline will be printed
/// before the first item and after the last. The delimiter will be printed
/// after each item *including the last*. This includes the final delimiter
/// because the trailing comma is nice on object literals.
struct Indent<'a, T> {
    items: &'a [T],
    delimiter: &'a str,
}

impl<'a, T> Indent<'a, T> {
    fn new(items: &'a [T], delimiter: &'a str) -> Self {
        Self { items, delimiter }
    }
}

impl<T: DisplayIndent> DisplayIndent for Indent<'_, T> {
    fn fmt(&self, ind: &mut Indenter) -> fmt::Result {
        // Shortcut to prevent newlines in empty collections/blocks
        if self.items.is_empty() {
            return Ok(());
        }

        ind.indent_level += 1;
        for item in self.items {
            Newline.fmt(ind)?;
            item.fmt(ind)?;
            write!(ind.formatter, "{}", self.delimiter)?;
        }
        ind.indent_level -= 1;
        // Add a final newline after the last element. This should be back at
        // the original indentation
        Newline.fmt(ind)?;
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

impl DisplayIndent for Cow<'_, str> {
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
        write!(f, "{}", self.as_str())
    }
}

impl fmt::Display for NativeModuleName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Implement [`Display`](std::fmt::Display) on all types that implement
/// [`DisplayIndent`]
///
/// It would be great to do this with a blanket impl, but that's not possible
/// because `Display` is a foreign trait
macro_rules! impl_display {
    ($($type:ty)*) => {
        $(
            impl std::fmt::Display for $type {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    let mut indenter = Indenter::new(f, "  ");
                    DisplayIndent::fmt(self, &mut indenter)
                }
            }
        )*
    }
}

impl_display! {
    Module Statement Block Declaration Variable FunctionDefinition FunctionBody
    FunctionParameter If ImportDeclaration ImportModule ExportDeclaration
    Expression Literal TemplateLiteral TemplateChunk ArrayLiteral ObjectLiteral
    ObjectProperty FunctionCall Argument PropertyAccess OptionalPropertyAccess
    UnaryOperation UnaryOperator BinaryOperation BinaryOperator
    TernaryConditional PropertyName Binding ObjectPatternElement
    ArrayPatternElement
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::IntoExpression;
    use pretty_assertions::assert_eq;
    use std::fmt::Display;
    use test_case::test_case;

    /// Test stringification of AST nodes
    #[test_case(Literal::Undefined, "undefined"; "literal undefined")]
    #[test_case(Literal::Null, "null"; "literal null")]
    #[test_case(Literal::from(false), "false"; "literal false")]
    #[test_case(Literal::from(true), "true"; "literal true")]
    #[test_case(Literal::from(-11), "-11"; "literal int")]
    #[test_case(Literal::from(-3.24), "-3.24"; "literal float")]
    #[test_case(Literal::from("asdf"), "\"asdf\""; "literal string")]
    #[test_case(
        ObjectLiteral::new([
            ("x", 3.into_expr()),
            ("y", ObjectLiteral::new([("z", 5)]).into_expr()),
        ]),
        "{
  x: 3,
  y: {
    z: 5,
  },
}"; "literal object"
    )]
    #[test_case(
        Binding::object(
            [("x", "x", None), ("y", "y1", Some(1))],
            Some("z"),
        ),
        "{ x, y: y1 = 1, ...z }"; "binding object"
    )]
    #[test_case(
        Binding::array(
            [Some(("x", None)), None, Some(("y", Some(1)))],
            Some("z"),
        ),
        "[x, , y = 1, ...z]"; "binding array"
    )]
    #[test_case(
        Block::new([Declaration::new("x", 3)]),
        "{
  const x = 3;
}"; "block"
    )]
    // I'll add more test cases as I go. It's definitely not exhaustive.
    fn display<T: Display>(node: T, expected: &str) {
        assert_eq!(node.to_string(), expected);
    }

    // TODO prop test for display -> parse round trip
}
