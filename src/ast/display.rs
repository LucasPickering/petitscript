//! Generate source code from AST nodes

use crate::ast::{
    Argument, ArrayElement, ArrayLiteral, ArrayPatternElement,
    ArrayPatternElementKind, BinaryOperation, BinaryOperator, Binding,
    BindingKind, Block, Declaration, ExportDeclaration, ExportDeclarationKind,
    Expression, ExpressionKind, FunctionBody, FunctionCall, FunctionDefinition,
    FunctionParameter, Identifier, IdentifierNode, If, ImportDeclaration,
    ImportModule, ImportModuleKind, ImportNamed, Literal, Module,
    NativeModuleName, ObjectLiteral, ObjectPatternElement, ObjectProperty,
    ObjectPropertyKind, OptionalPropertyAccess, PropertyAccess, PropertyName,
    PropertyNameKind, Statement, StatementKind, TemplateChunk,
    TemplateChunkKind, TemplateLiteral, TernaryConditional, UnaryOperation,
    UnaryOperator, Variable,
};
use std::{borrow::Cow, fmt};

/// Wrapper around [`fmt::Formatter`] that enables incremented indenting
///
/// This doesn't apply any indenting itself. It just allows [DisplayIndent]
/// implementations to track and check the indentation level so they can insert
/// indentation as needed. [`Indent`] does the actual indentation.
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

/// A variant of [`Display`] that accepts an [`Indenter`]
///
/// The indenter allows the implementation to increase indentation (when
/// entering a new block), decrease it (when exiting a block), and insert
/// indentation (on a new line).
///
/// `fmt` takes an owned `self` so that it can be implemented for iterators. For
/// AST types, implement this on `&T`.
trait DisplayIndent {
    /// Format this value into the output, possibly with indentation
    ///
    /// Normally I like full words for variable names, but I cannot spell
    /// `indenter` correctly so I abbreviated it :)
    fn fmt(self, ind: &mut Indenter) -> fmt::Result;
}

impl DisplayIndent for &Module {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        Repeat::new(&self.statements, "\n").with_trailing().fmt(ind)
    }
}

impl DisplayIndent for &Statement {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        match &self.kind {
            StatementKind::Empty => ";".fmt(ind),
            StatementKind::Block(block) => block.fmt(ind),
            StatementKind::Expression(expression) => {
                // If the statement is just an object literal, we need to wrap
                // it in parens so it doesn't look like a block
                if let ExpressionKind::Literal(Literal::Object(_)) =
                    &expression.kind
                {
                    ("(", expression, ");").fmt(ind)
                } else {
                    (expression, ";").fmt(ind)
                }
            }
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

impl DisplayIndent for &Block {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        ("{", Indent::new(&self.statements, ""), "}").fmt(ind)
    }
}

impl DisplayIndent for &Declaration {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        ("const ", Repeat::new(&self.variables, ", "), ";").fmt(ind)
    }
}

impl DisplayIndent for &Variable {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        (&self.binding, self.init.as_ref().map(|init| (" = ", init))).fmt(ind)
    }
}

impl DisplayIndent for &FunctionDefinition {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
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

impl DisplayIndent for &FunctionParameter {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        (
            if self.varargs { Some("...") } else { None },
            &self.variable,
        )
            .fmt(ind)
    }
}

impl DisplayIndent for &FunctionBody {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        match self {
            FunctionBody::Expression(expression) => {
                if let ExpressionKind::Literal(Literal::Object(_)) =
                    &expression.kind
                {
                    // If the body is just an object literal, we need to wrap it
                    // in parens so it doesn't look like a block
                    ("(", expression, ")").fmt(ind)
                } else {
                    expression.fmt(ind)
                }
            }
            FunctionBody::Block(block) => block.fmt(ind),
        }
    }
}

impl DisplayIndent for &If {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        ("if (", &self.condition, ") ", &self.body, &self.else_body).fmt(ind)
    }
}

impl DisplayIndent for &ImportDeclaration {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        // Only include the comma if both parts are present
        let parts = Pair::new(
            self.default.as_ref(),
            if self.named.is_empty() {
                None
            } else {
                Some(("{ ", Repeat::new(&self.named, ", "), " } "))
            },
            ", ",
        );
        ("import ", parts, " from ", &self.module, ";").fmt(ind)
    }
}

impl DisplayIndent for &ImportNamed {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        // If the local ident doesn't match the imported, include a rename
        if self.local.as_str() == self.imported.as_str() {
            self.imported.fmt(ind)
        } else {
            (&self.imported, " as ", &self.local).fmt(ind)
        }
    }
}

impl DisplayIndent for &ImportModule {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        match &self.kind {
            ImportModuleKind::Native(name) => {
                ("'", name.as_str(), "'").fmt(ind)
            }
            ImportModuleKind::Local { name, .. } => ("'", name, "'").fmt(ind),
        }
    }
}

impl DisplayIndent for &ExportDeclaration {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
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

impl DisplayIndent for &Expression {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
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

impl DisplayIndent for &IdentifierNode {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        self.as_str().fmt(ind)
    }
}

impl DisplayIndent for &Identifier {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        self.as_str().fmt(ind)
    }
}

impl DisplayIndent for &Literal {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        match self {
            Literal::Undefined => "undefined".fmt(ind),
            Literal::Null => "null".fmt(ind),
            Literal::Boolean(b) => b.fmt(ind),
            Literal::Float(float) => float.fmt(ind),
            Literal::Int(int) => int.fmt(ind),
            Literal::String(s) => {
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
            Literal::Array(array_literal) => array_literal.fmt(ind),
            Literal::Object(object_literal) => object_literal.fmt(ind),
        }
    }
}

impl DisplayIndent for &TemplateLiteral {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        ("`", Repeat::new(&self.chunks, ""), "`").fmt(ind)
    }
}

impl DisplayIndent for &TemplateChunk {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        match &self.kind {
            // TODO escape backticks in here
            TemplateChunkKind::Literal(literal) => literal.fmt(ind),
            // Escaping isn't needed here, the parser is smart enough to tell
            // when a backtick is part of the expression vs closing the literal
            TemplateChunkKind::Expression(expression) => {
                ("${", expression, "}").fmt(ind)
            }
        }
    }
}

impl DisplayIndent for &ArrayLiteral {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        ("[", Repeat::new(&self.elements, ", "), "]").fmt(ind)
    }
}

impl DisplayIndent for &ArrayElement {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        if self.spread {
            ("...", &self.expression).fmt(ind)
        } else {
            self.expression.fmt(ind)
        }
    }
}

impl DisplayIndent for &ObjectLiteral {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        ("{", Indent::new(&self.properties, ","), "}").fmt(ind)
    }
}

impl DisplayIndent for &ObjectProperty {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
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

impl DisplayIndent for &FunctionCall {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        (&self.function, "(", Repeat::new(&self.arguments, ", "), ")").fmt(ind)
    }
}

impl DisplayIndent for &Argument {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        match self {
            Argument::Expression(expression) => expression.fmt(ind),
            Argument::Spread(expression) => ("...", expression).fmt(ind),
        }
    }
}

impl DisplayIndent for &PropertyAccess {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
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

impl DisplayIndent for &OptionalPropertyAccess {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        (&self.object, "?.", &self.property).fmt(ind)
    }
}

impl DisplayIndent for &UnaryOperation {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        (&self.operator, &self.expression).fmt(ind)
    }
}

impl DisplayIndent for &UnaryOperator {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        match self {
            UnaryOperator::BooleanNot => "!".fmt(ind),
            UnaryOperator::Plus => "+".fmt(ind),
            UnaryOperator::Minus => "-".fmt(ind),
            UnaryOperator::Typeof => "typeof ".fmt(ind),
        }
    }
}

impl DisplayIndent for &BinaryOperation {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        (&self.lhs, " ", &self.operator, " ", &self.rhs).fmt(ind)
    }
}

impl DisplayIndent for &BinaryOperator {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        match self {
            BinaryOperator::Add => "+".fmt(ind),
            BinaryOperator::Sub => "-".fmt(ind),
            BinaryOperator::Mul => "*".fmt(ind),
            BinaryOperator::Div => "/".fmt(ind),
            BinaryOperator::Mod => "%".fmt(ind),
            BinaryOperator::LogicalAnd => "&&".fmt(ind),
            BinaryOperator::LogicalOr => "||".fmt(ind),
            BinaryOperator::Equal => "===".fmt(ind),
            BinaryOperator::NotEqual => "!==".fmt(ind),
            BinaryOperator::LessThan => "<".fmt(ind),
            BinaryOperator::GreaterThan => ">".fmt(ind),
            BinaryOperator::LessThanEqual => "<=".fmt(ind),
            BinaryOperator::GreaterThanEqual => ">=".fmt(ind),
            BinaryOperator::NullishCoalesce => "??".fmt(ind),
        }
    }
}

impl DisplayIndent for &TernaryConditional {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
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

impl DisplayIndent for &PropertyName {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        match &self.kind {
            PropertyNameKind::Identifier(identifier) => identifier.fmt(ind),
            PropertyNameKind::Expression(expression) => {
                ("[", expression, "]").fmt(ind)
            }
        }
    }
}

impl DisplayIndent for &Binding {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
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
            fn fmt(self, ind: &mut Indenter) -> fmt::Result {
                match self {
                    Self::Element(element) => element.fmt(ind),
                    Self::Rest(binding) => ("...", binding).fmt(ind),
                }
            }
        }

        match &self.kind {
            BindingKind::Identifier(identifier) => identifier.fmt(ind),
            BindingKind::Object { properties, rest } => {
                let properties = properties
                    .iter()
                    .map(OrRest::Element)
                    .chain(rest.as_ref().map(OrRest::Rest));
                ("{ ", Repeat::new(properties, ", "), " }").fmt(ind)
            }
            BindingKind::Array { elements, rest } => {
                let elements = elements
                    .iter()
                    .map(OrRest::Element)
                    .chain(rest.as_ref().map(OrRest::Rest));
                ("[", Repeat::new(elements, ", "), "]").fmt(ind)
            }
        }
    }
}

impl DisplayIndent for &ObjectPatternElement {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
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

impl DisplayIndent for &ArrayPatternElement {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
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

/// Delimited pair of options
///
/// If either is missing, the comma won't be included.
struct Pair<'a, T, U> {
    left: Option<T>,
    right: Option<U>,
    delimiter: &'a str,
}

impl<'a, T, U> Pair<'a, T, U> {
    fn new(left: Option<T>, right: Option<U>, delimiter: &'a str) -> Self {
        Self {
            left,
            right,
            delimiter,
        }
    }
}

impl<T: DisplayIndent, U: DisplayIndent> DisplayIndent for Pair<'_, T, U> {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        match (self.left, self.right) {
            (None, None) => "".fmt(ind),
            (Some(left), None) => left.fmt(ind),
            (None, Some(right)) => right.fmt(ind),
            (Some(left), Some(right)) => (left, self.delimiter, right).fmt(ind),
        }
    }
}

/// Print a sequence of values separated by a delimiter
///
/// `I` is typically `&[T]`.
///
/// By default, the delimiter is *not* emitted after the last element. Call
/// [`Repeat::with_trailing`] to emit it.
struct Repeat<'a, I> {
    items: I,
    delimiter: &'a str,
    /// Emit the delimiter after the final item?
    include_trailing: bool,
}

impl<'a, I> Repeat<'a, I> {
    fn new(items: I, delimiter: &'a str) -> Self {
        Self {
            items,
            delimiter,
            include_trailing: false,
        }
    }

    fn with_trailing(mut self) -> Self {
        self.include_trailing = true;
        self
    }
}

impl<I> DisplayIndent for Repeat<'_, I>
where
    I: IntoIterator,
    I::Item: DisplayIndent,
{
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        let mut iter = self.items.into_iter().peekable();
        // for loop doesn't work with peekable()
        while let Some(item) = iter.next() {
            item.fmt(ind)?;
            if self.include_trailing || iter.peek().is_some() {
                self.delimiter.fmt(ind)?;
            }
        }
        Ok(())
    }
}

/// Print a newline and subsequent indentation
struct Newline;

impl DisplayIndent for &Newline {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
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
///
/// `I` is typically `&[T]`.
struct Indent<'a, I> {
    items: I,
    delimiter: &'a str,
}

impl<'a, I> Indent<'a, I> {
    fn new(items: I, delimiter: &'a str) -> Self {
        Self { items, delimiter }
    }
}

impl<I> DisplayIndent for Indent<'_, I>
where
    I: IntoIterator,
    I::Item: DisplayIndent,
{
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        let mut empty = true;
        ind.indent_level += 1;
        for item in self.items {
            empty = false;
            Newline.fmt(ind)?;
            item.fmt(ind)?;
            write!(ind.formatter, "{}", self.delimiter)?;
        }
        ind.indent_level -= 1;
        // Add a final newline after the last element. This should be back at
        // the original indentation. Prevent newlines in empty blocks.
        if !empty {
            Newline.fmt(ind)?;
        }
        Ok(())
    }
}

impl<T> DisplayIndent for &Box<T>
where
    for<'a> &'a T: DisplayIndent,
{
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        (**self).fmt(ind)
    }
}

impl DisplayIndent for bool {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        write!(ind.formatter, "{self}")
    }
}

impl DisplayIndent for i64 {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        write!(ind.formatter, "{self}")
    }
}

impl DisplayIndent for f64 {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        write!(ind.formatter, "{self}")
    }
}

impl DisplayIndent for &str {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        write!(ind.formatter, "{self}")
    }
}

impl DisplayIndent for &String {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        write!(ind.formatter, "{self}")
    }
}

impl DisplayIndent for &Cow<'_, str> {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        write!(ind.formatter, "{self}")
    }
}

impl<T> DisplayIndent for &Option<T>
where
    for<'a> &'a T: DisplayIndent,
{
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
        self.as_ref().fmt(ind)
    }
}

impl<T: DisplayIndent> DisplayIndent for Option<T> {
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
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
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
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
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
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
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
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
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
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
    fn fmt(self, ind: &mut Indenter) -> fmt::Result {
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
    use crate::ast::{IntoExpression, IntoStatement};
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
}";
        "literal object"
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
}";
        "block"
    )]
    #[test_case(
        // Requires parens around the object
        ObjectLiteral::empty().into_expr().into_stmt(),
        "({});"; "statement object"
    )]
    #[test_case(
        // Requires parens around the object
        FunctionDefinition::expression([], ObjectLiteral::empty()),
        "() => ({})"; "function expression object"
    )]
    // I'll add more test cases as I go. It's definitely not exhaustive.
    fn display<T: Display>(node: T, expected: &str) {
        assert_eq!(node.to_string(), expected);
    }
}
