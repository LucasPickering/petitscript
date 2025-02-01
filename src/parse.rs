//! Parser-combinator go brrrrr

use crate::{
    ast::{
        ArrayElement, ArrayLiteral, Expression, Identifier, Literal,
        ObjectLiteral, ObjectProperty, PropertyName, Script, Statement,
    },
    error::LoadError,
    Source,
};
use chumsky::prelude::*;

/// Parse source code into an Abstract Syntax Tree
pub fn parse(source: impl Source) -> Result<Script, LoadError> {
    let name = source.name();
    let code = source.text()?;
    let parser = parser();
    parser
        .parse(&code)
        .into_result()
        .map_err(|errors| LoadError::Parse {
            source_name: name.map(String::from),
            errors: errors.into_iter().map(Rich::into_owned).collect(),
        })
}

/// TODO
trait Parser2<'src, O>:
    'src + Parser<'src, &'src str, O, extra::Err<Rich<'src, char>>>
{
}

impl<'src, O, P> Parser2<'src, O> for P where
    P: 'src + Parser<'src, &'src str, O, extra::Err<Rich<'src, char>>>
{
}

fn parser<'src>() -> impl Parser2<'src, Script> {
    statement()
        .repeated()
        .collect::<Vec<_>>()
        .map(|statements| Script {
            statements: statements.into(),
        })
}

/// Parser for a single statement
fn statement<'src>() -> impl Parser2<'src, Statement> {
    // Semicolons are always optional
    let terminator = just(';').or_not();

    choice((expression()
        .map(Statement::Expression)
        .then_ignore(terminator),))
    .boxed()
}

/// Parser for a single expression
fn expression<'src>() -> impl Parser2<'src, Expression> {
    let mut expression = Recursive::declare();
    expression.define(
        choice((literal(expression.clone()).map(Expression::Literal),)).boxed(),
    );
    expression
}

/// Parser for a value literal
fn literal<'src>(
    expression: impl Parser2<'src, Expression> + Clone,
) -> impl Parser2<'src, Literal> {
    choice((
        just("undefined").to(Literal::Undefined),
        just("null").to(Literal::Null),
        just("false").to(Literal::Boolean(false)),
        just("true").to(Literal::Boolean(true)),
        int().map(Literal::Int),
        float().map(Literal::Float),
        string().map(Literal::String),
        array_literal(expression.clone()).map(Literal::Array),
        object_literal(expression).map(Literal::Object),
    ))
    .padded()
}

/// Parser for a signed int literal
fn int<'src>() -> impl Parser2<'src, i64> {
    just('-')
        .or_not()
        .then(text::int(10))
        .to_slice()
        .map(|s: &str| s.parse().unwrap())
}

/// Parser for a float literal
fn float<'src>() -> impl Parser2<'src, f64> {
    let digits = text::digits(10);

    let exp = just('e')
        .or(just('E'))
        .then(one_of("+-").or_not())
        .then(digits);

    just('-')
        .or_not()
        .then(digits)
        .then(just('.'))
        .then(digits.or_not())
        .then(exp.or_not())
        .to_slice()
        .map(|s: &str| s.parse().unwrap())
}

/// Parser for a string literal
fn string<'src>() -> impl Parser2<'src, String> {
    let escape = just('\\')
        .then(choice((
            just('\\'),
            just('/'),
            just('"'),
            just('b').to('\x08'),
            just('f').to('\x0C'),
            just('n').to('\n'),
            just('r').to('\r'),
            just('t').to('\t'),
            just('u').ignore_then(
                text::digits(16).exactly(4).to_slice().validate(
                    |digits, e, emitter| {
                        char::from_u32(u32::from_str_radix(digits, 16).unwrap())
                            .unwrap_or_else(|| {
                                emitter.emit(Rich::custom(
                                    e.span(),
                                    "invalid unicode character",
                                ));
                                '\u{FFFD}' // unicode replacement character
                            })
                    },
                ),
            ),
        )))
        .ignored()
        .boxed();

    none_of("\\\"")
        .ignored()
        .or(escape)
        .repeated()
        .to_slice()
        .map(ToString::to_string)
        .delimited_by(just('"'), just('"'))
}

/// Parser for an array literal
fn array_literal<'src>(
    expression: impl Parser2<'src, Expression>,
) -> impl Parser2<'src, ArrayLiteral> {
    let element = expression.map(ArrayElement::Expression);
    element
        .separated_by(just(',').padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .map(|elements| ArrayLiteral {
            elements: elements.into(),
        })
        .padded()
        .delimited_by(just('['), just(']'))
        .boxed()
}

/// Parser for an object literal
fn object_literal<'src>(
    expression: impl Parser2<'src, Expression>,
) -> impl Parser2<'src, ObjectLiteral> {
    let member = string()
        .then_ignore(just(':').padded())
        .then(expression)
        .map(|(key, value)| ObjectProperty::Property {
            property: PropertyName::Literal(Identifier(key)),
            expression: value,
        });
    member
        .separated_by(just(',').padded())
        .collect::<Vec<_>>()
        .map(|properties| ObjectLiteral {
            properties: properties.into(),
        })
        .padded()
        .delimited_by(just('{'), just('}'))
        .boxed()
}
