//! JSON parsing and stringification

use crate::{Array, Number, Object, Value};
use std::{fmt::Write, str};
use winnow::{
    ascii::{dec_int, float},
    combinator::{
        alt, delimited, dispatch, empty, fail, not, peek, preceded, repeat,
        separated, separated_pair, terminated,
    },
    error::{AddContext, ContextError, ParserError, StrContext},
    token::{any, none_of, one_of, take, take_while},
    Parser,
};

/// Recursively write a JSON value into a string buffer
pub fn write_json(buf: &mut String, value: &Value) {
    // Errors aren't possible in any of these writes because we're writing to
    // a string
    match value {
        Value::Undefined => todo!(),
        Value::Null => write!(buf, "null").unwrap(),
        Value::Boolean(b) => write!(buf, "{b}").unwrap(),
        Value::Number(number) => write!(buf, "{number}").unwrap(),
        Value::String(s) => write!(buf, "\"{s}\"").unwrap(),
        Value::Array(array) => {
            write!(buf, "[").unwrap();
            for (i, element) in array.iter().enumerate() {
                if i > 0 {
                    write!(buf, ", ").unwrap();
                }
                write_json(buf, element);
            }
            write!(buf, "]").unwrap();
        }
        Value::Object(object) => {
            write!(buf, "{{").unwrap();
            for (i, (key, value)) in object.iter().enumerate() {
                if i > 0 {
                    write!(buf, ", ").unwrap();
                }
                write!(buf, "\"{key}\"").unwrap();
                write!(buf, ": ").unwrap();
                write_json(buf, value);
            }
            write!(buf, "}}").unwrap();
        }
        // These two can't be represented well as JSON, so just use undefined
        #[cfg(feature = "buffer")]
        Value::Buffer(_) => write_json(buf, &Value::Undefined),
        Value::Function(_) => write_json(buf, &Value::Undefined),
    }
}

type Stream<'i> = &'i str;

/// Parse a JSON string to a [Value]
pub fn parse_json(mut input: &str) -> Result<Value, ContextError> {
    delimited(ws, json_value, ws).parse_next(&mut input)
}

// Parser implementation is taken from winnow's examples:
// https://github.com/winnow-rs/winnow/blob/v0.7.4/examples/json/parser_dispatch.rs

/// `alt` is a combinator that tries multiple parsers one by one, until
/// one of them succeeds
fn json_value<
    'i,
    E: ParserError<Stream<'i>> + AddContext<Stream<'i>, StrContext>,
>(
    input: &mut Stream<'i>,
) -> winnow::Result<Value, E> {
    // `dispatch` gives you `match`-like behavior compared to `alt` successively
    // trying different implementations.
    dispatch!(peek(any);
        'n' => null.value(Value::Null),
        't' => true_.map(Value::Boolean),
        'f' => false_.map(Value::Boolean),
        '"' => string.map(Value::from),
        '+' => number.map(Value::Number),
        '-' => number.map(Value::Number),
        '0'..='9' => number.map(Value::Number),
        '[' => array.map(Value::Array),
        '{' => object.map(Value::Object),
        _ => fail,
    )
    .parse_next(input)
}

/// `literal(string)` generates a parser that takes the argument string.
///
/// This also shows returning a sub-slice of the original input
fn null<'i, E: ParserError<Stream<'i>>>(
    input: &mut Stream<'i>,
) -> Result<&'i str, E> {
    // This is a parser that returns `"null"` if it sees the string "null", and
    // an error otherwise
    "null".parse_next(input)
}

/// We can combine `tag` with other functions, like `value` which returns a
/// given constant value on success.
fn true_<'i, E: ParserError<Stream<'i>>>(
    input: &mut Stream<'i>,
) -> Result<bool, E> {
    // This is a parser that returns `true` if it sees the string "true", and
    // an error otherwise
    "true".value(true).parse_next(input)
}

/// We can combine `tag` with other functions, like `value` which returns a
/// given constant value on success.
fn false_<'i, E: ParserError<Stream<'i>>>(
    input: &mut Stream<'i>,
) -> Result<bool, E> {
    // This is a parser that returns `false` if it sees the string "false", and
    // an error otherwise
    "false".value(false).parse_next(input)
}

/// Parse a number. Try to parse it as an int first, and it that fails then
/// parse as a float
fn number<'i, E: ParserError<Stream<'i>>>(
    input: &mut Stream<'i>,
) -> Result<Number, E> {
    alt((
        // If the int is trailed by `.` or `e`, it's actually a float
        (dec_int, peek(not(one_of(['.', 'e'])))).map(|(i, _)| Number::Int(i)),
        float.map(Number::Float),
    ))
    .parse_next(input)
}

/// This parser gathers all `char`s up into a `String`with a parse to take the
/// double quote character, before the string (using `preceded`) and after the
/// string (using `terminated`).
fn string<
    'i,
    E: ParserError<Stream<'i>> + AddContext<Stream<'i>, StrContext>,
>(
    input: &mut Stream<'i>,
) -> Result<String, E> {
    preceded(
        '\"',
        terminated(
            repeat(0.., character).fold(String::new, |mut string, c| {
                string.push(c);
                string
            }),
            '\"',
        ),
    )
    // `context` lets you add a static string to errors to provide more
    // information in the error chain (to indicate which parser had an
    // error)
    .context(StrContext::Expected("string".into()))
    .parse_next(input)
}

/// You can mix the above declarative parsing with an imperative style to handle
/// more unique cases, like escaping
fn character<'i, E: ParserError<Stream<'i>>>(
    input: &mut Stream<'i>,
) -> Result<char, E> {
    let c = none_of('\"').parse_next(input)?;
    if c == '\\' {
        dispatch!(any;
          '"' => empty.value('"'),
          '\\' => empty.value('\\'),
          '/'  => empty.value('/'),
          'b' => empty.value('\x08'),
          'f' => empty.value('\x0C'),
          'n' => empty.value('\n'),
          'r' => empty.value('\r'),
          't' => empty.value('\t'),
          'u' => unicode_escape,
          _ => fail,
        )
        .parse_next(input)
    } else {
        Ok(c)
    }
}

fn unicode_escape<'i, E: ParserError<Stream<'i>>>(
    input: &mut Stream<'i>,
) -> Result<char, E> {
    alt((
        // Not a surrogate
        u16_hex
            .verify(|cp| !(0xD800..0xE000).contains(cp))
            .map(|cp| cp as u32),
        // See https://en.wikipedia.org/wiki/UTF-16#Code_points_from_U+010000_to_U+10FFFF for details
        separated_pair(u16_hex, "\\u", u16_hex)
            .verify(|(high, low)| {
                (0xD800..0xDC00).contains(high)
                    && (0xDC00..0xE000).contains(low)
            })
            .map(|(high, low)| {
                let high_ten = (high as u32) - 0xD800;
                let low_ten = (low as u32) - 0xDC00;
                (high_ten << 10) + low_ten + 0x10000
            }),
    ))
    .verify_map(
        // Could be probably replaced with .unwrap() or _unchecked due to the
        // verify checks
        std::char::from_u32,
    )
    .parse_next(input)
}

fn u16_hex<'i, E: ParserError<Stream<'i>>>(
    input: &mut Stream<'i>,
) -> Result<u16, E> {
    take(4usize)
        .verify_map(|s| u16::from_str_radix(s, 16).ok())
        .parse_next(input)
}

/// Some combinators, like `separated` or `repeat`, will call a parser
/// repeatedly, accumulating results in a `Vec`, until it encounters an error.
/// If you want more control on the parser application, check out the `iterator`
/// combinator (cf `examples/iterator.rs`)
fn array<
    'i,
    E: ParserError<Stream<'i>> + AddContext<Stream<'i>, StrContext>,
>(
    input: &mut Stream<'i>,
) -> Result<Array, E> {
    preceded(
        ('[', ws),
        terminated(separated(0.., json_value, (ws, ',', ws)), (ws, ']')),
    )
    .map(|elements: Vec<Value>| Array::from(elements))
    .context(StrContext::Expected("array".into()))
    .parse_next(input)
}

fn object<
    'i,
    E: ParserError<Stream<'i>> + AddContext<Stream<'i>, StrContext>,
>(
    input: &mut Stream<'i>,
) -> Result<Object, E> {
    preceded(
        ('{', ws),
        terminated(separated(0.., key_value, (ws, ',', ws)), (ws, '}')),
    )
    .map(|items: Vec<(String, Value)>| items.into_iter().collect())
    .context(StrContext::Expected("object".into()))
    .parse_next(input)
}

fn key_value<
    'i,
    E: ParserError<Stream<'i>> + AddContext<Stream<'i>, StrContext>,
>(
    input: &mut Stream<'i>,
) -> Result<(String, Value), E> {
    separated_pair(string, (ws, ':', ws), json_value).parse_next(input)
}

/// Parser combinators are constructed from the bottom up:
/// first we write parsers for the smallest elements (here a space character),
/// then we'll combine them in larger parsers
fn ws<'i, E: ParserError<Stream<'i>>>(
    input: &mut Stream<'i>,
) -> Result<&'i str, E> {
    const WS: &[char] = &[' ', '\t', '\r', '\n'];

    // Combinators like `take_while` return a function. That function is the
    // parser,to which we can pass the input
    take_while(0.., WS).parse_next(input)
}

#[cfg(test)]
mod test {
    use super::*;
    use indexmap::indexmap;

    type Error = winnow::error::ContextError;

    #[test]
    fn json_number() {
        assert_eq!(number::<Error>.parse_peek("42"), Ok(("", Number::Int(42))));
        assert_eq!(
            number::<Error>.parse_peek("42.0"),
            Ok(("", Number::Float(42.0)))
        );
        assert_eq!(
            number::<Error>.parse_peek("1e3"),
            Ok(("", Number::Float(1000.0)))
        );
    }

    #[test]
    fn json_string() {
        assert_eq!(string::<Error>.parse_peek("\"\""), Ok(("", "".to_owned())));
        assert_eq!(
            string::<Error>.parse_peek("\"abc\""),
            Ok(("", "abc".to_owned()))
        );
        assert_eq!(
            string::<Error>.parse_peek(
                "\"abc\\\"\\\\\\/\\b\\f\\n\\r\\t\\u0001\\u2014\u{2014}def\""
            ),
            Ok(("", "abc\"\\/\x08\x0C\n\r\t\x01——def".to_owned())),
        );
        assert_eq!(
            string::<Error>.parse_peek("\"\\uD83D\\uDE10\""),
            Ok(("", "😐".to_owned()))
        );

        assert!(string::<Error>.parse_peek("\"").is_err());
        assert!(string::<Error>.parse_peek("\"abc").is_err());
        assert!(string::<Error>.parse_peek("\"\\\"").is_err());
        assert!(string::<Error>.parse_peek("\"\\u123\"").is_err());
        assert!(string::<Error>.parse_peek("\"\\uD800\"").is_err());
        assert!(string::<Error>.parse_peek("\"\\uD800\\uD800\"").is_err());
        assert!(string::<Error>.parse_peek("\"\\uDC00\"").is_err());
    }

    #[test]
    fn json_object() {
        let input = r#"{"a":42,"b":"x"}"#;
        let expected = Value::from(indexmap! {
            "a" => Value::from(42),
            "b" => "x".into(),
        });
        assert_eq!(parse_json(input).unwrap(), expected);
    }

    #[test]
    fn json_array() {
        let input = r#"[42,"x"]"#;
        let expected = Value::from(vec![Value::from(42), "x".into()]);
        assert_eq!(parse_json(input).unwrap(), expected);
    }

    #[test]
    fn json_whitespace() {
        let input = r#"
  {
    "null" : null,
    "true"  :true ,
    "false":  false  ,
    "number" : 123e4 ,
    "string" : " abc 123 " ,
    "array" : [ false , 1 , "two" ] ,
    "object" : { "a" : 1.0 , "b" : "c" } ,
    "empty_array" : [  ] ,
    "empty_object" : {   }
  }
  "#;

        let expected = Value::from(indexmap! {
            "null" => Value::Null,
            "true" => true.into(),
            "false" => false.into(),
            "number" => 123e4.into(),
            "string" => " abc 123 ".into(),
            "array" => vec![
                Value::from(false),
                Value::from(1),
                Value::from("two"),
            ].into(),
            "object" => indexmap! {
                "a" => Value::from(1.0),
                "b" => "c".into(),
            }.into(),
            "empty_array" => Array::new().into(),
            "empty_object" => Object::new().into(),
        });

        assert_eq!(parse_json(input).unwrap(), expected);
    }
}
