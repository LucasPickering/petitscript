use indexmap::IndexMap;
use petitscript::{
    Engine, Exports, Source, Value, World, error::RuntimeError, value::Number,
};
use pretty_assertions::assert_eq;
use std::{path::Path, sync::LazyLock};
use test_case::test_case;

fn add(_: &World, (a, b): (Number, Number)) -> Number {
    a + b
}

/// Shared engine used for all tests
static ENGINE: LazyLock<Engine> = LazyLock::new(|| {
    let mut exports = Exports::default();
    exports.export_fn("add", add);
    Engine::builder()
        .with_stdlib()
        .with_native_module("math", exports)
        .build()
});

/// Test a full compile+execution pipeline for an external PS file
///
/// This expects the script to both compile and execute successfully, and
/// asserts on the *default* export of each script. If we need to assert on
/// multiple values, export an object.
#[test_case(
    "bindings.js",
    [
        // Object bindings
        ("a", Value::from(1)),
        ("b", 2.into()),
        ("cd", [("c", 3), ("d", 4)].into()),
        // Arr bindings
        ("j", 1.into()),
        ("k", 2.into()),
        ("lm", [3, 4].into()),
        // Plain const bindings
        ("x", 1.into()),
        ("y", 2.into()),
        ("objDef", 7.into()),
        ("arrDef", 7.into()),
        ("skip", [1, 3].into()),
    ];
    "bindings"
)]
#[test_case("capture.js", 9; "capture")]
#[test_case("importLocal.js", 9; "import_local")]
#[test_case("importNative.js", 9; "import_native")]
#[test_case("json.js", true; "json")]
#[test_case("parameterScope.js", 7; "parameter_scope")]
#[test_case(
    "propertyAccess.js",
    [
        ("id", 1.into()),
        ("opt", 2.into()),
        ("expr", 3.into()),
        ("missing", Value::Undefined),
    ];
    "property_access"
)]
#[test_case("prototype.js", true; "prototype")]
#[test_case("recursion.js", [("direct", 0), ("mutual", 0)]; "recursion")]
#[test_case("varargs.js", 3; "varargs")]
fn execution(file_name: &'static str, expected: impl Into<Value>) {
    let exported = execute(format!("tests/ps/{file_name}"))
        .unwrap_or_else(|error| panic!("{error:#}"));
    let expected_default = expected.into();
    assert_eq!(exported, expected_default);
}

/// Test error cases during execution
///
/// Assert the program fails with the given error message.
#[test_case(
    "importUnknown.js",
    "Module does not provide an export with the name `unknown`";
    "import_unknown"
)]
fn error(file_name: &'static str, expected_error: &'static str) {
    let error = execute(format!("tests/ps/{file_name}")).unwrap_err();
    let error = format!("{error:#}");
    assert!(
        error.contains(expected_error),
        "Expected error message to contain `{expected_error}`\nbut it was:\n\
        {error}"
    );
}

/// Test stack traces on runtime errors
///
/// This format is pretty specific and feels odd to test, but I want to make
/// sure SOMETHING is testing stack traces.
#[test]
fn error_stack_trace() {
    let error = execute("tests/ps/stackTrace.js").unwrap_err();
    let error = format!("{error:#}");
    assert_eq!(
        error,
        "
error: `x` is not defined
   |
   = note: most recent call last
  --> tests/ps/stackTrace.js:11:16
   |
11 | export default g();
   |                ^^^
   |
  ::: tests/ps/stackTrace.js:8:10
   |
 8 |   return f() + 1;
   |          ^^^
   |
  ::: tests/ps/stackTrace.js:4:10
   |
 4 |   return error();
   |          ^^^^^^^
   |
  ::: tests/ps/common.js:2:28
   |
 2 | export const error = () => x;
   |                            ^
",
    );
}

/// Compile and execute a program
///
/// If the program succeeds, expected no named exports and return the default
/// export.
fn execute(path: impl AsRef<Path>) -> Result<Value, RuntimeError> {
    let path = path.as_ref();
    // Sanity check
    assert!(path.exists(), "Path {} does not exist", path.display());
    // Only propagate runtime errors. Everything else is a panic.
    let source = Source::file(path).unwrap();
    let program = source.parse().unwrap();
    let world = ENGINE.instantiate(program);
    let exports = world.run()?;

    assert_eq!(
        exports.named,
        IndexMap::new(),
        "Named exports should be empty"
    );

    Ok(exports.default.expect("Expected default export"))
}
