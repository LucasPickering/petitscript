use indexmap::IndexMap;
use petitscript::{
    Engine, Error, Exports, Process, Source, Value, value::Number,
};
use std::{path::PathBuf, sync::LazyLock};
use test_case::test_case;

fn add(_: &Process, (a, b): (Number, Number)) -> Number {
    a + b
}

/// Shared engine used for all tests
static ENGINE: LazyLock<Engine> = LazyLock::new(|| {
    let mut exports = Exports::default();
    exports.export_fn("add", add);
    Engine::builder()
        .with_stdlib()
        .with_module("math".parse().unwrap(), exports)
        .build()
});

/// Test a full compile+execution pipeline for an external PS file
///
/// This expects the script to both compile and execute successfully, and
/// asserts on the *default* export of each script. If we need to assert on
/// multiple values, export an object.
#[test_case(
    "bindings",
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
#[test_case("capture", 9; "capture")]
#[test_case("importLocal", 9; "import_local")]
#[test_case("importNative", 9; "import_native")]
#[test_case("json", true; "json")]
#[test_case("parameterScope", 7; "parameter_scope")]
#[test_case("prototype", true; "prototype")]
#[test_case("varargs", 3; "varargs")]
fn execution(file_name: &'static str, expected: impl Into<Value>) {
    let path = PathBuf::from(format!("tests/ps/{file_name}.js"));
    let exported = execute(path).unwrap_or_else(|error| panic!("{error}"));
    let expected_default = expected.into();
    assert_eq!(exported, expected_default);
}

/// Test error cases during execution
///
/// Assert the program fails with an error message _containing_ the given error.
#[test_case(
    "stackTrace",
    "Error (most recent call last)
  in <root> at tests/ps/stackTrace.js:11:16
  in g at tests/ps/stackTrace.js:8:10
  in f at tests/ps/stackTrace.js:4:10
  in error at tests/ps/common.js:2:28
`x` is not defined";
    "stack_trace")]
#[test_case(
    "importUnknown",
    "Module does not provide an export with the name `unknown`";
    "import_unknown"
)]
fn error(file_name: &'static str, expected_error: &'static str) {
    let path = PathBuf::from(format!("tests/ps/{file_name}.js"));
    let error = execute(path).unwrap_err().to_string();
    assert!(
        error.contains(expected_error),
        "Expected error message to contain `{expected_error}`\nbut it was:\n\
        {error}"
    );
}

/// Compile and execute a program
///
/// If the program succeeds, expected no named exports and return the default
/// export.
fn execute(path: PathBuf) -> Result<Value, Error> {
    // Sanity check
    assert!(path.exists(), "Path {} does not exist", path.display());
    let source = Source::file(&path).unwrap();
    let process = ENGINE.compile(source).unwrap();
    let exports = process.execute()?;

    assert_eq!(
        exports.named,
        IndexMap::new(),
        "Named exports should be empty"
    );

    Ok(exports.default.expect("Expected default export"))
}
