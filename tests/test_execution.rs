use indexmap::IndexMap;
use petitscript::{
    error::RuntimeError, Engine, Error, Exports, Number, Process, Value,
};
use std::path::PathBuf;
use test_case::test_case;

fn add(_: &Process, (a, b): (Number, Number)) -> Result<Number, RuntimeError> {
    Ok(a + b)
}

/// Test a full compile+execution pipeline for an external PS file. This expects
/// the script to both compile and execute successfully, and asserts on the
/// *default* export of each script. If we need to assert on multiple values,
/// export an object.
#[test_case("capture", 9; "capture")]
#[test_case("parameterScope", 7; "parameter_scope")]
#[test_case("importLocal", 6; "import_local")]
#[test_case("importNative", 6; "import_native")]
#[test_case("json", true; "json")]
fn test_execution(file_name: &'static str, expected: impl Into<Value>) {
    let mut engine = Engine::new();
    let module = Exports::named([("add", engine.create_fn(add))]);
    engine.register_module("math", module).unwrap();
    let path = PathBuf::from(format!("tests/ps/{file_name}.js"));

    let exported = execute(&engine, path).unwrap();
    let expected_default = expected.into();
    assert_eq!(exported, expected_default);
}

/// Test error cases during execution. Assert the program fails with an error
/// message _containing_ the given error
#[test_case(
    "stackTrace",
    "Error (most recent call last)
  in <root> at ps/stackTrace.js:11:16
  in g at ps/stackTrace.js:8:10
  in f at ps/stackTrace.js:4:10
  in error at ps/common.js:3:10
`x` is not defined";
    "stack_trace")]
fn test_error(file_name: &'static str, expected_error: &'static str) {
    let mut engine = Engine::new();
    let module = Exports::named([("add", engine.create_fn(add))]);
    engine.register_module("math", module).unwrap();
    let path = PathBuf::from(format!("tests/ps/{file_name}.js"));

    let error = execute(&engine, path).unwrap_err().to_string();
    assert!(
        error.contains(expected_error),
        "Expected error message to contain `{expected_error}`\nbut it was:\n\
        {error}"
    );
}

/// Compile and execute a program. If the program succeeds, expected no named
/// exports and return the default export
fn execute(engine: &Engine, path: PathBuf) -> Result<Value, Error> {
    if !path.exists() {
        // Sanity check
        panic!("Path {path:?} does not exist");
    }
    let process = engine
        .compile(path.clone())
        .unwrap_or_else(|error| panic!("Error compiling {path:?}: {error}"));
    let exports = process.execute()?;

    assert_eq!(
        exports.named,
        IndexMap::new(),
        "Named exports should be empty"
    );

    Ok(exports.default.expect("Expected default export"))
}
