use indexmap::IndexMap;
use petitscript::{value::Number, Engine, Error, Exports, Process, Value};
use std::{env, path::PathBuf, sync::LazyLock};
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

/// Test a full compile+execution pipeline for an external PS file. This expects
/// the script to both compile and execute successfully, and asserts on the
/// *default* export of each script. If we need to assert on multiple values,
/// export an object.
#[test_case("capture", 9; "capture")]
#[test_case("parameterScope", 7; "parameter_scope")]
#[test_case("importLocal", 6; "import_local")]
#[test_case("importNative", 6; "import_native")]
#[test_case("json", true; "json")]
#[test_case("prototype", true; "prototype")]
fn execution(file_name: &'static str, expected: impl Into<Value>) {
    let path = PathBuf::from(format!("tests/ps/{file_name}.js"));
    let exported = execute(path).unwrap_or_else(|error| panic!("{error}"));
    let expected_default = expected.into();
    assert_eq!(exported, expected_default);
}

/// Test error cases during execution. Assert the program fails with an error
/// message _containing_ the given error
#[test_case(
    "stackTrace",
    "Error (most recent call last)
  in <root> at $CWD/tests/ps/stackTrace.js:11:16
  in g at $CWD/tests/ps/stackTrace.js:8:10
  in f at $CWD/tests/ps/stackTrace.js:4:10
  in error at $CWD/tests/ps/common.js:3:10
`x` is not defined";
    "stack_trace")]
fn error(file_name: &'static str, expected_error: &'static str) {
    let path = PathBuf::from(format!("tests/ps/{file_name}.js"));
    let cwd = env::current_dir()
        .unwrap()
        .into_os_string()
        .into_string()
        .unwrap();
    // Current dir is dynamic so we have to manually replace it
    let expected_error = expected_error.replace("$CWD", &cwd);
    let error = execute(path).unwrap_err().to_string();
    assert!(
        error.contains(&expected_error),
        "Expected error message to contain `{expected_error}`\nbut it was:\n\
        {error}"
    );
}

/// Compile and execute a program. If the program succeeds, expected no named
/// exports and return the default export
fn execute(path: PathBuf) -> Result<Value, Error> {
    if !path.exists() {
        // Sanity check
        panic!("Path {path:?} does not exist");
    }
    let process = ENGINE
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
