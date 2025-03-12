use indexmap::IndexMap;
use petitscript::{Engine, Value};
use std::path::PathBuf;
use test_case::test_case;

/// Test a full compile+execution pipeline for an external PS file. This expects
/// the script to both compile and execute successfully, and asserts on the
/// *default* export of each script. If we need to assert on multiple values,
/// export an object.
#[test_case("capture", 9; "capture")]
#[test_case("parameterScope", 7; "parameter_scope")]
fn test_execution(file_name: &'static str, expected: impl Into<Value>) {
    let engine = Engine::new();
    let path = PathBuf::from(format!("tests/ps/{file_name}.js"));
    if !path.exists() {
        // Sanity check
        panic!("Path {path:?} does not exist");
    }
    let process = engine
        .compile(path.clone())
        .unwrap_or_else(|error| panic!("Error compiling {path:?}: {error}"));
    let exports = process
        .execute()
        .unwrap_or_else(|error| panic!("Error executing {path:?}: {error}"));

    assert_eq!(
        exports.named,
        IndexMap::new(),
        "Named exports should be empty"
    );
    let expected_default = expected.into();
    assert_eq!(
        exports.default.expect("Expected default export"),
        expected_default
    );
}
