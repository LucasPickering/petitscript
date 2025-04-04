use indexmap::IndexMap;
use petitscript::{
    error::RuntimeError, Engine, Exports, Number, Process, Value,
};
use std::path::PathBuf;
use test_case::test_case;

/// Test a full compile+execution pipeline for an external PS file. This expects
/// the script to both compile and execute successfully, and asserts on the
/// *default* export of each script. If we need to assert on multiple values,
/// export an object.
#[test_case("capture", 9; "capture")]
#[test_case("parameterScope", 7; "parameter_scope")]
#[test_case("importLocal", 6; "import_local")]
#[test_case("importNative", 6; "import_native")]
fn test_execution(file_name: &'static str, expected: impl Into<Value>) {
    fn add(
        _: &Process,
        (a, b): (Number, Number),
    ) -> Result<Number, RuntimeError> {
        Ok(a + b)
    }

    let mut engine = Engine::new();
    let module = Exports::named([("add", engine.create_fn(add))]);
    engine.register_module("math", module).unwrap();
    let path = PathBuf::from(format!("tests/ps/{file_name}.js"));

    let exported = execute(&engine, path);
    let expected_default = expected.into();
    assert_eq!(exported, expected_default);
}

/// Compile and execute a program. Expect no named exports, and return the
/// default export.
fn execute(engine: &Engine, path: PathBuf) -> Value {
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

    exports.default.expect("Expected default export")
}
