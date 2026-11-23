use petitscript::Source;
use pretty_assertions::assert_eq;
use std::{path::Path, sync::Arc};

/// Test parse -> display -> parse round trip
///
/// 1. Start with source code because it's easy to write
/// 2. Parse it to get an AST
/// 3. Stringify the AST back to source code
/// 4. Parse again
/// 5. Compare the two ASTs.
///
/// **Step 3** is the target of this test, the rest is overhead. This is
/// trusting that the parse works.
#[test]
fn display_round_trip() {
    let js_root = Path::new("tests/ps/");
    let source = Source::file(js_root.join("everything.js")).unwrap();
    let parsed = source.parse().unwrap_or_else(|error| panic!("{error:#}"));
    // This line is what we're testing!! The source codes won't be textually
    // equal, but they should be semantically equal
    let stringified: Arc<str> = parsed.root().to_string().into();
    let reparsed = Source::String {
        text: stringified.clone(),
        root: js_root.to_owned(),
    }
    .parse()
    .unwrap_or_else(|error| panic!("{error:#}"));
    // Node IDs should all match because the AST is parsed in the same order
    assert_eq!(parsed.root(), reparsed.root(), "{stringified}");
}
