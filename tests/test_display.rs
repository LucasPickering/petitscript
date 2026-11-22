use petitscript::{Source, ast::NodeId};
use pretty_assertions::assert_eq;

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
    let source = Source::file("tests/ps/everything.js").unwrap();
    let parsed = source.parse().unwrap_or_else(|error| panic!("{error:#}"));
    // This line is what we're testing!! The source codes won't be textually
    // equal, but they should be semantically equal
    let stringified = parsed.root().to_string();
    let reparsed = Source::from(stringified.clone())
        .parse()
        .unwrap_or_else(|error| panic!("{error:#}"));
    NodeId::with_all_equal(|| {
        assert_eq!(parsed.root(), reparsed.root(), "{stringified}");
    });
}
