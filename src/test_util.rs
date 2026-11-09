/// Assert the given expression matches a pattern and optional condition
///
/// Additionally, evaluate an expression using the bound pattern. This can be
/// used to apply additional assertions inline, or extract bound values to use
/// in subsequent statements.
///
/// ```
/// let x = [1, 2, 3];
/// assert_matches!(x, [_, _, _]);
///
/// // Conditions
/// assert_matches!(x, [e0, _, _] if e0 == 1);
///
/// // Extract a value
/// let e0 = assert_matches!(x, [e0, _, _] => e0);
/// assert_eq!(e0, 1);
/// ```
#[macro_export]
macro_rules! assert_matches {
    ($expr:expr, $pattern:pat $(if $condition:expr)? $(,)?) => {
        $crate::assert_matches!($expr, $pattern $(if $condition)? => ());
    };
    ($expr:expr, $pattern:pat $(if $condition:expr)? => $output:expr $(,)?) => {
        match $expr {
            // If a conditional was given, check it. This has to be a separate
            // arm to prevent borrow fighting over the matched value
            $(ref value @ $pattern if !$condition => {
                panic!(
                    "Value {value:?} does not match condition {condition}",
                    condition = stringify!($condition),
                );
            })?
            $pattern => $output,
            value => panic!(
                "Unexpected value {value:?} does not match pattern {expected}",
                expected = stringify!($pattern),
            ),
        }
    };
}
