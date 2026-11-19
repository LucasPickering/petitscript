//! Proptest implementations for AST types
//!
//! I wanted to call this module `proptest`, but that interferes with the crate
//! name in generated code. `pbt` is nice and short.

use crate::ast::{
    BindingKind, Declaration, Expression, ExpressionKind, IdentifierNode,
    StatementKind, Variable,
};
use proptest::{
    arbitrary::{Arbitrary, any, any_with},
    prop_oneof,
    strategy::{BoxedStrategy, Strategy},
};

// These all return BoxedStrategy because it's ways easier than writing down the
// static types. There's probably a small perf penalty for it.
// unstable: https://github.com/rust-lang/rust/issues/63063
// Use `impl Strategy<Value = ...>` in each implementation

/// Strategy for `Declaration::variables`
pub fn declaration_variables() -> impl Strategy<Value = Box<[Variable]>> {
    any_with::<Box<[Variable]>>((
        // Variable list can't be empty
        (1..=3).into(),
        // Force init clause to be populated (which is syntactically required)
        Some(true),
    ))
}

/// Strategy for `Variable::init`
///
/// Parameter value:
/// - `Some(true)`: Always populate `Some(_)`
/// - `Some(false)`: Always return `None`
/// - `None`: Randomly determine `None` vs `Some(_)`
///
/// It would be nice to use the `Probability` type here, but the `weighted()`
/// strategy doesn't accept `0.0` or `1.0` as weights (why not???).
pub fn variable_init(
    guarantee: Option<bool>,
) -> impl Strategy<Value = Option<Box<Expression>>> {
    match guarantee {
        Some(false) => Strategy::boxed((|| None) as fn() -> _),
        Some(true) => any::<Box<Expression>>().prop_map(Some).boxed(),
        None => any::<Option<Box<Expression>>>().boxed(),
    }
}

// ==============
// Manually implemented a select few recursive nodes. The entire AST is
// mutually recursive so I picked just a few to break up the recursive.
// ==============
impl Arbitrary for ExpressionKind {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf =
            prop_oneof![any::<IdentifierNode>().prop_map(Self::Identifier),];
        // leaf.prop_recursive(8, 256, 10, |inner| {
        //     prop_oneof![
        //         any::<Expression>()
        //             .prop_map(|expr| Self::Parenthesized(Box::new(expr)))
        //     ]
        // })
        // .boxed()
        leaf.boxed()
    }
}

impl Arbitrary for StatementKind {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = prop_oneof![
            any::<()>().prop_map(|()| Self::Empty),
            any::<Expression>().prop_map(Self::Expression),
            any::<Declaration>().prop_map(Self::Declaration),
        ];
        // leaf.prop_recursive(8, 256, 10, |inner| {
        //     prop_oneof![any::<Block>().prop_map(Self::Block)]
        // })
        // .boxed()
        leaf.boxed()
    }
}

impl Arbitrary for BindingKind {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        // TODO recursion
        prop_oneof![any::<IdentifierNode>().prop_map(Self::Identifier),].boxed()
    }
}
