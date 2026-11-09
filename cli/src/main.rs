use petitscript::{Engine, Source};
use std::env;

fn main() -> miette::Result<()> {
    let path = env::args().nth(1).expect("Expected path");
    let engine = Engine::default();
    let process = engine.compile(Source::file(path)?)?;

    let exports = process.execute()?;
    println!("Exported:\n{exports:#}");
    let json = serde_json::to_string_pretty(&exports.default).unwrap();
    println!("Default (JSON): {json}");
    Ok(())
}
