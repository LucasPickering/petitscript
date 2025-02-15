use petit_js::{Engine, Error};
use std::{env, path::Path};

fn main() -> Result<(), Error> {
    let path = env::args().nth(1).expect("Expected path");
    let engine = Engine::new();
    let process = engine.load(Path::new(&path))?;
    let exports = process.execute()?;
    println!("Exported:\n{exports:#}");
    Ok(())
}
