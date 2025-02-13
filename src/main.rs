use petit_js::{Engine, Error};
use std::{env, path::Path};

fn main() -> Result<(), Error> {
    let path = env::args().nth(1).expect("Expected path");
    let engine = Engine::new();
    let mut process = engine.load(Path::new(&path))?;
    let exports = smol::block_on(process.execute())?;
    println!("Exported:\n{exports:#}");
    Ok(())
}
