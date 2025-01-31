use petit_js::Engine;
use std::{env, path::Path};

fn main() {
    let path = env::args().nth(1).expect("Expected path");
    let engine = Engine::new();
    match engine.load(Path::new(&path)) {
        Ok(module) => println!("Exported:\n{module:#}"),
        Err(error) => eprintln!("Error: {error}"),
    }
}
