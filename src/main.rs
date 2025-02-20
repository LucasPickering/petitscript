use petit_js::Engine;
use std::{env, path::Path};

fn main() {
    let path = env::args().nth(1).expect("Expected path");
    let engine = Engine::new();
    let process = match engine.parse(Path::new(&path)) {
        Ok(process) => process,
        Err(error) => {
            eprintln!("{error}");
            return;
        }
    };
    match process.execute() {
        Ok(exports) => println!("Exported:\n{exports:#}"),
        Err(error) => eprintln!("{error}"),
    }
}
