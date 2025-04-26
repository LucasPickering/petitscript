use petitscript::Engine;
use std::{env, path::PathBuf};

fn main() {
    let path = env::args().nth(1).expect("Expected path");
    let engine = Engine::default();
    let process = match engine.compile(PathBuf::from(path)) {
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
