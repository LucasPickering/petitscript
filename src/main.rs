use petit_js::Runtime;
use std::env;

fn main() {
    let path = env::args().nth(1).expect("Expected path");
    let runtime = Runtime::new();
    match runtime.load(&path) {
        Ok(module) => println!("Exported:\n{module:#}"),
        Err(error) => eprintln!("Error: {error}"),
    }
}
