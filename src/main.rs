use std::env;

fn main() {
    let path = env::args().nth(1).expect("Expected path");
    match petit_js::load(&path) {
        Ok(module) => println!("{module:#}"),
        Err(error) => println!("{error}"),
    }
}
