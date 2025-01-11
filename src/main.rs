use std::env;

fn main() -> petit_js::Result<()> {
    let path = env::args().nth(1).expect("Expected path");
    petit_js::run(&path)
}
