use std::env;

fn main() -> petit_js::Result<()> {
    let path = env::args().nth(1).expect("Expected path");
    let module = petit_js::load(&path)?;
    println!("{module}");
    Ok(())
}
