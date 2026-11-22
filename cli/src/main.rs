use annotate_snippets::Renderer;
use petitscript::{Engine, Error, Source};
use std::{env, path::Path, process::ExitCode};

fn main() -> ExitCode {
    let path = env::args().nth(1).expect("Expected path");

    let renderer = Renderer::styled();
    match run(Path::new(&path)) {
        Ok(()) => ExitCode::SUCCESS,
        Err(Error::Parse(parse_errors)) => {
            eprintln!("{}", renderer.render(&parse_errors.to_report()));
            ExitCode::FAILURE
        }
        Err(Error::Runtime(error)) => {
            eprintln!("{}", renderer.render(&[error.to_report()]));
            ExitCode::FAILURE
        }
        Err(error) => {
            eprintln!("{error}");
            ExitCode::FAILURE
        }
    }
}

fn run(path: &Path) -> Result<(), petitscript::Error> {
    let engine = Engine::default();
    let world = engine.compile(Source::file(path)?)?;

    let exports = world.execute()?;
    println!("Exported:\n{exports:#}");
    let json = serde_json::to_string_pretty(&exports.default).unwrap();
    println!("Default (JSON): {json}");
    Ok(())
}
