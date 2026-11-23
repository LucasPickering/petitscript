use annotate_snippets::Renderer;
use clap::Parser;
use petitscript::{Engine, Error, Source};
use std::{path::PathBuf, process::ExitCode};

#[derive(Debug, Parser)]
struct Args {
    /// Path to a PetitScript file
    path: PathBuf,
    /// Parse and print the AST instead of executing
    #[arg(long)]
    parse: bool,
}

fn main() -> ExitCode {
    let args = Args::parse();

    let renderer = Renderer::styled();
    match run(args) {
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

fn run(args: Args) -> Result<(), petitscript::Error> {
    let source = Source::file(&args.path)?;
    let program = source.parse()?;

    if args.parse {
        println!("{:#?}", program.root());
        Ok(())
    } else {
        let engine = Engine::default();
        let world = engine.instantiate(program);
        let exports = world.run()?;
        println!("Exported:\n{exports:#}");
        Ok(())
    }
}
