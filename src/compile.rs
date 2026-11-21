//! The compile pipeline turns source code into an executable program.

mod function;
mod parse;

pub use parse::SUPPORTED_EXTENSIONS;

use crate::{
    Source,
    ast::{Module, Walk},
    compile::function::{CaptureFunctions, LabelFunctions},
    error::ParseErrors,
    source::{SourceTable, SpanTable},
};

/// Parse some source code into an AST
pub fn parse(source: impl Into<Source>) -> Result<Module, ParseErrors> {
    Ok(Compiler::new(source).parse()?.program.module)
}

/// Parse and compile source code into an executable [`Program`]
pub fn compile(source: impl Into<Source>) -> Result<Program, ParseErrors> {
    let program = Compiler::new(source).parse()?.label().capture().end();
    Ok(program)
}

/// Compile a prebuilt AST into an executable [`Program`]
///
/// Helpful for tests and other environments where an AST is built
/// programatically rather than by parsing source code. Compilation is
/// infallible.
pub fn compile_ast(module: Module) -> Program {
    Compiler {
        program: ParsedAst {
            module,
            spans: SpanTable::default(),
        },
        sources: SourceTable::default(), // No source code to map here
    }
    .label()
    .capture()
    .end()
}

/// A compiled program
///
/// This is produced by [`compile`] and contains all the static information
/// needed to run a program. It is not itself executable though; that requires
/// a [`Process`].
#[derive(Debug)]
pub struct Program {
    sources: SourceTable,
    module: Module,
    /// A debug table of source spans for each node. This will help us produce
    /// good error messages at runtime
    spans: SpanTable,
}

impl Program {
    /// TODO
    pub fn sources(&self) -> &SourceTable {
        &self.sources
    }

    /// TODO
    pub fn module(&self) -> &Module {
        &self.module
    }

    /// TODO
    pub fn spans(&self) -> &SpanTable {
        &self.spans
    }
}

/// A wrapper for a partially compiled program. The generic param is the program
/// type. This enforces that compiler pipeline steps are called in a particular
/// order. Each step of the pipeline should emit a unique type using a typestate
/// wrapper.
struct Compiler<T> {
    sources: SourceTable,
    program: T,
}

/// AST that has been parsed, but had no transformations applied
struct ParsedAst {
    module: Module,
    spans: SpanTable,
}

/// AST after function labelling
struct LabelledAst {
    module: Module,
    spans: SpanTable,
}

/// AST after function capture
struct CapturedAst {
    module: Module,
    spans: SpanTable,
}

impl Compiler<()> {
    fn new(source: impl Into<Source>) -> Self {
        let mut sources = SourceTable::default();
        sources.insert(source.into());
        Self {
            sources,
            program: (),
        }
    }

    /// Parse source code into an AST
    fn parse(mut self) -> Result<Compiler<ParsedAst>, ParseErrors> {
        let (module, spans) = parse::parse(&mut self.sources)?;
        Ok(Compiler {
            sources: self.sources,
            program: ParsedAst { module, spans },
        })
    }
}

impl Compiler<ParsedAst> {
    /// Label functions in bound expressions such as `const f = () => {}`,
    ///
    /// This is just for for debuggability and convenience, and has no semantic
    /// impact on the program.
    fn label(self) -> Compiler<LabelledAst> {
        let mut module = self.program.module;
        module.walk(&mut LabelFunctions);
        Compiler {
            sources: self.sources,
            program: LabelledAst {
                module,
                spans: self.program.spans,
            },
        }
    }
}

impl Compiler<LabelledAst> {
    /// Calculate captured variables in each function definition
    fn capture(self) -> Compiler<CapturedAst> {
        let mut module = self.program.module;
        module.walk(&mut CaptureFunctions::new());
        Compiler {
            sources: self.sources,
            program: CapturedAst {
                module,
                spans: self.program.spans,
            },
        }
    }
}

impl Compiler<CapturedAst> {
    fn end(self) -> Program {
        Program {
            sources: self.sources,
            module: self.program.module,
            spans: self.program.spans,
        }
    }
}
