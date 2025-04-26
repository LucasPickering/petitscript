//! The compile pipeline turns source code into an executable program.

mod function;
mod parse;

pub use parse::SUPPORTED_EXTENSIONS;

use crate::{
    ast::{Module, Node, Walk},
    compile::function::{CaptureFunctions, LabelFunctions},
    source::{SourceTable, SpanTable},
    Error, Source,
};

/// Parse some source code into an AST
pub fn parse(source: impl Source) -> Result<Node<Module>, Error> {
    Ok(Compiler::new(source).parse()?.program.module)
}

/// Parse and compile source code into an executable [Program]
pub fn compile(source: impl Source) -> Result<Program, Error> {
    let program = Compiler::new(source).parse()?.label().capture().end();
    Ok(program)
}

/// Compile a prebuilt AST into an executable [Program]. Helpful for tests and
/// other environments where an AST is built programatically rather than by
/// parsing source code.
pub fn compile_ast(module: Node<Module>) -> Result<Program, Error> {
    let program = Compiler {
        program: ParsedAst {
            module,
            spans: SpanTable::default(),
        },
        sources: SourceTable::default(), // No source code to map here
    }
    .label()
    .capture()
    .end();
    Ok(program)
}

/// An executable program. This is produced by [compile]
#[derive(Debug)]
pub struct Program {
    sources: SourceTable,
    module: Node<Module>,
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
    module: Node<Module>,
    spans: SpanTable,
}

/// AST after function labelling
struct LabelledAst {
    module: Node<Module>,
    spans: SpanTable,
}

/// AST after function capture
struct CapturedAst {
    module: Node<Module>,
    spans: SpanTable,
}

impl Compiler<()> {
    fn new(source: impl Source) -> Self {
        let mut sources = SourceTable::default();
        sources.insert(source);
        Self {
            sources,
            program: (),
        }
    }

    /// Parse source code into an AST
    fn parse(mut self) -> Result<Compiler<ParsedAst>, Error> {
        let (module, spans) = parse::parse(&mut self.sources)?;
        Ok(Compiler {
            sources: self.sources,
            program: ParsedAst { module, spans },
        })
    }
}

impl Compiler<ParsedAst> {
    /// Label functions in bound expressions such as `const f = () => {}`, for
    /// debuggability and convenience. This has no semantic impact on the
    /// program
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
