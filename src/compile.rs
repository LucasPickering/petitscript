//! The compile pipeline turns source code into an executable program.

mod function;
mod parse;

pub use function::FunctionDefinitionId;
pub use parse::SUPPORTED_EXTENSIONS;

use crate::{
    ast::{source::SourceTable, walk::Walk, Module},
    compile::function::{CaptureFunctions, FunctionTable, LabelFunctions},
    Error, Source,
};

/// Compile source code into an executable [Program]
pub fn compile(source: impl Source) -> Result<Program, Error> {
    let program = Compiler::new(source)
        .parse()?
        .label()
        .capture()
        .lift()
        .end();
    Ok(program)
}

/// An executable program. This is produced by [compile]
#[derive(Debug)]
pub struct Program {
    sources: SourceTable,
    module: Module,
    function_table: FunctionTable,
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

    /// The function table stores the definition of each function, including
    /// its parameters and bodies. This is similar to the .text section of a
    /// binary
    pub fn function_table(&self) -> &FunctionTable {
        &self.function_table
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
struct ParsedAst(Module);

/// AST after function labelling
struct LabelledAst(Module);

/// AST after function capture
struct CapturedAst(Module);

/// A program after function lifting
struct Lifted {
    module: Module,
    function_table: FunctionTable,
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
        let ast = parse::parse(&mut self.sources)?;
        Ok(Compiler {
            sources: self.sources,
            program: ParsedAst(ast),
        })
    }
}

impl Compiler<ParsedAst> {
    /// Label functions in bound expressions such as `const f = () => {}`, for
    /// debuggability and convenience. This has no semantic impact on the
    /// program
    fn label(self) -> Compiler<LabelledAst> {
        let mut ast = self.program.0;
        ast.walk(&mut LabelFunctions);
        Compiler {
            sources: self.sources,
            program: LabelledAst(ast),
        }
    }
}

impl Compiler<LabelledAst> {
    fn capture(self) -> Compiler<CapturedAst> {
        let mut ast = self.program.0;
        ast.walk(&mut CaptureFunctions::new());
        Compiler {
            sources: self.sources,
            program: CapturedAst(ast),
        }
    }
}

impl Compiler<CapturedAst> {
    /// Lift functions into a separate table, replacing their bodies with
    /// references
    fn lift(self) -> Compiler<Lifted> {
        let mut ast = self.program.0;
        let function_table = FunctionTable::lift(&mut ast);
        Compiler {
            sources: self.sources,
            program: Lifted {
                module: ast,
                function_table,
            },
        }
    }
}

impl Compiler<Lifted> {
    fn end(self) -> Program {
        Program {
            sources: self.sources,
            module: self.program.module,
            function_table: self.program.function_table,
        }
    }
}
