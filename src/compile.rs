//! The compile pipeline turns source code into an executable program.

mod function;
mod parse;

pub use function::FunctionDefinitionId;

use crate::{
    ast::{walk::Walk, Ast},
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
    source: Box<dyn Source>,
    ast: Ast,
    function_table: FunctionTable,
}

impl Program {
    /// TODO
    pub fn source(&self) -> &dyn Source {
        &*self.source
    }

    /// TODO
    pub fn ast(&self) -> &Ast {
        &self.ast
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
    source: Box<dyn Source>,
    program: T,
}

/// AST that has been parsed, but had no transformations applied
struct ParsedAst(Ast);

/// AST after function labelling
struct LabelledAst(Ast);

/// AST after function capture
struct CapturedAst(Ast);

/// A program after function lifting
struct Lifted {
    ast: Ast,
    function_table: FunctionTable,
}

impl Compiler<()> {
    fn new(source: impl Source) -> Self {
        Self {
            source: Box::new(source),
            program: (),
        }
    }

    /// Parse source code into an AST
    fn parse(self) -> Result<Compiler<ParsedAst>, Error> {
        let ast = parse::parse(&*self.source)?;
        Ok(Compiler {
            source: self.source,
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
            source: self.source,
            program: LabelledAst(ast),
        }
    }
}

impl Compiler<LabelledAst> {
    fn capture(self) -> Compiler<CapturedAst> {
        let mut ast = self.program.0;
        ast.walk(&mut CaptureFunctions::new());
        Compiler {
            source: self.source,
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
            source: self.source,
            program: Lifted {
                ast,
                function_table,
            },
        }
    }
}

impl Compiler<Lifted> {
    fn end(self) -> Program {
        Program {
            source: self.source,
            ast: self.program.ast,
            function_table: self.program.function_table,
        }
    }
}
