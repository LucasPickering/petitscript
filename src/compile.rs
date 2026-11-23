//! The compile pipeline turns source code into an executable program.

mod function;
mod parse;

pub use parse::SUPPORTED_EXTENSIONS;

use crate::{
    Source,
    ast::{AstVisitor, Module, Walk},
    compile::function::{CaptureFunctions, LabelFunctions},
    error::ParseErrors,
    source::{SourceId, SourceTable, SpanTable},
};
use std::collections::HashMap;

/// A compiled program
///
/// This is produced by [`Source::parse`] and contains all the static
/// information needed to run a program. It is not itself executable though;
/// that requires a [`World`](crate::World).
///
/// # Introspection
///
/// You can inspect the [abstract syntax tree](crate::ast) of a parsed program
/// with [`Program::root`].
///
/// ```
/// use petitscript::{
///     Program,
///     ast::{IntoExpression, Module, NodeId},
/// };
/// let program = Program::parse("export default 3;").unwrap();
/// // NodeId::with_all_equal is useful for equality assertions on ASTs. Each
/// // AST node gets a unique ID, and the generation order can vary based on
/// // how the AST is constructed. with_all_equal() causes assertions to
/// // ignore node IDs, making the comparison just structural.
/// NodeId::with_all_equal(|| {
///     assert_eq!(
///         program.root(),
///         &Module::new([3.into_expr().export_default()])
///     );
/// });
/// ```
#[derive(Debug)]
pub struct Program {
    /// Compiled source code
    modules: Modules,
    /// Source(s) that were parsed to make this program
    sources: SourceTable,
    /// A debug table of source spans for each node. This will help us produce
    /// good error messages at runtime
    spans: SpanTable,
}

impl Program {
    /// Parse and compile some source code into a [`Program`]
    ///
    /// The returned program can be executed with
    /// [`Engine::instantiate`](crate::Engine::instantiate).
    ///
    /// # Errors
    ///
    /// Return an error if the program is syntactically invalid.
    pub fn parse(source: impl Into<Source>) -> Result<Self, ParseErrors> {
        Ok(Compiler::new(source).parse()?.label().capture().end())
    }

    /// Compile a constructed abstract syntax tree into a program
    ///
    /// This is helpful for environments where an AST is built programatically
    /// rather than by parsing source code. This is infallible
    /// because parsing is the only fallible part of the compilation pipeline.
    pub fn compile(module: Module) -> Self {
        let modules = Modules {
            root: module,
            imported: HashMap::new(),
        };
        Compiler {
            program: ParsedAst {
                modules,
                spans: SpanTable::default(),
            },
            sources: SourceTable::default(), // No source code to map here
        }
        .label()
        .capture()
        .end()
    }

    /// Get the root module of the program
    ///
    /// This is the root of the abstract syntax tree that composes the program.
    pub fn root(&self) -> &Module {
        &self.modules.root
    }

    /// Get the sources parsed to make this program
    pub(crate) fn sources(&self) -> &SourceTable {
        &self.sources
    }

    /// Get the source span table
    pub(crate) fn spans(&self) -> &SpanTable {
        &self.spans
    }

    /// Get a local module by its source ID
    pub(crate) fn module(&self, source: SourceId) -> Option<&Module> {
        self.modules.imported.get(&source)
    }
}

/// All the modules that comprise a program
///
/// All modules are loaded during parsing. After that point, each module is
/// independent for the rest of the compilation pipeline.
#[derive(Debug)]
struct Modules {
    /// Entrypoint module
    root: Module,
    /// Additional local modules, each keyed by its source
    ///
    /// These modules are all imported by `import ... from './local'`. All
    /// imported modules are flattened into this list.
    imported: HashMap<SourceId, Module>,
}

impl Modules {
    /// Apply an AST visitor to each module in the collection
    fn walk_all(&mut self, visitor: &mut dyn AstVisitor) {
        self.root.walk(visitor);
        for module in self.imported.values_mut() {
            module.walk(visitor);
        }
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
    modules: Modules,
    spans: SpanTable,
}

/// AST after function labelling
struct LabelledAst {
    modules: Modules,
    spans: SpanTable,
}

/// AST after function capture
struct CapturedAst {
    modules: Modules,
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
        let (modules, spans) = parse::parse(&mut self.sources)?;
        Ok(Compiler {
            sources: self.sources,
            program: ParsedAst { modules, spans },
        })
    }
}

impl Compiler<ParsedAst> {
    /// Label functions in bound expressions such as `const f = () => {}`,
    ///
    /// This is just for for debuggability and convenience, and has no semantic
    /// impact on the program.
    fn label(self) -> Compiler<LabelledAst> {
        let mut modules = self.program.modules;
        modules.walk_all(&mut LabelFunctions);
        Compiler {
            sources: self.sources,
            program: LabelledAst {
                modules,
                spans: self.program.spans,
            },
        }
    }
}

impl Compiler<LabelledAst> {
    /// Calculate captured variables in each function definition
    fn capture(self) -> Compiler<CapturedAst> {
        let mut modules = self.program.modules;
        modules.walk_all(&mut CaptureFunctions::new());
        Compiler {
            sources: self.sources,
            program: CapturedAst {
                modules,
                spans: self.program.spans,
            },
        }
    }
}

impl Compiler<CapturedAst> {
    fn end(self) -> Program {
        Program {
            modules: self.program.modules,
            sources: self.sources,
            spans: self.program.spans,
        }
    }
}
