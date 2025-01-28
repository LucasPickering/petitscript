mod eval;
mod exec;
pub mod module;
mod scope;

use crate::{
    error::Result,
    runtime::{eval::Evaluate, exec::Execute, module::Module, scope::Scope},
    value::Value,
};
use boa_ast::declaration::{Binding, VariableList};
use boa_interner::{Interner, Sym};

pub struct Runtime {
    source: boa_ast::Module,
    state: RuntimeState,
}

impl Runtime {
    /// TODO
    pub fn new(interner: Interner, source: boa_ast::Module) -> Runtime {
        Self {
            source,
            state: RuntimeState::new(interner),
        }
    }

    /// TODO
    pub fn load(&mut self) -> Result<Module> {
        let state = &mut self.state;
        self.source.items().items().exec(state)?;
        let module = Module {
            default: state.export_default.take(),
            named: state
                .export_names
                .drain(..)
                .map(|name| {
                    let value = state.scope.get(&name)?;
                    Ok((name, value))
                })
                .collect::<Result<_>>()?,
        };
        Ok(module)
    }
}

/// TODO
#[derive(Debug)]
struct RuntimeState {
    scope: Scope,
    stack: Vec<StackFrame>,
    export_default: Option<Value>,
    /// TODO
    export_names: Vec<String>,
    interner: Interner,
}

impl RuntimeState {
    fn new(interner: Interner) -> Self {
        Self {
            scope: Scope::new(),
            stack: Vec::new(),
            export_default: None,
            export_names: Vec::new(),
            interner,
        }
    }

    /// TODO
    fn export(&mut self, name: String) -> Result<()> {
        // TODO error on duplicate export
        self.export_names.push(name);
        Ok(())
    }

    /// TODO
    fn export_default(&mut self, value: Value) -> Result<()> {
        // TODO error if something is already exported
        self.export_default = Some(value);
        Ok(())
    }

    /// Resolve a Boa interner symbol into the corresponding string
    fn resolve_sym(&self, symbol: Sym) -> &str {
        // This should only fail if the ID isn't valid UTF-8, or a bug
        // somewhere. It's not worth propagating the potential error everywhere,
        // since we plan to get rid of this eventually when replacing boa with
        // our own parser.
        self.interner.resolve_expect(symbol).utf8().expect("TODO")
    }

    /// Call a function and return its return value
    fn call(&mut self, function: &Value, args: Vec<Value>) -> Result<Value> {
        let Value::Function(function) = function else {
            todo!("error")
        };

        // Create a new stack frame and execute the fn body. Function is
        // responsible for setting the return value in its frame
        self.stack.push(StackFrame::new(self.scope.child()));
        // TODO add args to scope
        function.body.statement_list().statements().exec(self)?;
        let frame = self.stack.pop().expect("Just pushed to stack");
        Ok(frame.return_value)
    }
}

/// TODO
#[derive(Debug)]
struct StackFrame {
    scope: Scope,
    /// Slot where the return value is stored imperatively. This is EAX!
    /// Default return value is `undefined`.
    return_value: Value,
}

impl StackFrame {
    fn new(global_scope: Scope) -> Self {
        Self {
            scope: global_scope.child(),
            return_value: Value::Undefined,
        }
    }
}
