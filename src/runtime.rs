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

    // TODO
    fn declare_all(
        &mut self,
        variables: &VariableList,
        mutable: bool,
    ) -> Result<Vec<String>> {
        let mut declared = Vec::with_capacity(variables.as_ref().len());
        for variable in variables.as_ref() {
            let value = variable
                .init()
                .map(|expr| expr.eval(self))
                .transpose()?
                .unwrap_or_default();
            match variable.binding() {
                Binding::Identifier(identifier) => {
                    let name = self.resolve_sym(identifier.sym())?.to_owned();
                    self.scope.declare(name.clone(), value, mutable);
                    declared.push(name);
                }
                Binding::Pattern(pattern) => todo!(),
            }
        }
        Ok(declared)
    }

    /// TODO
    fn resolve_sym(&self, symbol: Sym) -> Result<&str> {
        self.interner
            .resolve_expect(symbol)
            .utf8()
            .ok_or_else(|| todo!())
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
