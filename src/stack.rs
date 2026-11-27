//! Stack machine interpreter

use crate::{
    Exports, Program, Value,
    ast::{BinaryOperator, Identifier},
    compile::stack::{Compiler, Instruction},
    error::RuntimeErrorKind,
    value::{Array, Object},
};
use std::vec::Drain;

/// TODO
pub fn run(program: Program) -> Result<Exports, RuntimeErrorKind> {
    let instructions = Compiler::compile(program).expect("TODO");
    let mut stack = StackMachine::default();
    for instruction in instructions {
        stack.execute(instruction)?;
    }

    /*
    TODO keep this? needs to be adapted for locals
    // Every STATEMENT execution should be zero-sum on the stack, so it must be
    // empty by the end. Anything else indicates a bug.
    assert_eq!(
        stack.stack.as_slice(),
        [],
        "Stack must be empty after execution"
    );
    */

    Ok(stack.exports)
}

/// A stack-based virtual machine for executing a compiled PetitScript program
///
/// This is loosely modeled on the [bytecode virtual machine from Crafting
/// Interpreters](https://craftinginterpreters.com/a-bytecode-virtual-machine.html).
/// Rather than representing instructions as encoded bytes, this uses the
/// [`Instruction`] enum. This works because compiled programs never need to be
/// represented on disk. Programs are always parsed and compiled JIT.
#[derive(Debug, Default)]
struct StackMachine {
    /// Working stack of values
    ///
    /// The top value of this is called the "active" value. This stores both
    /// temporaries (for expression evaluation) as well as locals. This is akin
    /// to The Stack (of Stack & Heap fame) for native programs.
    stack: Vec<Value>,
    /// Exported values
    ///
    /// This is built procedurally as we execute and is the final result of
    /// execution.
    exports: Exports,
}

impl StackMachine {
    /// Execute one instruction
    fn execute(
        &mut self,
        instruction: Instruction,
    ) -> Result<(), RuntimeErrorKind> {
        match instruction {
            Instruction::BinaryOperation(operator) => {
                self.binary_operator(operator);
            }
            Instruction::ExportDefault => self.export_default()?,
            Instruction::ExportNamed { name, slot } => {
                self.export_named(name, slot)?;
            }
            Instruction::Pop => {
                self.pop();
            }
            Instruction::Push(value) => self.push(value),
            Instruction::PushArray { len, spreads } => {
                // Elements are sliced off in the order they were pushed, so we
                // don't need to reverse
                let mut values = self.pop_n(len);
                let mut array = Array::default();
                for i in 0..len {
                    let value = values.next();
                    // These are optimized to avoid allocations where possible
                    if spreads.contains(&i) {
                        let new = value.try_into_array()?;
                        array = array.concat(new);
                    } else {
                        array = array.push(value);
                    }
                }
                drop(values);
                self.push(array.into());
            }
            Instruction::PushObject {
                len_properties,
                len_values,
                spreads,
            } => {
                let mut values = self.pop_n(len_values);
                let mut object = Object::default();
                for i in 0..len_properties {
                    if spreads.contains(&i) {
                        let value = values.next();
                        let new_object = value.try_into_object()?;
                        object = object.insert_all(new_object);
                    } else {
                        let property = values.next();
                        let value = values.next();
                        // Make sure *not* to use the Display impl
                        object = object.insert(property.to_string(), value);
                    }
                }
                drop(values);
                self.push(object.into());
            }
            Instruction::PushReference { slot } => {
                self.push(self.stack[slot].clone());
            }
        }
        Ok(())
    }

    /// Pop the top value and export it as the default
    fn export_default(&mut self) -> Result<(), RuntimeErrorKind> {
        let value = self.pop();
        self.exports.export_default(value)
    }

    /// Export a named value
    fn export_named(
        &mut self,
        name: Identifier,
        slot: usize,
    ) -> Result<(), RuntimeErrorKind> {
        let value = self.stack[slot].clone();
        self.exports.export(name.into(), value)
    }

    /// Apply a binary operator
    fn binary_operator(&mut self, operator: BinaryOperator) {
        // RHS is on top!!
        let rhs = self.pop();
        let lhs = self.pop();
        let output = match operator {
            BinaryOperator::Add => lhs + rhs,
            BinaryOperator::Sub => lhs - rhs,
            BinaryOperator::Mul => lhs * rhs,
            BinaryOperator::Div => lhs / rhs,
            BinaryOperator::Mod => lhs % rhs,
            BinaryOperator::LogicalAnd => todo!(),
            BinaryOperator::LogicalOr => todo!(),
            BinaryOperator::Equal => todo!(),
            BinaryOperator::NotEqual => todo!(),
            BinaryOperator::LessThan => todo!(),
            BinaryOperator::GreaterThan => todo!(),
            BinaryOperator::LessThanEqual => todo!(),
            BinaryOperator::GreaterThanEqual => todo!(),
            BinaryOperator::NullishCoalesce => todo!(),
        };
        self.push(output);
    }

    /// TODO
    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    /// TODO
    fn pop(&mut self) -> Value {
        self.stack.pop().expect("TODO")
    }

    /// Pop multiple items off the stack
    ///
    /// Values are returned in an iterator with the top element of the stack at
    /// the *end* of the iterator.
    ///
    /// ## Panics
    ///
    /// Panics if there's `< n` values on the stack
    fn pop_n(&mut self, n: usize) -> StackSlice<'_> {
        let len = self.stack.len();
        assert!(n <= len, "TODO");
        StackSlice(self.stack.drain((len - n)..))
    }
}

/// A slice of values drained off the end of the stack
///
/// Use this for operations that consume a fixed amount of values. Panics if
/// too many or too few values are consumed.
struct StackSlice<'a>(Drain<'a, Value>);

impl StackSlice<'_> {
    /// Get the next value off the front of the slice
    ///
    /// This returns values **in the order in which they were pushed**.
    ///
    /// ## Panics
    ///
    /// Panics if the slice is empty
    fn next(&mut self) -> Value {
        self.0.next().expect("TODO")
    }
}

impl Drop for StackSlice<'_> {
    fn drop(&mut self) {
        assert!(self.0.next().is_none(), "TODO");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    /// Test expression evaluation
    #[test_case("true", true; "literal boolean")]
    #[test_case("3", 3; "literal number")]
    #[test_case("[1, ...[2, 3]]", [1, 2, 3]; "literal array")]
    #[test_case(
        "({ a: 1, b: [2, 3], ...{ c: 'asdf' } })",
        [("a", Value::from(1)), ("b", [2, 3].into()), ("c", "asdf".into())];
        "literal object"
    )]
    #[test_case("1 + 2", 3; "add")]
    #[test_case("1 - 2", -1; "subtract")]
    #[test_case("1 * 2", 2; "multiply")]
    #[test_case("4 / 2", 2; "divide")]
    #[test_case("15 % 4", 3; "modulus")]
    fn expression(expression_source: &str, expected: impl Into<Value>) {
        let program =
            Program::parse(format!("export default {expression_source};"))
                .unwrap();
        let exports = run(program).unwrap();
        assert_eq!(exports.default.unwrap(), expected.into());
    }

    /// Test statement execution evaluation
    #[test_case("const x = 3; export default x;", 3; "declaration")]
    fn statement(source: &str, expected: impl Into<Value>) {
        let program = Program::parse(source).unwrap();
        let exports = run(program).unwrap();
        assert_eq!(exports.default.unwrap(), expected.into());
    }

    #[test]
    fn export_named() {
        let source = "
export const x = 3, y = 4;
export const z = x * y;";
        let program = Program::parse(source).unwrap();
        let exports = run(program).unwrap();
        assert_eq!(exports, Exports::named([("x", 3), ("y", 4), ("z", 12)]));
    }

    /// Test compilation error cases
    #[test_case(
        "const x = 3; const x = 3;", "`x` is already declared in this scope";
        "binding duplicate"
    )]
    fn compile_errors(source: &str, expected_error: &str) {
        let program = Program::parse(source).unwrap();
        let errors = Compiler::compile(program).unwrap_err();
        assert_eq!(errors.errors.len(), 1, "Expected exactly 1 error");
        assert_eq!(errors.errors[0].to_string(), expected_error);
    }
}
