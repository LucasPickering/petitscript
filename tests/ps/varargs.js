// Test varargs in function parameters (decl position) and arguments (expr position)
function call(f, ...args) {
  f(...args);
}

function add(a, b) {
  return a + b;
}

export default call(add, ...[1, 2]); // 1 + 2 = 3
