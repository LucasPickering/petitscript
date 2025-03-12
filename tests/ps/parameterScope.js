// Each parameter should be able to reference previous parameters in its default
// expression

function f(a, b = a + 1, c = a + b + 1) {
  return a + b + c;
}

export default f(1); // Should be 7
