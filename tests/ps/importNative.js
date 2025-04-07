import { add } from "math";

function add2(a, b) {
  // Make sure the imported name is captured
  return add(a, b);
}

function call(f, ...args) {
  return f(...args);
}

export default add(1, 2) + add2(1, 2) + call(add, 1, 2);
