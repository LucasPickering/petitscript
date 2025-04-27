import { add } from "math";

// Make sure the imported name is captured
const add2 = (a, b) => add(a, b);

const call = (f, ...args) => f(...args);

export default add(1, 2) + add2(1, 2) + call(add, 1, 2);
