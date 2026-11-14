import { add } from "math";

// Make sure the imported name is captured
const add2 = (a, b) => add(a, b);

const call = (f, a, b) => f(a, b);

export default add(1, 2) + add2(1, 2) + call(add, 1, 2);
