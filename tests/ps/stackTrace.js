import { error } from "./common.js";

function f() {
  return error();
}

function g() {
  return f() + 1;
}

export default g();
