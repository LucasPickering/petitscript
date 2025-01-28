let x = 3;
const y = 4;

function f() {
  x = 5;
}

function g() {
  let y = 0;
  y = 10;
}

console.log(x, y);
f();
g();
console.log(x, y);
