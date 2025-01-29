let x = 3;
const y = 4;

function f(a) {
  x = a;
}

function g() {
  let y = 0;
  y = 10;
}

function nested() {
  let w = 3;
  return () => {
    w = 4;
    return w;
  };
}

console.log(x, y);
f(5);
g();
console.log(x, y);
console.log("should be 4:", nested()());

if (true) {
  const ifScope = 3;
  console.log("should be 3:", ifScope);
}
console.log("should error!");
console.log(ifScope);
