const y = 4;

function shadow(a) {
  const y = 0;
}

function nested() {
  const w = 3;
  return () => w;
}

console.log(x, y);
shadow(5);
console.log(x, y);
console.log("should be 4:", nested()());

if (true) {
  const ifScope = 3;
  console.log("should be 3:", ifScope);
}
console.log("should error!");
console.log(ifScope);
