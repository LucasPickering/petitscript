async function f() {
  return 3;
}

const { x = await f() } = {};
console.log(x);
