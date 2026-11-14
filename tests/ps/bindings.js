// Test binding oddities: multiple vars, objects, arrays
const { a, b, arr: [j, k, ...lm], ...cd } = {
  a: 1,
  b: 2,
  c: 3,
  d: 4,
  arr: [1, 2, 3, 4],
};
const x = 1, y = 2;
// Defaults
const [arrDef = 7] = [];
const { objDef = 7 } = {};
const [skip1, , skip3] = [1, 2, 3];

export default {
  a,
  b,
  cd,
  j,
  k,
  lm,
  x,
  y,
  arrDef,
  objDef,
  skip: [skip1, skip3],
};
