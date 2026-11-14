// Test binding oddities: multiple vars, objects, arrays
const { a, b, arr: [j, k, ...lm], ...cd } = {
  a: 1,
  b: 2,
  c: 3,
  d: 4,
  arr: [1, 2, 3, 4],
};
const x = 1, y = 2;

export default { a, b, cd, j, k, lm, x, y };
