// Direct and mutual recursion

const f = (n) => {
  if (n <= 1) {
    return n;
  }
  return f(n - 1);
};

const g1 = (n) => {
  if (n <= 1) {
    return n;
  }
  return g2(n);
};
const g2 = (n) => {
  return g1(n - 1);
};

export default { direct: f(10), mutual: g1(10) };
