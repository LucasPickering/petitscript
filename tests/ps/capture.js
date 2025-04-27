const x = 2;

const f = () => {
  const y = 3;
  const z = 10; // Should *not* get captured
  const g = (z) => x + y + z;
  const x = 10; // Should *not* get captured
  return g;
};

const g = f();
export default g(4); // 2 + 3 + 4 = 9
