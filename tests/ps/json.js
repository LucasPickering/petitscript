const o = {
  a: 1,
  b: 2,
  friends: ["ken", 42.0, false],
};

const s = JSON.stringify(o);
console.log(s);
const o2 = JSON.parse(s);

console.log(typeof o, typeof o2);
export default o === o2;
