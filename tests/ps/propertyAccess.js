const o = { a: 1, b: 2, [":::"]: 3 };

export default {
  id: o.a,
  opt: o?.b,
  expr: o[":::"],
  missing: o?.c?.d, // undefined
};
