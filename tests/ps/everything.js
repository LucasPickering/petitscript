// A bit of every supported syntax. This will parse but not run.
import f from "native";
import def from "./common.js";
import { add } from "./common.js";
import def, { add } from "./common.js";

// Assignments
const x = 3;
const y = 4, z = 10;
const { o: { a, b: b1, c: [c1, c2, c3] } } = null;

null;
undefined;
null ?? undefined;
typeof null;

// Booleans
!true;
true && false;
true || false;
true === false;
true !== false;

// Numbers
-3;
-3.14;
177777e17;
940.333e-10;
-9.33;
-12.;
10001;
1.23 < -4.56;
1.23 > -4.56;
1.23 <= -4.56;
1.23 >= -4.56;
1.23 + 4.56;
1.23 - 4.56;
1.23 * 4.56;
1.23 / 4.56;
1.23 % 4.56;

// Strings
"''";
"\"";
// dprint-ignore
'"';
// dprint-ignore
'\'';

// Templates
``;
`\``;
`${expr}`;
`${`${`${3}`}`}`;
`hello ${world}`;

// Arrays
[[[[[]]]], 99, { a: 1 }];

// Objects
({
  a: 1,
  o: {
    [f()]: -3.,
    [a + x]: false,
    ["::"]: "::",
    arr: [[], []],
  },
});

// Functions
const fEmpty = () => {};
const fExpr = () => ({ a: 1, arr: [1, 3, 4] });
const fBlock = () => {
  const w = 10;
  return w + 7;
};
const fParams = (
  x,
  y = 3,
  f = fEmpty,
  { a, b },
  [a, b],
  gEmpty = () => {},
  gExpr = () => 3,
  gBlock = () => {
    const x = 3;
  },
) => {
  f(gEmpty(gExpr(gBlock())));
};
function fFunction() {
  return -99;
}

export const x = 3, y = 4;
export default { a: 1 };
