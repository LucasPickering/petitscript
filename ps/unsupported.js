// A bunch of JS features that aren't supported in PS.
// The parser should produce errors for all of these at once.

import "./common.js"; // Bare imports

// let/var
let x = 3;
var x = 3;

// Async
async function f() {}

// Array elision
const a = [1, , 3];

for (const x in o) {
  // The outer node is invalid, so the inner node does *not* produce an error
  let y = 3;
}

// Classes
class C {}
