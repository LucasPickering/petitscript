function f() {
  return x + 1;
}

function g() {
  return f();
}

function h() {
  return g();
}

h();
