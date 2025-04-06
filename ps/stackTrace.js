function f() {
  return x;
}

function g() {
  return f() + 1;
}

function h() {
  return g() + 1;
}

export default h();
