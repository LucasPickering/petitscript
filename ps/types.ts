type T = string;
function f<T>(arg: T): T {
  return arg;
}

f("asdf");
