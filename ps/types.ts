type str = string;
function f<T>(arg: T): T {
  return arg;
}

export const s = f<str>("asdf");
