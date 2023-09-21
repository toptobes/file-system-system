export const a = { a: 3, b: 3, test };

console.log(a.test());

function test() {
  return add(2, 3);
}

function add(a, b) {
  return a + b;
}

export function fibonacci(n) {
  if (n <= 1) {
    return n;
  } else {
    return fibonacci(n - 1) + fibonacci(n - 2);
  }
}

export function* fibonacciGen() {
  let [a, b] = [0, 1];

  while (true) {
    yield a;
    [a, b] = [b, a + b];
  }
}
