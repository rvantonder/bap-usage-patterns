// fib.c
// calculate the 10th fibonacci number (55)
int main() {
  int a = 1, b = 1, n = 10;
  for (int i = 3; i <= n; i++) {
      int c = a + b;
      a = b;
      b = c;
  }
  return b;
}
