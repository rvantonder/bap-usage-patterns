#include <stdio.h>

int g(int i);

int h(int i) {
  i += 1;
  g(i);
}

int g(int i) {
  if (i > 10) {
    return i;
  }
  i += 1;
  h(i);
}

int f(int i) {
  i += 1;
  g(i);
}

int main() {
  int i = 0;
  int result;

  result = f(i);
  printf("Res: %d\n", result);
}
