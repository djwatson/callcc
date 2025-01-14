#include "types.c"

static gc_obj fib(gc_obj n) {
  double nd = to_double(n);
  if (nd < 2.0) {
    return double_to_gc(nd);
  }
  auto f1 = fib(double_to_gc(nd - 1.0));
  auto f2 = fib(double_to_gc(nd - 2.0));
  return double_to_gc(to_double(f1) + to_double(f2));
}

int main() {
  for(uint64_t i = 0; i < 10; i++) {
    gc_obj res = fib(double_to_gc(35.0));
    printf("Res %f\n", to_double(res));
  }
  return 0;
}
