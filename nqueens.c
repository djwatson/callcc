#include "stdio.h"
#include "stdint.h"
#include "stdbool.h"
#include "stdlib.h"
#include "gc.h"

struct cell {
  uint64_t car;
  uint64_t cdr;
};

bool trace = false;

uint64_t get_car(uint64_t a) {
  struct cell* c = (struct cell*)a;
  return c->car;
}
uint64_t get_cdr(uint64_t a) {
  struct cell* c = (struct cell*)a;
  return c->cdr;
}

uint64_t cons(uint64_t a, uint64_t b) {
  struct cell* c = rcimmix_alloc(sizeof(struct cell));

  //struct cell* c = GC_MALLOC(sizeof(struct cell));
  c->car = a;
  c->cdr = b;
  return (uint64_t)c;
}

uint64_t append(uint64_t a, uint64_t b) {
  if (a == 0) {
    return b;
  }
  return cons(get_car(a), append(get_cdr(a), b));
}

uint64_t one_to_n(uint64_t n) {
  uint64_t ret =0;
  for(int i = n; i != 0; i--) {
    ret = cons(i, ret);
  }
  return ret;
}

bool ok(uint64_t row, uint64_t dist, uint64_t placed) {
  if (placed == 0) {
    return true;
  }
  return (get_car(placed) != (row + dist)) &&
    (get_car(placed) != (row - dist)) &&
    ok(row, dist + 1, get_cdr(placed));
}

int my_try(uint64_t x, uint64_t y, uint64_t z) {
  if (x == 0) {
    if (y == 0) {
      if (trace) {
	printf("%li", z);
      }
      return 1;
    } else {
      return 0;
    }
  } else {
    uint64_t cur = 0;
    if (ok(get_car(x), 1, z)) {
      cur = my_try(append(get_cdr(x), y), 0, cons(get_car(x), z));
    }
    return cur + my_try(get_cdr(x), cons(get_car(x), y), z);
  }
};

int nqueens(int n) {
  return my_try(one_to_n(n), 0, 0);
}

int main() {
  gc_init();
  for(uint64_t i = 0; i < 10; i++) {
    printf("%i\n", nqueens(13));
  }
  return 0;
}
