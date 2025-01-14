#include <stdint.h>

#include "types.c"
#include "gc.h"

gc_obj create_x(gc_obj n) {
  vector_s *result = rcimmix_alloc(sizeof(vector_s) + sizeof(gc_obj)*to_fixnum(n));
  result->len = n;
  for(int64_t i = 0; i < to_fixnum(n); i++) {
    result->v[i] = tag_fixnum(i);
  }
  return tag_vector(result);
}
gc_obj create_y(gc_obj x) {
  auto n = to_vector(x)->len;
  vector_s* result = rcimmix_alloc(sizeof(vector_s) + sizeof(gc_obj)*to_fixnum(n));
  result->len = n;
  for(int64_t i = to_fixnum(n)-1; i >= 0; i--) {
    result->v[i] = to_vector(x)->v[i];
  }
  return tag_vector(result);
}
gc_obj my_try(gc_obj n) {
  auto res = create_y(create_x(n));
  auto v = to_vector(res);
  return v->len;
}

gc_obj go(gc_obj m, gc_obj n) {
  gc_obj res;
  for(uint64_t i = 0; i < to_fixnum(m); i++) {
    res = my_try(n);
  }
  return res;
}

int main() {
  gc_init();
  auto res = go(tag_fixnum(500), tag_fixnum(1000000));
  printf("Res is %li\n", to_fixnum(res));
  return 0;
}
