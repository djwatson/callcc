#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "gc.h"

typedef struct cellp{
  struct cellp* car;
  struct cellp* cdr;
}cell;

cell* createn(uint64_t n) {
  cell* ret = NULL;
  for(uint64_t i = 0; i < n; i++) {
    cell* n = malloc(sizeof(cell));
    n->car = NULL;
    n->cdr = ret;
    ret = n;
  }
  return ret;
}

cell* iterative(cell* p) {
  cell* ret = NULL;
  while(p) {
    cell* n = rcimmix_alloc(sizeof(cell));
    n->car = p->car;
    n->cdr = ret;
    p = p->cdr->cdr;
    ret = n;
  }
  return ret;
}

int length(cell* p) {
  int ret = 0;
  while(p) {
    ret++;
    p = p->cdr;
  }
  return ret;
}

int main() {
  gc_init();
  cell* p  = createn(1000);
  for(uint64_t i =0; i < 1000000; i++) {
    cell* res = iterative(p);
  }
}
