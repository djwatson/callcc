#pragma once

#include <stdint.h>

void gc_init(void* stacktop);
void gc_add_root(uint64_t *rootp);
void gc_pop_root(uint64_t const *rootp);
uint64_t* gc_get_stack_top();
bool gc_is_small(uint64_t sz);
void gc_log(uint64_t a);
void gc_log_fast(uint64_t a);
void gc_log_with_slab(uint64_t a, void* sp);
typedef struct alloc_result {
  void* p;
  void* slab;
} alloc_result;
alloc_result rcimmix_alloc_with_slab(uint64_t sz);
void* rcimmix_alloc(uint64_t sz);
