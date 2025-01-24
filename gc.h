#pragma once

#include <stdint.h>

void gc_init();
void* rcimmix_alloc(uint64_t sz);
void gc_add_root(uint64_t *rootp);
void gc_pop_root(uint64_t const *rootp);
uint64_t* gc_get_stack_top();

