#pragma once

#include <stdint.h>
#include <assert.h>
#include <stdlib.h>

// A mapping from arbitrary void* to T* base address lookup, using a radix table.
static constexpr uint64_t shift = 12;
static constexpr uint64_t ind_sz = 1 << shift;
static constexpr uint64_t ind_mask = (ind_sz - 1);

typedef struct  alloc_table {
  uint64_t min;
  uint64_t max;

  void*** map[ind_sz]; // Types are awesome.

} alloc_table;

bool alloc_table_lookup(alloc_table *table, void *p, void**slab);
void alloc_table_set_range(alloc_table *table, void *val, void *p, uint64_t range);


