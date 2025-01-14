#include "alloc_table.h"

static void set(alloc_table *table, void *val, uint64_t ps) {
  if (table->min == 0 || ps < table->min) {
    table->min = ps;
  }
  if (table->max == 0 || (ps + ind_sz) >= table->max) {
    table->max = ps + ind_sz;
  }
  auto l1 = &table->map[(ps >> (shift + shift + shift)) & ind_mask];
  if (*l1 == nullptr) {
    *l1 = (void ***)calloc(1, ind_sz * sizeof(void *));
  }
  auto l2 = &(*l1)[(ps >> (shift + shift)) & ind_mask];
  if (*l2 == nullptr) {
    *l2 = (void **)calloc(1, ind_sz * sizeof(void *));
  }
  (*l2)[(ps >> shift) & ind_mask] = val;
}

void *alloc_table_lookup(alloc_table *table, void *p) {
  uint64_t ps = (uint64_t)p;
  if (ps < table->min) {
    return nullptr;
  }
  if (ps > table->max) {
    return nullptr;
  }

  auto l1 = table->map[(ps >> (shift + shift + shift)) & ind_mask];
  if (l1 == nullptr) {
    return nullptr;
  }
  auto l2 = l1[(ps >> (shift + shift)) & ind_mask];
  if (l2 == nullptr) {
    return nullptr;
  }
  auto l3 = l2[(ps >> shift) & ind_mask];
  return l3;
}

void alloc_table_set_range(alloc_table *table, void *val, void *p, uint64_t range) {
  uint64_t ps = (uint64_t)p;

  assert((range & ind_mask) == 0);
  assert((ps & ind_mask) == 0);

  for (uint64_t i = 0; i < (range >> shift); i++) {
    set(table, val, ps);
    ps += ind_sz;
  }
}
