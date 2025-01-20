#define _POSIX_C_SOURCE 200112L
#define _GNU_SOURCE

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <pthread.h>

#include "alloc_table.h"
#include "gc.h"
#include "kvec.h"
#include "list.h"

#define likely(x) __builtin_expect(x, 1)
#define unlikely(x) __builtin_expect(x, 0)
#define NOINLINE __attribute__((noinline))
#define INLINE __attribute__((always_inline))

static constexpr uint64_t size_classes = 4096 / 8;
static constexpr uint64_t default_slab_size = 16384;
static uint64_t next_collect = 50000000;
static uint64_t collect_cnt = 0;
static uint64_t PAGE_SIZE = 1UL << 12;
static alloc_table atable;

static uint64_t *stacktop;

uint64_t* gc_get_stack_top() {
  return stacktop;
}
typedef struct freelist_s {
  uint64_t start_ptr;
  uint64_t end_ptr;
} freelist_s;

typedef struct slab_info {
  uint32_t class;
  bool marked;
  uint64_t markbits[(default_slab_size / 8) / 64];
  uint8_t *end;
  uint8_t *start;
  list_head link;
} slab_info;

static void bts(uint64_t *bits, uint64_t bit) {
  auto word = bit / 64;
  auto b = bit % 64;
  bits[word] |= 1UL << b;
}
static void btr(uint64_t *bits, uint64_t bit) {
  auto word = bit / 64;
  auto b = bit % 64;
  bits[word] &= ~(1UL << b);
}
static bool bt(uint64_t *bits, uint64_t bit) {
  auto word = bit / 64;
  auto b = bit % 64;
  return bits[word] & (1UL << b);
}

static freelist_s freelist[size_classes];
static slab_info *partials[size_classes];
static slab_info *cur_slab[size_classes];
static kvec_t(slab_info *) all_slabs;
static kvec_t(uint64_t *) roots;
static LIST_HEAD(free_slabs);
static kvec_t(slab_info *) large_free;

bool get_partial_range(uint64_t sz_class, freelist_s *fl) { return false; }

void gc_init() {
  stacktop = (uint64_t *)__builtin_frame_address(0);

  void* addr;
#if __APPLE__
  addr = pthread_get_stackaddr_np(pthread_self());
#elif __linux__
  size_t size;
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_getattr_np(pthread_self(), &attr);
  pthread_attr_getstack(&attr, &addr, &size);
  pthread_attr_destroy(&attr);
  addr = (unsigned char*)addr + size;
#else
#error "Unknown OS: Can't get stack base"
#endif
  printf("frametop %p pthreadtop %p\n", stacktop, addr);
  stacktop = addr;
  
  // Set defaults so we don't have to check for wrapping in
  // the fastpath.
  for (uint64_t i = 0; i < size_classes; i++) {
    freelist[i].start_ptr = default_slab_size;
    freelist[i].end_ptr = default_slab_size;
  }
  kv_init(all_slabs);
  kv_init(roots);
  kv_init(large_free);
}

void gc_add_root(uint64_t *rootp) { kv_push(roots, rootp); }

void gc_pop_root(uint64_t *rootp) {
  auto old_rootp = kv_pop(roots);
  assert(old_rootp == rootp);
}

typedef struct range {
  uint64_t *start;
  uint64_t *end;
} range;

static kvec_t(range) markstack;
static void mark() {
  while (kv_size(markstack) > 0) {
    range r = kv_pop(markstack);
    // printf("RANGE %p %p\n", r.start, r.end);
    //  Double check it is aligned.
    assert(((int64_t)r.start & 0x7) == 0);
    assert(((int64_t)r.end & 0x7) == 0);
    while (r.start < r.end) {
      uint8_t *val = (uint8_t *)*r.start;
      slab_info *slab;
      bool found = alloc_table_lookup(&atable, val, (void**)&slab);
      if (found && slab && (val >= slab->start) && (val < slab->end)) {
        // Find the start of the object
        uint64_t index =
            ((uint64_t)val - (uint64_t)slab->start) / (slab->class * 8);
        uint64_t base_ptr = (uint64_t)slab->start + (slab->class * 8 * index);
        if (!bt(slab->markbits, index)) {
          bts(slab->markbits, index);
          // printf("Marking %p cls %i\n", base_ptr, slab->class);
          kv_push(markstack,
                  ((range){(uint64_t *)base_ptr,
                           (uint64_t *)(base_ptr + slab->class * 8)}));
        }
        slab->marked = true;
      }
      r.start++;
    }
  }
}

__attribute__((noinline, preserve_none)) static void rcimmix_collect() {
  struct timespec start;
  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &start);

  // Clear marks
  for (uint64_t i = 0; i < kv_size(all_slabs); i++) {
    slab_info *slab = kv_A(all_slabs, i);
    memset(slab->markbits, 0, sizeof(slab->markbits));
    slab->marked = false;
  }

  // Init mark stack
  kv_init(markstack);

  // Mark roots
  for (uint64_t i = 0; i < kv_size(roots); i++) {
    auto root = kv_A(roots, i);
    kv_push(markstack, ((range){root, root + 1}));
  }

  // Mark stack
  uint64_t *sp = (uint64_t *)__builtin_frame_address(0);
  kv_push(markstack, ((range){sp, stacktop}));
  mark();

  // Sweep empty blocks.
  kv_destroy(large_free);
  kv_init(large_free);
  uint64_t free_blocks = 0;
  for (uint64_t i = 0; i < kv_size(all_slabs); i++) {
    auto slab = kv_A(all_slabs, i);
    if (!list_empty(&slab->link)) {
      // It's already an empty block.
      free_blocks++;
      continue;
    }
    if (!slab->marked) {
      // TODO free large
      if (slab->class >= size_classes) {
        // free(slab->start);
        // slab->start = nullptr;
        kv_push(large_free, slab);
      } else {
        list_add(&slab->link, &free_slabs);
        free_blocks++;
      }
    }
  }
  /* printf("Free blocks: %li all blocks: %li\n", free_blocks, kv_size(all_slabs)); */
  for (uint64_t i = 0; i < size_classes; i++) {
    cur_slab[i] = nullptr;
    freelist[i].start_ptr = default_slab_size;
    freelist[i].end_ptr = default_slab_size;
  }

  auto new_next_collect =
      (kv_size(all_slabs) - free_blocks) * default_slab_size * 2;
  if (new_next_collect > next_collect) {
    next_collect = new_next_collect;
  }

  kv_destroy(markstack);

  clock_gettime(CLOCK_MONOTONIC, &end);
  double time_taken =
      ((double)end.tv_sec - (double)start.tv_sec) * 1000.0; // sec to ms
  time_taken +=
      ((double)end.tv_nsec - (double)start.tv_nsec) / 1000000.0; // ns to ms
  printf("COLLECT %.3f ms, there are %li slabs next %li\n", time_taken,
         kv_size(all_slabs), next_collect);
}

static slab_info *alloc_slab(uint64_t sz_class) {
  slab_info *free = list_first_entry_or_null(&free_slabs, slab_info, link);
  if (free) {
    free->class = sz_class;
    list_del(&free->link);
  } else {
    free = calloc(1, sizeof(slab_info));
    free->class = sz_class;
    init_list_head(&free->link);

    posix_memalign((void **)&free->start, PAGE_SIZE, default_slab_size);
    free->end = free->start + default_slab_size;
    kv_push(all_slabs, free);
  }
  alloc_table_set_range(&atable, free, free->start, free->end - free->start);
  return free;
}

static uintptr_t align(uintptr_t val, uintptr_t alignment) {
  return (val + alignment - 1) & ~(alignment - 1);
}

NOINLINE __attribute__((preserve_most)) static void *rcimmix_alloc_slow(uint64_t sz) {
  if (collect_cnt >= next_collect) {
    collect_cnt = 0;
    rcimmix_collect();
    return rcimmix_alloc(sz);
  }
  assert((sz & 0x7) == 0);

  uint64_t sz_class = sz / 8;
  // It has to be a large slab.
  if (sz_class >= size_classes) {
    if (kv_size(large_free)) {
      // TODO hack hack hack
      collect_cnt += sz;
      auto f = kv_pop(large_free);
      assert(f->class >= sz_class);
      return f->start;
    } else {
      sz = align(sz, PAGE_SIZE);
      slab_info *info = malloc(sizeof(slab_info));
      sz_class = sz / 8;
      info->class = sz_class;
      init_list_head(&info->link);
      posix_memalign((void **)&info->start, PAGE_SIZE, sz);
      info->end = info->start + sz;
      alloc_table_set_range(&atable, info, info->start, sz);
      collect_cnt += sz;
      kv_push(all_slabs, info);

      return info->start;
    }
  }
  // It's in a small slab.
  //  assert(freelist[sz_class].start_ptr >= freelist[sz_class].end_ptr);
  if (get_partial_range(sz_class, &freelist[sz_class])) {
    abort();
  } else {
    auto slab = alloc_slab(sz_class);
    freelist[sz_class].start_ptr = (uint64_t)slab->start;
    freelist[sz_class].end_ptr = (uint64_t)slab->end;
    collect_cnt += freelist[sz_class].end_ptr - freelist[sz_class].start_ptr;
  }
  return rcimmix_alloc(sz);
}

void *rcimmix_alloc(uint64_t sz) {
  assert((sz & 0x7) == 0);
  uint64_t sz_class = sz / 8;
  if (unlikely(sz_class >= size_classes)) {
     return rcimmix_alloc_slow(sz);
  } else {
    auto fl = &freelist[sz_class];

    auto s = fl->start_ptr;
    auto start = fl->start_ptr + sz_class * 8;
    if (unlikely(start > fl->end_ptr)) {
       return rcimmix_alloc_slow(sz);
    } else {

      fl->start_ptr = start;
      return (void *)s;
    }
  }
}
