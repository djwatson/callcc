#define _POSIX_C_SOURCE 200112L
#define _GNU_SOURCE

#include <assert.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

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

uint64_t *gc_get_stack_top() { return stacktop; }
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

static uintptr_t align(uintptr_t val, uintptr_t alignment) {
  return (val + alignment - 1) & ~(alignment - 1);
}

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
static bool bt(uint64_t const *bits, uint64_t bit) {
  auto word = bit / 64;
  auto b = bit % 64;
  return bits[word] & (1UL << b);
}

static freelist_s freelist[size_classes];
static slab_info *partials[size_classes];
static slab_info *cur_slab[size_classes];
static LIST_HEAD(live_slabs);
static kvec_t(uint64_t *) roots;

static constexpr uint16_t page_classes = 16;
static kvec_t(slab_info *) pages_free[page_classes];

static uint16_t sz_to_page_class(uint64_t sz) {
  assert(sz >= PAGE_SIZE);
  uint32_t zeros = 32 - __builtin_clz(sz-1); // Find next-largest power of two.
  return zeros - 12; // Page bits.
}

bool get_partial_range(uint64_t sz_class, freelist_s *fl) { return false; }

void gc_init() {
  stacktop = (uint64_t *)__builtin_frame_address(0);

  void *addr;
#if __APPLE__
  addr = pthread_get_stackaddr_np(pthread_self());
#elif __linux__
  size_t size;
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_getattr_np(pthread_self(), &attr);
  pthread_attr_getstack(&attr, &addr, &size);
  pthread_attr_destroy(&attr);
  addr = (unsigned char *)addr + size;
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
  kv_init(roots);
  for(uint64_t i = 0; i < page_classes; i++) {
    kv_init(pages_free[i]);
  }
}

void gc_add_root(uint64_t *rootp) { kv_push(roots, rootp); }

void gc_pop_root(uint64_t const *rootp) {
  auto old_rootp = kv_pop(roots);
  assert(old_rootp == rootp);
}

typedef struct range {
  uint64_t *start;
  uint64_t *end;
} range;

static uint64_t totsize;
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
      bool found = alloc_table_lookup(&atable, val, (void **)&slab);
      if (found && (slab != nullptr) && (val >= slab->start) && (val < slab->end)) {
        // Find the start of the object
        uint64_t index =
            ((uint64_t)val - (uint64_t)slab->start) / (slab->class * 8);
        uint64_t base_ptr = (uint64_t)slab->start + (slab->class * 8 * index);
        if (!bt(slab->markbits, index)) {
          totsize += slab->class * 8;
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

extern int64_t symbol_table;

static void merge_and_free_slab(slab_info* slab) {
  // TODO: actual merge.
  auto page_class = sz_to_page_class(slab->end - slab->start);
  //printf("Page class %i sz class %i\n", page_class, slab->class);
  if (page_class > page_classes) {
    // Direct free
    //printf("Freeing huge %i\n", slab->class*8);
    free(slab->start);
    free(slab);
    return;
  }
  init_list_head(&slab->link);
  //printf("Freeing slab %i\n", slab->class);
  kv_push(pages_free[page_class], slab);
}

__attribute__((noinline, preserve_none)) static void rcimmix_collect() {
  struct timespec start;
  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &start);
  totsize = 0;

  printf("COLLECT...\n");

  // Clear marks
  list_head *itr;
  list_for_each (itr, &live_slabs) {
    slab_info *slab = container_of(itr, slab_info, link);
    memset(slab->markbits, 0, sizeof(slab->markbits));
    slab->marked = false;
  }

  // Init mark stack
  kv_init(markstack);

  // Mark C roots
  // TODO: unnecessary with conservative collection.
  for (uint64_t i = 0; i < kv_size(roots); i++) {
    auto root = kv_A(roots, i);
    kv_push(markstack, ((range){root, root + 1}));
  }

  // Mark stack
  uint64_t *sp = (uint64_t *)__builtin_frame_address(0);
  kv_push(markstack, ((range){sp, stacktop}));

  // Mark symbol table: TODO cleaup types
  uint64_t *v = (uint64_t *)(symbol_table & ~7);
  auto len = (*v) >> 3;
  for (uint64_t i = 0; i < len; i++) {
    uint64_t *symbol = (uint64_t *)(v[1 + i] & ~7);
    kv_push(markstack, ((range){&symbol[1], &symbol[3]}));
  }

  // Run mark loop.
  mark();

  // Sweep empty blocks.
  uint64_t freed_bytes = 0;
  uint64_t total_bytes = 0;
  itr = live_slabs.next;
  while(!list_is_head(itr, &live_slabs)) {
    auto next_itr = itr->next;
    auto slab = container_of(itr, slab_info, link);
    assert(!list_empty(&slab->link));
    if (!slab->marked) {
      list_del(itr);
      merge_and_free_slab(slab);
      freed_bytes += slab->class *8UL;
    }
    total_bytes += slab->class *8UL;
    itr = next_itr;
  }
  for (uint64_t i = 0; i < size_classes; i++) {
    cur_slab[i] = nullptr;
    freelist[i].start_ptr = default_slab_size;
    freelist[i].end_ptr = default_slab_size;
  }
  uint64_t live_bytes = total_bytes - freed_bytes;

  if (freed_bytes < (total_bytes /2)) {
    next_collect *= 2;
  }

  kv_destroy(markstack);

  clock_gettime(CLOCK_MONOTONIC, &end);
  double time_taken =
      ((double)end.tv_sec - (double)start.tv_sec) * 1000.0; // sec to ms
  time_taken +=
      ((double)end.tv_nsec - (double)start.tv_nsec) / 1000000.0; // ns to ms
  printf(
	 "COLLECT %.3f ms, total %li, free%% %f, next_collect %li\n",
	 time_taken, total_bytes, 100.0 * (double)freed_bytes / (double)total_bytes, next_collect);
}

static slab_info *alloc_slab(uint64_t sz_class) {
  auto sz = sz_class < size_classes ? default_slab_size : align(sz_class * 8, PAGE_SIZE);
  auto page_class = sz_to_page_class(sz);

  // TODO: split the range here based on actual size.
  // TODO: check larger bins & split.
  if (page_class < page_classes && kv_size(pages_free[page_class])) {
    slab_info* free = kv_pop(pages_free[page_class]);
    if (free) {
      //printf("Found free slab class %li %i %i\n", sz_class, free->class, page_class);
      free->class = sz_class;
      alloc_table_set_range(&atable, free, free->start,
                            free->end - free->start);
      list_add(&free->link, &live_slabs);
      return free;
    }
  } else {
    //printf("Allocing slab: %li\n", sz_class*8);
  }
  
  slab_info* free = calloc(1, sizeof(slab_info));
  free->class = sz_class;
  init_list_head(&free->link);

  posix_memalign((void **)&free->start, PAGE_SIZE, sz);
  free->end = free->start + sz;
  list_add(&free->link, &live_slabs);

  alloc_table_set_range(&atable, free, free->start, free->end - free->start);
  return free;
}

NOINLINE __attribute__((preserve_most)) static void *
rcimmix_alloc_slow(uint64_t sz) {
  if (collect_cnt >= next_collect) {
    collect_cnt = 0;
    rcimmix_collect();
    return rcimmix_alloc(sz);
  }
  assert((sz & 0x7) == 0);

  uint64_t sz_class = sz / 8;
  // It is a large slab.
  if (sz_class >= size_classes) {
    auto slab = alloc_slab(sz_class);
    collect_cnt += sz;
    return slab->start;
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
  }
  auto fl = &freelist[sz_class];

  auto s = fl->start_ptr;
  auto start = fl->start_ptr + (sz_class * 8);
  if (unlikely(start > fl->end_ptr)) {
    return rcimmix_alloc_slow(sz);
  }

  fl->start_ptr = start;
  return (void *)s;
}
