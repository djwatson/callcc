#define _POSIX_C_SOURCE 200112L
#define _GNU_SOURCE
#define GENGC 1
// #define USE_MPROTECT

#include <sys/mman.h>

#include <assert.h>
#include <gmp.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "alloc_table.h"
#include "gc.h"
#include "util/kvec.h"
#include "util/list.h"
#include "util/bitset.h"

#define likely(x) __builtin_expect(x, 1)
#define unlikely(x) __builtin_expect(x, 0)
#define NOINLINE __attribute__((noinline))
#define INLINE __attribute__((always_inline))

static constexpr uint64_t size_classes = 4096 / 8;

// Apparently termux already has this defined.
#ifndef PAGE_SIZE
static constexpr uint64_t PAGE_SIZE = 1UL << 12;
#endif
static constexpr uint64_t default_slab_size = PAGE_SIZE * 4;
static uint64_t next_collect = 50000000;
static uint64_t collect_cnt = 0;
static alloc_table atable;

static uint64_t *stacktop;

uint64_t *gc_get_stack_top() { return stacktop; }

static constexpr uint64_t mark_word_cnt = (default_slab_size / 8) / 64;
static constexpr uint64_t mark_byte_cnt = (default_slab_size / 8) / 8;

typedef struct slab_info {
  uint32_t class;
  uint32_t marked;
  uint64_t markbits[mark_word_cnt];
  uint8_t *end;
  uint8_t *start;
  list_head link;
} slab_info;

typedef struct freelist_s {
  uint64_t start_ptr;
  uint64_t end_ptr;
  slab_info *slab;
} freelist_s;

static uintptr_t align(uintptr_t val, uintptr_t alignment) {
  return (val + alignment - 1) & ~(alignment - 1);
}

static freelist_s freelist[size_classes];
static kvec_t(slab_info *) partials[size_classes];
static LIST_HEAD(live_slabs);
static kvec_t(uint64_t *) roots;

static constexpr uint16_t page_classes = 16;
static kvec_t(slab_info *) pages_free[page_classes];

static uint64_t page_class_to_sz(uint64_t page_class) {
  return (1UL << page_class) * PAGE_SIZE;
}
static uint64_t sz_to_page_class(uint64_t sz, bool check) {
  assert(sz >= PAGE_SIZE); // Must be at least one page
  assert(sz % PAGE_SIZE == 0);

  // Convert size to number of pages
  uint64_t pages = sz / PAGE_SIZE;

  uint32_t class = 64 - __builtin_clzll(pages);

  // If already a power of two, go down one class.
  if ((pages & (pages - 1)) == 0) {
    class --;
  } else if (check) {
    assert(false);
  }

  return class;
}

bool get_partial_range(uint64_t sz_class, freelist_s *fl) {
  auto slab = fl->slab;
again:
  int64_t end_index = -1;
  if (!slab || fl->end_ptr >= (uint64_t)slab->end) {
    if (kv_size(partials[sz_class]) > 0) {
      slab = kv_pop(partials[sz_class]);
      assert(slab->class == sz_class);
      fl->slab = slab;
    } else {
      return false;
    }
  } else {
    end_index = ((fl->end_ptr - (uint64_t)slab->start)) / (slab->class * 8);
  }
  uint64_t maxbit = ((slab->end - slab->start)) / (slab->class * 8);
  uint64_t new_start;
  if (!find_next_bit(slab->markbits, maxbit, end_index + 1, true, &new_start)) {
    slab = nullptr;
    goto again;
  }
  uint64_t new_end = maxbit - 1;
  find_next_bit(slab->markbits, maxbit, new_start + 1, false, &new_end);
  for (uint64_t i = new_start; i < new_end; i++) {
    assert(!bt(slab->markbits, i));
  }
  fl->start_ptr = (uint64_t)slab->start + new_start * slab->class * 8;
  fl->end_ptr = (uint64_t)slab->start + new_end * slab->class * 8;
  assert((uintptr_t)fl->start_ptr >= (uintptr_t)fl->slab->start);
  assert((uintptr_t)fl->end_ptr <= (uintptr_t)fl->slab->end);
  return true;
}

static void *rcimmix_alloc_align(size_t sz) {
  return rcimmix_alloc(align(sz, sizeof(void *)));
}
static void *rcimmix_realloc_align(void *p, size_t old_sz, size_t new_sz) {
  auto res = rcimmix_alloc_align(new_sz);
  auto copy_sz = old_sz;
  if (new_sz < old_sz) {
    copy_sz = new_sz;
  }
  memcpy(res, p, copy_sz);
  return res;
}
static void rcimmix_free(void *, size_t) {}

static intptr_t memstart;
static intptr_t memend;

void gc_init(void *stacktop_in) {
  memstart = (intptr_t)mmap(nullptr, PAGE_SIZE*PAGE_SIZE*120, PROT_READ | PROT_WRITE, MAP_PRIVATE|MAP_ANON, -1, 0);
  if (memstart == -1) {
    abort();
  }
  memend = memstart + PAGE_SIZE*PAGE_SIZE*120;
  alloc_table_init(&atable, memstart, memend);
  stacktop = stacktop_in;
  // Set defaults so we don't have to check for wrapping in
  // the fastpath.
  for (uint64_t i = 0; i < size_classes; i++) {
    freelist[i].start_ptr = default_slab_size;
    freelist[i].end_ptr = default_slab_size;
    freelist[i].slab = nullptr;
    kv_init(partials[i]);
  }
  kv_init(roots);
  for (uint64_t i = 0; i < page_classes; i++) {
    kv_init(pages_free[i]);
  }

  mp_set_memory_functions(rcimmix_alloc_align, rcimmix_realloc_align,
                          rcimmix_free);
}

void gc_add_root(uint64_t *rootp) { kv_push(roots, rootp); }

void gc_pop_root(uint64_t const *rootp) {
#ifndef NDEBUG
  auto old_rootp = kv_pop(roots);
  assert(old_rootp == rootp);
#endif
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
    //  Double check it is aligned.
    assert(((int64_t)r.start & 0x7) == 0);
    assert(((int64_t)r.end & 0x7) == 0);
    while (r.start < r.end) {
      uint8_t *val = (uint8_t *)*r.start;
      slab_info *slab;
      bool found = alloc_table_lookup(&atable, val, (void **)&slab);
      if (found && (slab != nullptr) && (val >= slab->start) &&
          (val < slab->end)) {
        // Find the start of the object
        if (list_empty(&slab->link)) {
          r.start++;
          continue;
        }
        uint64_t index =
            ((uint64_t)val - (uint64_t)slab->start) / (slab->class * 8);
        uint64_t base_ptr = (uint64_t)slab->start + (slab->class * 8 * index);
        if (!bt(slab->markbits, index)) {
          totsize += slab->class * 8;
          slab->marked += slab->class * 8;
          bts(slab->markbits, index);
          kv_push(markstack,
                  ((range){(uint64_t *)base_ptr,
                           (uint64_t *)(base_ptr + slab->class * 8)}));
        }
      }
      r.start++;
    }
  }
}

static void merge_and_free_slab(slab_info *slab) {
  // TODO: actual merge.
  if (slab->class < size_classes) {
    slab->start -= mark_byte_cnt;
  }
  auto page_class = sz_to_page_class(slab->end - slab->start, true);
  if (page_class >= page_classes) {
    // Direct free
    // printf("Freeing huge %i\n", slab->class*8);
    alloc_table_set_range(&atable, nullptr, slab->start,
                          slab->end - slab->start);
    free(slab->start);
    free(slab);
    return;
  }
  init_list_head(&slab->link);
#ifdef USE_MPROTECT
  mprotect(slab->start, slab->end - slab->start, PROT_NONE);
#endif
  kv_push(pages_free[page_class], slab);
}

static uint64_t collect_big = 0;
static bool next_force_full = false;
extern void *cur_link;
__attribute__((noinline, preserve_none)) static void rcimmix_collect() {
  /* struct timespec start; */
  /* struct timespec end; */
  /* clock_gettime(CLOCK_MONOTONIC, &start); */
  bool collect_full = next_force_full;

  /* collect_big += next_collect; */
  /* if (collect_big >= next_collect_big) { */
  /*   /\* collect_full = true; *\/ */
  /*   collect_big = 0; */
  /* } */
#ifdef GENGC
  if (collect_big++ == 8) {
    collect_big = 0;
    collect_full = true;
  }
#else
  collect_full = true;
#endif

  /* collect_full = true; */

  // Init mark stack
  kv_init(markstack);

  // Clear marks
  list_head *itr;
  list_for_each(itr, &live_slabs) {
    slab_info *slab = container_of(itr, slab_info, link);

    if (collect_full) {
      totsize = 0;
      memset(slab->markbits, 0, sizeof(slab->markbits));
      slab->marked = 0;
      if (slab->class < size_classes) {
        uint64_t *logbits = (uint64_t *)(slab->start - mark_byte_cnt);
        memset(logbits, 0, mark_byte_cnt);
      }
    } else {

      // Remembered set analysis for sticky mark-bit sweeping
      // (generational mark-sweep).
      if (slab->class < size_classes) {
        // Small classes use a logbits area at the start of the
        // slab.
        uint64_t *logbits = (uint64_t *)(slab->start - mark_byte_cnt);
        assert(((uint64_t)logbits & (default_slab_size - 1)) == 0);
        uint64_t bit = 0;
        while (true) {
          uint64_t res;
          auto hasnext =
              find_next_bit(logbits, mark_byte_cnt * 8, bit, false, &res);
          if (!hasnext) {
            break;
          }
          uint64_t logptr = (uint64_t)logbits + (res * 8);
          uint64_t index = (logptr - (uint64_t)slab->start) / (slab->class * 8);
          // Only walk remembered set if the object it is in is already marked -
          // otherwise it will already traced if live.
          if (bt(slab->markbits, index)) {
            kv_push(markstack,
                    ((range){(uint64_t *)logptr, (uint64_t *)(logptr + 8)}));
          }

          bit = res + 1;
        }
        /* printf("logcnt: %li\n", logcnt); */
        // Reset remembered set.
        memset(logbits, 0, mark_byte_cnt);
      } else {
        // printf("MARKLARGE\n");
        //  Large objects use a single bit, bit 0 in markbits
        if (bt(slab->markbits, 1)) {
          kv_push(markstack,
                  ((range){(uint64_t *)slab->start,
                           (uint64_t *)(slab->start + slab->class * 8)}));
          // Reset markbit.
          btr(slab->markbits, 1);
        }
      }
    }
  }
  for (uint64_t i = 0; i < size_classes; i++) {
    kv_clear(partials[i]);
  }

  // Mark static roots
  for (uint64_t i = 0; i < kv_size(roots); i++) {
    auto root = kv_A(roots, i);
    kv_push(markstack, ((range){root, root + 1}));
  }

  // Mark stack
  uint64_t *sp = (uint64_t *)__builtin_frame_address(0);
  kv_push(markstack, ((range){sp, stacktop}));


  // Run mark loop.
  mark();

  // Sweep empty blocks.
  uint64_t freed_bytes = 0;
  /* uint64_t total_bytes = 0; */
  itr = live_slabs.next;
  while (!list_is_head(itr, &live_slabs)) {
    auto next_itr = itr->next;
    auto slab = container_of(itr, slab_info, link);
    assert(!list_empty(&slab->link));
    if (slab->marked == 0) {
      list_del(itr);
      merge_and_free_slab(slab);
      freed_bytes += slab->end - slab->start;
    } else {
      auto tot_size = slab->end - slab->start;
      /* if (slab->marked != tot_size) { */
      /* 	printf("Frag %i clss %i %% %f\n", slab->marked, slab->class,
       * 100.0*(double)(tot_size - slab->marked) / (double)tot_size); */
      /* } */
      if (slab->class < size_classes) {
        if (slab->marked < tot_size / 2) {
          kv_push(partials[slab->class], slab);
        }
      }
    }
    /* total_bytes += slab->end - slab->start; */
    itr = next_itr;
  }
  for (uint64_t i = 0; i < size_classes; i++) {
    freelist[i].start_ptr = default_slab_size;
    freelist[i].end_ptr = default_slab_size;
    freelist[i].slab = nullptr;
  }
  /* uint64_t live_bytes = total_bytes - freed_bytes; */

  // TODO: ideally we would have a running statistic
  // how many bytes we *expect* to be freed by a full collect vs.
  // a minor collection.
  //
  // earley is highly fragmented, but full GC's don't help.
  // paraffins needs lots of full GC's.
  if (collect_full && (next_collect < totsize)) {
    next_collect = totsize;
  }
  next_force_full = false;
  if (!collect_full && freed_bytes < next_collect / 2) {
    next_force_full = true;
  }

  kv_destroy(markstack);

  /* clock_gettime(CLOCK_MONOTONIC, &end); */
  /* auto rem_bytes = total_bytes - freed_bytes; */
  /* double time_taken = */
  /*     ((double)end.tv_sec - (double)start.tv_sec) * 1000.0; // sec to ms */
  /* time_taken += */
  /*     ((double)end.tv_nsec - (double)start.tv_nsec) / 1000000.0; // ns to ms
   */
  /* printf( */
  /* 	 "COLLECT %.3f ms, full %i, %li total %li, freed %li, free%% %f,
   * next_collect %li, totsize %li rembytes %li, frag %% %f\n", */
  /* 	 time_taken, collect_full, totsize, total_bytes, freed_bytes, 100.0 *
   * (double)freed_bytes / (double)total_bytes, next_collect, */
  /* 	 totsize, rem_bytes, */
  /* 	 100.0 * (double) (rem_bytes - totsize) / (double)rem_bytes); */
}

static slab_info *alloc_slab(uint64_t sz_class) {
  auto sz = sz_class < size_classes ? default_slab_size
                                    : align(sz_class * 8, default_slab_size);
  // Find page class: Choose next largest class to ensure we have enough room.
  // TODO: no need to increment if perfect size.
  auto page_class = sz_to_page_class(sz, false);
  sz = page_class_to_sz(page_class);

  // TODO: split the range here based on actual size.
  // TODO: check larger bins & split.
  if (page_class < page_classes && kv_size(pages_free[page_class])) {
    slab_info *free = kv_pop(pages_free[page_class]);
    if (free) {
      assert(sz <= (free->end - free->start));
#ifdef USE_MPROTECT
      mprotect(free->start, free->end - free->start, PROT_READ | PROT_WRITE);
#endif
      free->class = sz_class;
      list_add(&free->link, &live_slabs);
      return free;
    }
  } else {
    // printf("Allocing slab: %li\n", sz_class*8);
  }

  static uint64_t tot_size = 0;
  tot_size += sizeof(slab_info);
  slab_info *free = calloc(1, sizeof(slab_info));
  free->class = sz_class;
  init_list_head(&free->link);

  tot_size += sz;
  //printf("Tot size %li\n", tot_size);
  free->start = (uint8_t*)memstart;
  memstart += sz;
  if (memstart >= memend) {
    abort();
  }
  if ((uint64_t)free->start & (default_slab_size - 1)) {
    abort();
  }
  //posix_memalign((void **)&free->start, default_slab_size, sz);
  memset(free->start, 0, sz);
  free->end = free->start + sz;
  list_add(&free->link, &live_slabs);

  alloc_table_set_range(&atable, free, free->start, free->end - free->start);
  return free;
}

NOINLINE __attribute__((preserve_most)) static alloc_result
rcimmix_alloc_slow(uint64_t sz) {
  if (collect_cnt >= next_collect) {
    collect_cnt = 0;
    rcimmix_collect();
    return rcimmix_alloc_with_slab(sz);
  }
  assert((sz & 0x7) == 0);

  uint64_t sz_class = sz / 8;
  // It is a large slab.
  if (sz_class >= size_classes) {
    auto slab = alloc_slab(sz_class);
    collect_cnt += sz;
    return (alloc_result){slab->start, slab};
  }
  // It's in a small slab.
  //  assert(freelist[sz_class].start_ptr >= freelist[sz_class].end_ptr);
  if (get_partial_range(sz_class, &freelist[sz_class])) {

  } else {
    auto slab = alloc_slab(sz_class);
    // Leave room for logbits
    memset(slab->start, 0, mark_byte_cnt);
    slab->start += mark_byte_cnt;

    freelist[sz_class].start_ptr = (uint64_t)slab->start;
    freelist[sz_class].end_ptr = (uint64_t)slab->end;
    freelist[sz_class].slab = slab;
    collect_cnt += freelist[sz_class].end_ptr - freelist[sz_class].start_ptr;
  }
  return rcimmix_alloc_with_slab(sz);
}

alloc_result rcimmix_alloc_with_slab(uint64_t sz) {
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
  return (alloc_result){(void *)s, fl->slab};
}

void *rcimmix_alloc(uint64_t sz) { return rcimmix_alloc_with_slab(sz).p; }

bool gc_is_small(uint64_t sz) {
  uint64_t sz_class = sz / 8;
  return likely(sz_class < size_classes);
}

// Assumes a is a small allocation.
void gc_log_fast(uint64_t a) {
#ifdef GENGC
#ifndef NDEBUG
  slab_info *slab;
  assert(alloc_table_lookup(&atable, (void *)a, (void **)&slab));
#endif
  uint64_t *logbits = (uint64_t *)(a & ~(default_slab_size - 1));

  uint64_t addr = a & (default_slab_size - 1);
  bts(logbits, (addr / 8));
#endif
}

NOINLINE void gc_log(uint64_t a) {
#ifdef GENGC
  slab_info *slab;
  if (!alloc_table_lookup(&atable, (void *)a, (void **)&slab)) {
    // It's in the static data section (probably).
    return;
  }

  if (likely(slab->class < size_classes)) {
    uint64_t *logbits = (uint64_t *)(a & ~(default_slab_size - 1));
    uint64_t addr = a & (default_slab_size - 1);
    bts(logbits, (addr / 8));
  } else {
    if (bt(slab->markbits, 0)) {
      bts(slab->markbits, 1);
    }
  }
#endif
}

void gc_log_with_slab(uint64_t a, void *sp) {
#ifdef GENGC
  slab_info *slab = sp;
  assert(slab);
  if (slab->class < size_classes) {
    uint64_t *logbits = (uint64_t *)(a & ~(default_slab_size - 1));
    uint64_t addr = a & (default_slab_size - 1);
    bts(logbits, (addr / 8));
  } else {
    if (bt(slab->markbits, 0)) {
      bts(slab->markbits, 1);
    }
  }
#endif
}
