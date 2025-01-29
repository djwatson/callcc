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
static constexpr uint64_t PAGE_SIZE = 1UL << 12;
static constexpr uint64_t default_slab_size = PAGE_SIZE*4;
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
  slab_info* slab;
} freelist_s;

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
static kvec_t(slab_info*) partials[size_classes];
static LIST_HEAD(live_slabs);
static kvec_t(uint64_t *) roots;

static constexpr uint16_t page_classes = 16;
static kvec_t(slab_info *) pages_free[page_classes];

static uint16_t sz_to_page_class(uint64_t sz) {
  assert(sz >= PAGE_SIZE);
  uint32_t zeros = 32 - __builtin_clz(sz-1); // Find next-largest power of two.
  return zeros - 12; // Page bits.
}

bool find_next_bit(uint64_t const* bits, uint64_t maxbit, uint64_t bit, bool invert, uint64_t* result) {
  auto word = bit / 64;
  auto b = bit % 64;
  /* printf("find_next_bit word %li b %li\n", word, b); */

  if (bit >= maxbit) {
    return false;
  }

  uint64_t search = bits[word] >> b;
  if (invert) {
    search = ~search;
  }

  auto res = __builtin_ffsll(search);
  if(res && res <= (64 - b)) {
    bit += res - 1;
    *result = bit;
    if (invert) {
      assert(!bt(bits, bit));
    } else {
      assert(bt(bits, bit));
    }
    return true;
  }
  bit += 64 - b;
  assert((bit % 64) == 0);
  [[clang::musttail]] return find_next_bit(bits, maxbit, bit, invert, result);
}

bool get_partial_range(uint64_t sz_class, freelist_s *fl) {
  auto slab = fl->slab;
  /* if (sz_class != 6) { */
  /*   return false; */
  /* } */
  //return false;
 again:
  int64_t end_index = -1;
  if (!slab || fl->end_ptr >= (uint64_t)slab->end) {
    if (kv_size(partials[sz_class]) > 0) {
      slab = kv_pop(partials[sz_class]);
      //printf("New slab %i %p %p\n", slab->marked, slab->start, slab->end);
      assert(slab->class == sz_class);
      fl->slab = slab;      
    } else {
      return false;
    }
  } else {
    //assert(fl->start_ptr >= slab->start && fl->end_ptr <= slab->end);
    end_index = ((fl->end_ptr - (uint64_t)slab->start)) / (slab->class *8);
    /* printf("Cont slab end_indx %li\n", end_index); */
  }
  /* printf("CHECKING for partial %lx\n", slab->markbits[(end_index+1)/64]); */
  uint64_t maxbit = ((slab->end - slab->start)) / (slab->class * 8);
  uint64_t new_start;
  if(!find_next_bit(slab->markbits, maxbit, end_index + 1, true, &new_start)) {
    slab = nullptr;
    /* printf("No free start\n"); */
    goto again;
  }
  /* if (new_start > 0 && new_start < maxbit-1) { */
  /*   printf("New start %li %i %i %i\n", new_start, bt(slab->markbits, new_start-1), */
  /* 	   bt(slab->markbits, new_start), */
  /* 	   bt(slab->markbits, new_start+1)); */
  /* } */
  uint64_t new_end = maxbit-1;
  find_next_bit(slab->markbits, maxbit, new_start+1, false, &new_end);
  for(uint64_t i = new_start; i<new_end; i++) {
    assert(!bt(slab->markbits, i));
  }
  //printf("End index was %li, new start %li, new end %li\n", end_index, new_start, new_end);
  fl->start_ptr = (uint64_t)slab->start + new_start * slab->class * 8;
  fl->end_ptr = (uint64_t)slab->start + new_end * slab->class * 8;
  return true;
}

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
    freelist[i].slab = nullptr;
    kv_init(partials[i]);
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
	  slab->marked += slab->class * 8;
          bts(slab->markbits, index);
          // printf("Marking %p cls %i\n", base_ptr, slab->class);
          kv_push(markstack,
                  ((range){(uint64_t *)base_ptr,
                           (uint64_t *)(base_ptr + slab->class * 8)}));
        }
      }
      r.start++;
    }
  }
}

extern int64_t symbol_table;
extern  int64_t shadow_stack[100];

static void merge_and_free_slab(slab_info* slab) {
  // TODO: actual merge.
  auto page_class = sz_to_page_class(slab->end - slab->start);
  //printf("Page class %i sz class %i\n", page_class, slab->class);
  if (page_class >= page_classes) {
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

static uint64_t collect_big = 0;
static bool next_force_full = false;
__attribute__((noinline, preserve_none)) static void rcimmix_collect() {
  struct timespec start;
  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &start);
  bool collect_full = next_force_full;

  /* collect_big += next_collect; */
  /* if (collect_big >= next_collect_big) { */
  /*   /\* collect_full = true; *\/ */
  /*   collect_big = 0; */
  /* } */
  if (collect_big++ == 16) {
    collect_big = 0;
    collect_full = true;
  }

  /* collect_full = true; */

  // Init mark stack
  kv_init(markstack);

  // Clear marks
  list_head *itr;
  list_for_each (itr, &live_slabs) {
    slab_info *slab = container_of(itr, slab_info, link);

    if (collect_full) {
      totsize = 0;
      memset(slab->markbits, 0, sizeof(slab->markbits));
      slab->marked = 0;
      if (slab->class < size_classes) {
	uint64_t* logbits = (uint64_t*)(slab->start - mark_byte_cnt);
	memset(logbits, 0, mark_byte_cnt);
      }
    }  else {

      // Remembered set analysis for sticky mark-bit sweeping
      // (generational mark-sweep).
      if (slab->class < size_classes) {
	// Small classes use a logbits area at the start of the
	// slab.
	uint64_t* logbits = (uint64_t*)(slab->start - mark_byte_cnt);
	uint64_t bit = 0;
	while(true) {
	  uint64_t res;
	  auto hasnext = find_next_bit(logbits, mark_byte_cnt*8, bit, false, &res);
	  if (!hasnext) {
	    break;
	  }
	  uint64_t logptr = (uint64_t)logbits + (res * 8);
	  uint64_t index = (logptr - (uint64_t)slab->start) / (slab->class*8);
	  // Only walk remembered set if the object it is in is already marked -
	  // otherwise it will already traced if live.
	  if (bt(slab->markbits, index)) {
	    kv_push(markstack,
		    ((range){(uint64_t*)logptr, (uint64_t*)(logptr + 8)}));
	  }
	  
	  bit = res + 1;
	}
	/* printf("logcnt: %li\n", logcnt); */
	// Reset remembered set.
	memset(logbits, 0, mark_byte_cnt);
      } else {
	printf("MARKLARGE\n");
	// Large objects use a single bit, bit 0 in markbits
	if (bt(slab->markbits, 1)) {
	  kv_push(markstack,
		  ((range){(uint64_t*)slab->start,
			   (uint64_t*)(slab->start + slab->class * 8)}));
	  // Reset markbit.
	  btr(slab->markbits, 1);
	}
      }


    }
  }
  for (uint64_t i = 0; i < size_classes; i++) {
    kv_clear(partials[i]);
  }

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
  kv_push(markstack, ((range){(uint64_t*)&shadow_stack[0], (uint64_t*)&shadow_stack[100]}));

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
    if (slab->marked == 0) {
      list_del(itr);
      merge_and_free_slab(slab);
      freed_bytes += slab->end - slab->start;
    } else {
      auto tot_size = slab->end - slab->start;
      /* if (slab->marked != tot_size) { */
      /* 	printf("Frag %i clss %i %% %f\n", slab->marked, slab->class, 100.0*(double)(tot_size - slab->marked) / (double)tot_size); */
      /* } */
      if (slab->class < size_classes) {
	if (slab->marked < tot_size/2) {
	  kv_push(partials[slab->class], slab);
	}
      }
    }
    total_bytes += slab->end - slab->start;
    itr = next_itr;
  }
  for (uint64_t i = 0; i < size_classes; i++) {
    freelist[i].start_ptr = default_slab_size;
    freelist[i].end_ptr = default_slab_size;
    freelist[i].slab = nullptr;
  }
  uint64_t live_bytes = total_bytes - freed_bytes;

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

  clock_gettime(CLOCK_MONOTONIC, &end);
  auto rem_bytes = total_bytes - freed_bytes;
  double time_taken =
      ((double)end.tv_sec - (double)start.tv_sec) * 1000.0; // sec to ms
  time_taken +=
      ((double)end.tv_nsec - (double)start.tv_nsec) / 1000000.0; // ns to ms
  /* printf( */
  /* 	 "COLLECT %.3f ms, full %i, %li total %li, freed %li, free%% %f, next_collect %li, totsize %li rembytes %li, frag %% %f\n", */
  /* 	 time_taken, collect_full, totsize, total_bytes, freed_bytes, 100.0 * (double)freed_bytes / (double)total_bytes, next_collect, */
  /* 	 totsize, rem_bytes, */
  /* 	 100.0 * (double) (rem_bytes - totsize) / (double)rem_bytes); */
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
      list_add(&free->link, &live_slabs);
      return free;
    }
  } else {
    //printf("Allocing slab: %li\n", sz_class*8);
  }
  
  slab_info* free = calloc(1, sizeof(slab_info));
  free->class = sz_class;
  init_list_head(&free->link);

  posix_memalign((void **)&free->start, default_slab_size, sz);
  free->end = free->start + sz;
  list_add(&free->link, &live_slabs);

  alloc_table_set_range(&atable, free, free->start, free->end - free->start);
  if (sz_class < size_classes) {
    // Leave room for logbits
    memset(free->start, 0, mark_byte_cnt);
    free->start += mark_byte_cnt;
  }
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
    
  } else {
    auto slab = alloc_slab(sz_class);
    freelist[sz_class].start_ptr = (uint64_t)slab->start;
    freelist[sz_class].end_ptr = (uint64_t)slab->end;
    freelist[sz_class].slab = slab;
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

bool gc_is_small(uint64_t sz) {
  uint64_t sz_class = sz / 8;
  return likely(sz_class < size_classes);
}

// Assumes a is a small allocation.
void gc_log_fast(uint64_t a) {
  slab_info* slab;
  assert(alloc_table_lookup(&atable, (void*)a, (void**)&slab));
  uint64_t* logbits = (uint64_t*)(a & ~(default_slab_size-1));

  uint64_t addr = a & (default_slab_size-1);
  bts(logbits,(addr / 8) ); 
}

NOINLINE void gc_log(uint64_t a) {
  slab_info* slab;
  if (!alloc_table_lookup(&atable, (void*)a, (void**)&slab)) {
    // It's in the static data section (probably).
    return;
  }
  
  if (likely(slab->class < size_classes)) {
    uint64_t* logbits = (uint64_t*)(a & ~(default_slab_size-1));
    uint64_t addr = a & (default_slab_size-1);
    bts(logbits,(addr / 8));
  } else {
    // TODO: fix
    btr(slab->markbits, 1);
  }
}
