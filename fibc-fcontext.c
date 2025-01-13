#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <valgrind/valgrind.h>

#include <assert.h>
#include <gc.h>

static int64_t succ(int64_t n) { return n + 1; }
static int64_t pred(int64_t n) { return n - 1; }

typedef struct clo {
  int64_t (*ptr)(void *data, int64_t val);
  void *clo_data;
  int64_t res;
} clo;

static int64_t call_clo(clo *clo, int64_t x) { return clo->ptr(clo, x); };

typedef int64_t (*ccthunk)(clo *, int64_t);

static int64_t thunk(void *unused, int64_t n) { return n; }

typedef struct ccsave {
  int64_t (*ptr)(void *data, int64_t val);
  void *clo_data;

  void *stack;
  size_t sz;
  struct ccsave *prev_link;
} ccsave;

static void *stacktop;

static ccsave *cur_link = NULL;
static uint8_t tmpstack[100];
static int64_t ccresthunk(void *unused, int64_t n) {
  ccsave *c = (ccsave *)unused;
  cur_link = c->prev_link;

  int64_t stack_bottom = (int64_t)stacktop - c->sz;
  void *saved_stack = c->stack;
  size_t saved_sz = c->sz;
  uint64_t tmpstackalign = ((uint64_t)tmpstack + 100) & ~15;

#if defined(__x86_64__)
  asm volatile(
      "mov %4, %%r12\n\t" // Save the return value in a callee-saved reg.
      "mov %1, %%r13\n\t" // save old sp
      "mov %0, %%rsp\n\t" // Switch to tmpstack.
      "mov %1, %%rdi\n\t" // Set up memcpy call args
      "mov %2, %%rsi\n\t"
      "mov %3, %%rdx\n\t"
      "call memcpy\n\t"      // call memcpy
      "mov %%r13, %%rsp\n\t" // Restore the old stack pointer.
      "pop %%rbp\n\t"        // Pop the old frame pointer.
      "mov %%r12, %%rax\n\t" // Move return value from callee-saved to rax.
      "ret\n\t"              // Return to caller of 'callcc'.
      :                      // output
      : "r"(tmpstackalign), "r"(stack_bottom), "r"(saved_stack), "r"(saved_sz),
        "r"(n)
      : "rsp", "rbp", "memory", "rax", "r12", "r13", "rdi", "rsi",
        "rdx" // clobbers
  );
#elif defined(__aarch64__)
  asm volatile("mov x19, %4\n\t" // Save the return value in a callee-saved reg.
               "mov x20, %1\n\t" // Save the old sp
               "mov SP, %0\n\t"  // Switch to tmpstack.
               "mov x0, %1\n\t"  // Set up memcpy call args
               "mov x1, %2\n\t"
               "mov x2, %3\n\t"
               "bl memcpy\n\t"   // call memcpy
               "mov x0, x19\n\t" // Restore the old stack pointer.
               "mov sp, x20\n\t" // Move return value from callee-saved to rax.
               "ldp x29, x30, [sp], #16\n\t" // leave
               "ret\n\t"                     // Return to caller of 'callcc'.
               :                             // output
               : "r"(tmpstack), "r"(stack_bottom), "r"(saved_stack),
                 "r"(saved_sz), "r"(n)
               : "memory", "x19", "x20", "x0", "x1", "x2");
#endif
  __builtin_unreachable();
  return n;
}

void need_more_frames() {
  int64_t res;
  // This is called as a return point.  In x86_64, the return is in rax.
#if defined(__x86_64__)
  asm volatile("mov %%rax, %0\n\t" : "=r"(res) : : "rax");
  // In aarch return and arg1 are equal, so this could be
  // need_more_frames(int64_t res),
  // but let's do it like this for consistency.
#elif defined(__aarch64__)
  asm volatile("mov %0, x0\n\t" : "=r"(res) : : "x0");
#endif
  // assert(cur_link);
  ccresthunk(cur_link, res);
}

uint64_t memuse = 0;
void *mem;
void *my_malloc(size_t sz) {
  memuse -= sz;
  assert(memuse >= (uint64_t)mem);
  return (void *)memuse;
}

__attribute__((returns_twice, noinline, preserve_none)) int64_t
callcc(ccthunk t, int64_t x) {
  ccsave *stack = my_malloc(sizeof(ccsave));

  void *stack_bottom = __builtin_frame_address(0);
  size_t stack_sz = stacktop - stack_bottom;
  stack->stack = my_malloc(stack_sz);
  memcpy(stack->stack, stack_bottom, stack_sz);
  stack->sz = stack_sz;

  stack->ptr = ccresthunk;
  stack->clo_data = stack;
  stack->prev_link = cur_link;

  int64_t unused_res;

  cur_link = stack;
#if defined(__x86_64__)
  asm volatile("mov %1, %%rsp\n\t" // Reset stack pointer to stacktop
               "mov $0, %%rbp\n\t" // Clear out the frame pointer
               "push %2\n\t"       // Push return address of need_more_frames
               "mov %3, %%rdi\n\t" // Set up call thunk
               "mov %4, %%rsi\n\t" // and call argument
               "jmp *%5\n\t"       // Jump to thunk.
               : "=r"(unused_res)  // output
               : "r"(stacktop), "r"(need_more_frames), "r"(stack), "r"(x),
                 "r"(t)                 // input
               : "rdi", "rsi", "memory" // clobbers
  );

#elif defined(__aarch64__)
  asm volatile("mov sp, %1\n\t"   // Reset stack pointer to stacktop
               "mov x29, 0\n\t"   // Clear out the frame pointer
               "mov x30, %2\n\t"  // Set return address of need_more_frames
               "mov x0, %3\n\t"   // Set up call thunk
               "mov x1, %4\n\t"   // and call argument
               "br %5\n\t"        // Jump to thunk.
               : "=r"(unused_res) // output
               : "r"(stacktop), "r"(need_more_frames), "r"(stack), "r"(x),
                 "r"(t)               // input
               : "x0", "x1", "memory" // clobbers
  );
#endif
  // This is unreachable, but if you use __builtin_unreachable(), callers
  // will not have valid return points ... so fake a call.

  // Also, the result *must* be unknowable at compile-time: clang is smart
  // enough to inline this result, not knowing there is no result.
  return unused_res;
}

static int64_t addc(int64_t x, int64_t y, clo *k) {
  if (y == 0) {
    // return x;
    return call_clo(k, x);
  }
  [[clang::musttail]] return addc(succ(x), pred(y), k);
}

static int64_t fibc(int64_t x, clo *c);
static int64_t cc1(clo *c, int64_t x) { return fibc(pred(x), c); }

static int64_t cc2(clo *c, int64_t x) { return fibc(pred(pred(x)), c); }

static int64_t fibc(int64_t x, clo *c) {

  if (x == 0) {
    // return 0;
    return call_clo(c, 0);
  }
  if (pred(x) == 0) {
    // return 1;
    return call_clo(c, 1);
  }
  int64_t arg1 = callcc(cc1, x);
  int64_t arg2 = callcc(cc2, x);
  return addc(arg1, arg2, c);
}

clo *foo;

int64_t cont(clo *c, int64_t x) {
  foo = c;
  return x;
}

__attribute__((returns_twice)) int64_t bar(int64_t x) {
  return callcc(cont, x + 1);
}



//////// Leaf test from r4rstest

enum : int64_t {
  FALSE_REP = 4,
    NIL = 4 + 8,
    EOT = 4 + 9,
};

typedef struct cons_s {
  int64_t a;
  int64_t b;
}cons_s;

int64_t cons(int64_t a, int64_t b) {
  cons_s* res = my_malloc(sizeof(cons_s));
  res->a = a;
  res->b = b;
  return (int64_t)res + 1;
}

enum : uint8_t {
  RET_IDX,
  CONT_IDX,
  OBJ_IDX,
  EOT_IDX,
};

int pairp(int64_t obj) {
  return obj&1;
}

cons_s* to_cons(int64_t obj) {
  assert(pairp(obj));
  return (cons_s*)(obj-1);
}

int64_t thunk2(clo* cc, int64_t data) {
  int64_t* clo_data2 = (int64_t*)data;
  int64_t* clo_data = (int64_t*)clo_data2[0];
  int64_t obj = clo_data2[1];
  clo_data[CONT_IDX] = (int64_t)cc;
  return call_clo((clo*)clo_data[RET_IDX], obj);
}

int64_t recur_f(void *data, int64_t obj) {
  int64_t* clo_data = ((clo*)data)->clo_data;
  if (pairp(obj)) {
    while(pairp(obj)) {
      cons_s* c = to_cons(obj);
      obj=c->b;
      recur_f(data, c->a);
    }
  } else {
    int64_t* data2 = my_malloc(sizeof(int64_t)*2);
    data2[0] = (int64_t)clo_data;
    data2[1] = obj;
    callcc(thunk2, (int64_t)data2);
  }
  return 0;
}

int64_t return_f(void* data, int64_t obj) {
  int64_t* clo_data = ((clo*)data)->clo_data;
  return call_clo((clo*)clo_data[RET_IDX], clo_data[EOT_IDX]);
}

int64_t cont_f(void* data, int64_t x) {
  int64_t* clo_data = ((clo*)data)->clo_data;
  recur_f(data, clo_data[OBJ_IDX]);
  clo* new_cont = my_malloc(sizeof(clo));
  new_cont->ptr = return_f;
  new_cont->clo_data = clo_data;
  clo_data[CONT_IDX] = (int64_t)new_cont;
  return call_clo(new_cont, FALSE_REP);
}

int64_t thunk1(clo* cc, int64_t data) {
  int64_t* clo_data = (int64_t*)data;
  clo_data[RET_IDX] = (int64_t)cc;
  return call_clo((clo*)clo_data[CONT_IDX], FALSE_REP);
}

int64_t do_callcc(void* data, int64_t unused) {
  int64_t*clo_data = ((clo*)data)->clo_data;
  return callcc(thunk1, (int64_t)clo_data);
}

int64_t next_leaf_generator(int64_t obj, int64_t eot) {
  int64_t* clo_data = my_malloc(sizeof(int64_t)*4);
  clo_data[RET_IDX] = FALSE_REP;
  clo_data[EOT_IDX] = eot;
  clo_data[OBJ_IDX] = obj;
  clo* cont = my_malloc(sizeof(clo));
  clo_data[CONT_IDX] = (int64_t)cont;
  cont->clo_data = clo_data;
  cont->ptr = cont_f;

  clo* retval = my_malloc(sizeof(clo));
  retval->ptr = do_callcc;
  retval->clo_data = clo_data;
  return (int64_t)retval;
}

////////////////////////

int main() {
  uint64_t foobar;
  // GC_expand_hp(50000000);
  stacktop = (void *)((uint64_t)(&foobar) & ~15);
  size_t memsize = 1000000000;
  mem = malloc(memsize);
  memuse = (uint64_t)mem + memsize;

  VALGRIND_STACK_REGISTER(&tmpstack[100], &tmpstack[0]);

  /*
  // iter count 10
  for (int64_t i = 0; i < 10; i++) {
    memuse = (uint64_t)mem + memsize;
    clo c = {thunk, NULL};
    int64_t res = fibc(30, &c);
    printf("Res %li\n", res);
  }

  // Test 2: test that callcc *up* the stack works.
  int64_t res = bar(10);
  printf("Bar res is %li\n", res);
  if (res < 20)
    call_clo(foo, res + 1);
  */

  int64_t testa = cons(8, cons(16, cons(24, NIL)));
  int64_t testb = cons(cons(8, NIL), cons(16, cons(24, NIL)));
  int64_t testc = cons(cons(8, NIL), cons(16, cons(24, cons(32, NIL))));

  int64_t gen = next_leaf_generator(testc, EOT);
  int64_t res = call_clo((clo*)gen, 0);
  while(res != EOT) {
    printf("Res %li\n", res);
    res = call_clo((clo*)gen, 0);
  }
  
  
  return 0;
}
