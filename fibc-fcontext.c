#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <gc.h>

static int64_t succ(int64_t n) { return n + 1; }
static int64_t pred(int64_t n) { return n - 1; }

typedef struct clo {
  int64_t (*ptr)(void* data, int64_t val);
  void* clo_data;
  int64_t res;
} clo;



static int64_t call_clo(clo* clo, int64_t x) {
  return clo->ptr(clo, x);
};

typedef int64_t(*ccthunk)(clo*, int64_t);

static int64_t thunk(void* unused, int64_t n) { return n; }

typedef struct ccsave {
  int64_t (*ptr)(void* data, int64_t val);
  void* clo_data;

  void* stack;
  size_t sz;
} ccsave;

void* stacktop;

static int64_t ccresthunk(void* unused, int64_t n) {
  ccsave* c = (ccsave*)unused;

  register int64_t stack_bottom asm("rdi") = (int64_t)stacktop - c->sz;
  register void* saved_stack asm ("rsi") = c->stack;
  register size_t saved_sz asm ("rcx") = c->sz;
  register size_t res asm ("rax") = n;

   asm volatile (
          	 "mov %2, %%rsp\n\t" // Restore the old stack pointer.
		 "rep movsb\n\t" // Copy the old stack over (copy rsi -> rdi, rcx times)
		 "pop %%rbp\n\t" // Pop the old frame pointer.
		 "ret\n\t" // Return to caller of 'getstack'.
		: // output
		 : "r" (saved_stack), "r" (saved_sz), "r" (stack_bottom), "r" (res)//input
		: "rsp", "rbp", "memory"// clobbers
		);
   __builtin_unreachable();
   return n;
}

__attribute__((returns_twice, noinline, preserve_none)) int64_t callcc(ccthunk t, int64_t x) {
  ccsave* stack = GC_malloc(sizeof(ccsave));

  void* stack_bottom = __builtin_frame_address(0);
  size_t stack_sz = stacktop - stack_bottom;
  //printf("Stack size %li\n", stack_sz);
  stack->stack = GC_malloc(stack_sz);
  memcpy(stack->stack, stack_bottom, stack_sz);
  stack->sz = stack_sz;

  stack->ptr = ccresthunk;
  stack->clo_data = stack;

  return t((clo*)stack, x);
}

static int64_t addc(int64_t x, int64_t y, clo* k) {
  if (y == 0) {
    return call_clo(k, x);
  }
  return addc(succ(x), pred(y), k);
}

static int64_t fibc(int64_t x, clo* c);
static int64_t cc1(clo* c, int64_t x) {
  return fibc(pred(x), c);
}

static int64_t cc2(clo *c, int64_t x) { return fibc(pred(pred(x)), c); }

static int64_t fibc(int64_t x, clo* c) {
  if (x == 0) {
    return call_clo(c, 0);
  }
  if (pred(x) == 0) {
    return call_clo(c, 1);
  }
  int64_t arg1 = callcc(cc1, x);
  int64_t arg2 = callcc(cc2, x);
  return addc(arg1, arg2, c);
}

int main() {
  int foobar;
  GC_expand_hp(1000000000);
  stacktop = &foobar;
  // iter count 10
  for(int64_t i = 0; i < 10; i++) {
    clo c = {thunk, NULL};
    int64_t res = fibc(30, &c);
    printf("Res %li\n", res);
  }
  return 0;
}
