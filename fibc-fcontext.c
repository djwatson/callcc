#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <gc.h>
#include <assert.h>

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
  struct ccsave*prev_link;
} ccsave;

void* stacktop;

ccsave* cur_link = NULL;
static int64_t ccresthunk(void* unused, int64_t n) {
  ccsave* c = (ccsave*)unused;
  cur_link = c->prev_link;

  register int64_t stack_bottom asm("rdi") = (int64_t)stacktop - c->sz;
  register void* saved_stack asm ("rsi") = c->stack;
  register size_t saved_sz asm ("rcx") = c->sz;
  register size_t res asm ("rax") = n;

   asm volatile (
          	 "mov %2, %%rsp\n\t" // Restore the old stack pointer.
		 "rep movsb\n\t" // Copy the old stack over (copy rsi -> rdi, rcx times)
		 "pop %%rbp\n\t" // Pop the old frame pointer.
		 "ret\n\t" // Return to caller of 'callcc'.  Res already in rax.
		: // output
		 : "r" (saved_stack), "r" (saved_sz), "r" (stack_bottom), "r" (res)//input
		: "rsp", "rbp", "memory"// clobbers
		);
   __builtin_unreachable();
   return n;
}

void need_more_frames() {
  int64_t res;
  // This is called as a return point.  In x86_64, the return is in rax.
  asm volatile("mov %%rax, %0\n\t": "=r"(res));
  assert(cur_link);
  ccresthunk(cur_link, res);
}
// TODO 1) need framelink 2) need frame pointer

uint64_t memuse = 0;
void* my_malloc(size_t sz) {
  memuse -= sz;
  return (void*)memuse;
}

__attribute__((returns_twice, noinline, preserve_none)) int64_t callcc(ccthunk t, int64_t x) {
  ccsave* stack = my_malloc(sizeof(ccsave));

  void* stack_bottom = __builtin_frame_address(0);
  size_t stack_sz = stacktop - stack_bottom;
  //printf("Stack size %li\n", stack_sz);
  stack->stack = my_malloc(stack_sz);
  memcpy(stack->stack, stack_bottom, stack_sz);
  stack->sz = stack_sz;

  stack->ptr = ccresthunk;
  stack->clo_data = stack;
  stack->prev_link = cur_link;

  cur_link = stack;
  asm volatile ("mov %0, %%rsp\n\t" // Reset stack pointer to stacktop
		"mov $0, %%rbp\n\t" // Clear out the frame pointer (TODO also reset it in need_more_frames)
		"push %1\n\t" // Push return address of need_more_frames
		"mov %2, %%rdi\n\t" // Set up call thunk
		"mov %3, %%rsi\n\t" // and call argument
		"jmp *%4\n\t" // Jump to thunk.
		: // output
		: "r" (stacktop), "r"(need_more_frames), "r"(stack), "r"(x), "r"(t)// input
		: "rdi", "rsi", "rsp", "rbp", "memory"// clobbers
		);

  return t((clo*)stack, x);
}

static int64_t addc(int64_t x, int64_t y, clo* k) {
  if (y == 0) {
    //return x;
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
    //return 0;
    return call_clo(c, 0);
  }
  if (pred(x) == 0) {
    //return 1;
    return call_clo(c, 1);
  }
  int64_t arg1 = callcc(cc1, x);
  int64_t arg2 = callcc(cc2, x);
  return addc(arg1, arg2, c);
}

int main() {
  uint64_t foobar;
  //GC_expand_hp(50000000);
  stacktop = (void*)((uint64_t)(&foobar) & -15);
  size_t memsize = 400000000;
  void* mem = malloc(memsize);
  // iter count 10
  for(int64_t i = 0; i < 10; i++) {
    memuse = (uint64_t)mem + memsize;
    clo c = {thunk, NULL};
    int64_t res = fibc(30, &c);
    printf("Res %li\n", res);
  }
  return 0;
}
