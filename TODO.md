
* aarch64 failling:
  * Go back to shadow stack
  * tailcc won't work, we can't garantee how much shit is on stack in stubs.
  * we could remove noopt in stubs using lookaside stack anyway.
  * Maybe go back to shadow arg stack? ret imm16 is kinda slow on zen3.
    * and args are in backwards order, necessitating shift.
  * removes asm need for rest args & apply.

* benchmarks:
  * Size class fail: string fft compiler array1
  * need input/output: wc tail sum1 slatex read1 ray parsing dynamic cat
  * /: simplex scheme 
  * truncate: quicksort
  * exact-integer-sqrt: pi
  * complex: mbrotZ
  * slow records: gcbench
  * stack size check: earley, divrec
  * floor: chudnovsky
  * bignums: pi chudnovsky
  * equal

* r4rstest
  * input/output
  * large apply - aarch64
  * bignum

* fast globals

* figure out strategy for intrinsics????
* cleanup runtime
* inliner?? 

* self-compile
* fix loop-in-loop
* outpout/input buffering
* gc fd's.
* gset check
* bignums, compnums, ratnums

* recheck GC get stack top
* gc generational-lazy marking

* DONE self-tagging: experiment with better double tagging

* DONE GC: non-moving GC. bitmaps, slab alloc.  
  * DONE Passes free ranges for fast alloc via bump.
	* Needs to use partial slabs.
	* Need large runs, use bins (not a tree)

* call/cc: figure out valgrind or address santizer workings?
