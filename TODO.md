
* benchmarks:
  * GC needs defrag/generational: graphs
  * also needs inliner to remove alloc: graphs
  
  * need input/output: wc tail sum1 slatex read1 ray parsing dynamic cat
  
  * /: simplex scheme 
  * truncate: quicksort
  * complex: mbrotZ
  
  * slow records: gcbench
  * stack size check: earley, divrec

  * exact-integer-sqrt: pi
  * floor: chudnovsky
  * bignums: pi chudnovsky

  * equal
  
* cpstak:
  * global linking doesn't work quite right
  * argcnt needs a register, ugh.  Too slow otherwise.
* ack: register usage is whack

* r4rstest
  * input/output
  * bignum

* fast globals

* figure out strategy for intrinsics????
* cleanup runtime
* inliner?? appears to be unnecessary!!!! yay

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
