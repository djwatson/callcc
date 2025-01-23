
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
