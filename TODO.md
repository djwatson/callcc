
* failing:
  * nucleic.  something to do with flonum math?
  * pnpoly - incorrect result
  * lattice - not a list
  
  * need: read, /, truncate (quicksort?)
  * string: abort - large size class

* r4rstest
  * input/output
  * large apply - aarch64
  * bignum

* Maybe go back to shadow arg stack? ret imm16 is kinda slow on zen3.
  * and args are in backwards order, necessitating shift.
* fast globals

* figure out strategy for intrinsics????
* cleanup runtime
* inliner?? 
* apply - real apply

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
