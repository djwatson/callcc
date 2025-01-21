
* r4rstest
  * fix string->symbol
    * also need to GC symbols
  * input/output
  * large apply
    * should also fix inexact?
  * bignum

* Maybe go back to shadow arg stack? ret imm16 is kinda slow on zen3.
  * and args are in backwards order, necessitating shift.
* fast globals
* program-ify
* better letrec
* program-ify letrec????

* figure out strategy for intrinsics????
  * can generate declare's.  Prefix all with SCM_
* cleanup runtime
* inliner?? 
* apply - real apply

* r4rs, r5rs*
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
