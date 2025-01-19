
* fixup r4rs case selection: need to load cases in letrec for all before codegen

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

* DONE self-tagging: experiment with better double tagging

* DONE GC: non-moving GC. bitmaps, slab alloc.  
  * DONE Passes free ranges for fast alloc via bump.
	* Needs to use partial slabs.
	* Need large runs, use bins (not a tree)

* call/cc: figure out valgrind or address santizer workings?
