
* fibc, ctak, gcbench? 

* symbols can use name in ll
* macro-ify more of the generated code.

* check for undefined globals
* check for bad closure
* fast globals
* program-ify
* better letrec
* program-ify letrec????

* figure out strategy for intrinsics????
* cleanup runtime
* call/cc
* inliner?? 

* r4rs, r5rs*
* self-compile
* fix loop-in-loop
* outpout/input buffering
* gc fd's.
* gset check
* argcnt check
* varargs
* case-lambda
* bignums, compnums, ratnums

* DONE self-tagging: experiment with better double tagging

* DONE GC: non-moving GC. bitmaps, slab alloc.  
  * DONE Passes free ranges for fast alloc via bump.
	* Needs to use partial slabs.
	* Need large runs, use bins (not a tree)

* call/cc: figure out valgrind or address santizer workings?
