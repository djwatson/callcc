# known bugs:
  * stacktop still broken, smashes environ, but if set to environ-1, breaks other things.

# plan
  * Get all abort() to S_error() instead in types.c
     * need checking for records
  * regain lost perf
	* better control of inlining, read-char/peek-char should be inlined & fast.
	* error should have noreturn
	
	* fixup writing w-out going through write-char if possible?
	* gcbench: ratnums are too slow.  Doh.  Hand-roll ratnums?
  * install - fix dirs

	 
# full r7rs / safety / tests
  * r7rs test - asin, acos - basically all the arithmetic should work on ratnum and compnum too.
  
  Mostly error paths:
  * argtype stress test
  * macro tests - work!
  * port tests
  * copyish tests
  * gset check
  
# GC stuff:
* recheck GC get stack top - fixme
  * environ is getting smashed by call/cc

* gc fd's.
* cleanup vector sizing: just use another header for large similar to small logbits?
  * large could always use markbits, but it doesn't show up in any tests currently.
* And do same thing for static symbols?
* Cleanup the GC roots, some should be in types, add_root unused currently.
* Remove the radix tree: just use pre-allocated virtual space.
* cleanup strdata gc_log

# benchmarks / perf improvements:
  * Needs inliner to remove alloc: graphs
    * simple called-once: Do it based on libraries?
  * faster with float type: fft fibfp mbrot pnpoly simplex sumfp
  * faster with list/int typecheck removal: quicksort primes puzzle array1
  * faster with single-shot continuations: ctak fibc
  * storage use analysis: unboxing flonums, longjmp/setjmp call/cc, and typecheck removal.
    * call/cc can just be a simple escape analysis? global though
  
  * stack size check: earley, divrec
  * auto-listify globals: consargs stub in compiler called a lot: 
    * vector. Hand-coded in chez



----------------------------------------


# PROBABLY NEVER:	 

# CLEANUP

* srfi69 is already builtin

* figure out strategy for intrinsics????
* cleanup runtime - move as much to scm as possible
* environments/eval
  * check if necessary to rename intrinsics? Works in other eval somehow
     * I think this is caused by needing to 'expand' symbols inserted by 
	   syntax-case, and by the macro serializer.

* cpstak:
  * argcnt needs a register somehow?
  
# PERF

* fast globals, a.la chez

* Long term: Could do precise GC, using LLVM's stackmaps + non-integral pointer types. 
     This will probably break some optimizations based on type: Would need
	 a typecheck-removal pass
	 
# call/cc
  * call/cc: figure out valgrind or address santizer workings?

# Object code in the heap / jit
 * heap dump/reload.
 * Probably significantly improves bootup time, but needs custom linker.
 * Probably also requires precise GC GC
