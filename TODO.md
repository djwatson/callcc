# known bugs:

# plan
  * Options: -fno-eval, -fno-eval-macros
  * get r7rs-tests working
	* environments/eval
	   * fix 'environment', boot order, check paths work
	   * check if necessary to rename intrinsics? Works in other eval somehow
  * Get all abort() to S_error() instead in types.c
  * regain lost perf
    * bv2string - bytevector copy, utf8proc_iterate, strcmp, bytevector_set
	* cat, dynamic, read1, slatex, sum1, tail, wc - read
	* parsing, -- ??
	* fixup writing w-out going through write-char if possible?
	* equal - hash table junk - move to case-lambda, check eqhash and strhash are working.
               resize and use and instead of modulo.
	* maybe just pgo: conform??, cpstak??, gcbench, lattice, pnpoly, puzzle, quicksort, ray, scheme, sum, takl
  * install - fix dirs

	 
# full r7rs / safety / tests
  * r7rs test - symbol names, file-error, asin, acos, denominator, inexact, eofports
  
  Mostly error paths:
  * argtype stress test
  * macro tests - work!
  * port tests
  * copyish tests
  * gset check
  
# CLEANUP

* figure out strategy for intrinsics????
* cleanup runtime - move as much to scm as possible

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

# GC stuff:
* recheck GC get stack top

* gc fd's.
* cleanup vector sizing: just use another header for large similar to small logbits?
  * large could always use markbits, but it doesn't show up in any tests currently.
* And do same thing for static symbols?
* Cleanup the GC roots, some should be in types, add_root unused currently.
* Remove the radix tree: just use pre-allocated virtual space.
* cleanup strdata gc_log



----------------------------------------


# PROBABLY NEVER:	 

* cpstak:
  * argcnt needs a register somehow?

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
