# known bugs:

# plan
  * mkdir bin/install
  * system(), get full compile working with driver
  * run full benchmarks before unicode testing.
  * get r7rs-tests working
    * unicode
	* bytevectors
	* input/output
	* read syntax
	* numeric syntax
	* exceptions
	* environments/eval

	 
# full r7rs
  * unicode
  * r7rs test
  * argtype stress test
  * division tests
  * macro tests
  * port tests
  * unicode-tests
  * copyish tests
  
# benchmarks:
  * Needs inliner to remove alloc: graphs
  * faster with float type: fft fibfp mbrot pnpoly simplex sumfp
  * faster with list/int typecheck removal: quicksort primes puzzle array1
  * faster with single-shot continuations: ctak fibc
  
  * stack size check: earley, divrec

# OTHER

* storage use analysis: unboxing flonums, longjmp/setjmp call/cc, and typecheck removal.
   * call/cc can just be a simple escape analysis? global though

* auto-listify globals: consargs stub in compiler called a lot: 
  * vector. Hand-coded in chez
  
* cpstak:
  * argcnt needs a register, ugh.  Too slow otherwise.

* figure out strategy for intrinsics????
* cleanup runtime
* inliner?? graphs is slow without
  * can be simple called-once??? check graphs

* gc fd's.
* gset check

* recheck GC get stack top

# GC stuff:
* cleanup vector sizing: just use another header for large similar to small logbits?
  * large could always use markbits, but it doesn't show up in any tests currently.
* And do same thing for static symbols?
* Cleanup the GC roots, some should be in types, add_root unused currently.
* Remove the radix tree: just use pre-allocated virtual space.






# PROBABLY NEVER:	 

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
