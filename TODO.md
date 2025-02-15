
# known bugs:
  * gcroot constant inits locs

# plan
  * get bignum/ratnum/compnum constants lifted / working
    * test pi/chudnovsky
  * get ratnum/compnum working
  * moar self-compile
     * get-double-as-u64
  
# benchmarks:
  * Needs inliner to remove alloc: graphs
  * faster with float type: fft fibfp mbrot pnpoly simplex sumfp
  * faster with list/int typecheck removal: quicksort primes puzzle array1
  * faster with single-shot continuations: ctak fibc
  
  * complex: mbrotZ
  
  * stack size check: earley, divrec

  * exact-integer-sqrt: pi
  * bignums: pi chudnovsky

# GC stuff:
* cleanup vector sizing: just use another header for large similar to small logbits?
  * large could always use markbits, but it doesn't show up in any tests currently.
* And do same thing for static symbols?
* Cleanup the GC roots, some should be in types, add_root unused currently.
* Remove the radix tree: just use pre-allocated virtual space.

* Long term: Could do precise GC, using LLVM's stackmaps + non-integral pointer types. 
     This will probably break some optimizations based on type: Would need
	 a typecheck-removal pass

# OTHER

* storage use analysis: unboxing flonums, longjmp/setjmp call/cc, and typecheck removal.
   * call/cc can just be a simple escape analysis? global though


* auto-listify globals: consargs stub in compiler called a lot: 
  * vector. Hand-coded in chez
  
* cpstak:
  * argcnt needs a register, ugh.  Too slow otherwise.

* fast globals

* figure out strategy for intrinsics????
* cleanup runtime
* inliner?? graphs is slow without
  * can be simple called-once??? check graphs

* self-compile
* gc fd's.
* gset check
* bignums, compnums, ratnums

* recheck GC get stack top

# call/cc
  * call/cc: figure out valgrind or address santizer workings?

# Object code in the heap / jit
 * heap dump/reload.
 * Probably significantly improves bootup time, but needs custom linker.
 * Probably also requires precise GC GC
