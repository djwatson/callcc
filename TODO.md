* preserve_none seems to be smashing rbp. Doh.

# benchmarks:
  * Needs inliner to remove alloc: graphs
  
  * complex: mbrotZ
  
  * stack size check: earley, divrec

  * exact-integer-sqrt: pi
  * bignums: pi chudnovsky

# GC stuff:
  * GC faster marklarge
  * Maybe add type tag to vec/cons, and use a bit to indicate 'young' for logging.
    * Mostly matters for vector?
    * use field-logging?  Or even simple RC?
	
* storage use analysis: unboxing flonums, longjmp/setjmp call/cc, and typecheck removal.
   * call/cc can just be a simple escape analysis? global though


* debug info in llvm: would help perf record --call-graph=dwarf
* auto-listify globals: consargs stub in compiler called a lot: 
  * vector. Hand-coded in chez
  
* cpstak:
  * argcnt needs a register, ugh.  Too slow otherwise.

* r4rstest
  * bignum

* fast globals

* figure out strategy for intrinsics????
* cleanup runtime
* inliner?? graphs is slow without
  * can be simple called-once??? check graphs

* self-compile
  * eval / environment
  * display, including write-flonum to string
      * which could mean bignums, etc......
* gc fd's.
* gset check
* bignums, compnums, ratnums

* recheck GC get stack top

* call/cc: figure out valgrind or address santizer workings?
