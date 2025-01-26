* benchmarks:
  * GC needs defrag/generational: graphs, earley, gcbench
  * also needs inliner to remove alloc: graphs
  
  * need input/output: wc tail sum1 slatex read1 ray parsing dynamic cat scheme
  
  * complex: mbrotZ
  
  * stack size check: earley, divrec

  * exact-integer-sqrt: pi
  * bignums: pi chudnovsky
  
* faster symbol table using hashtable

* cpstak:
  * argcnt needs a register, ugh.  Too slow otherwise.

* r4rstest
  * bignum

* fast globals

* figure out strategy for intrinsics????
* cleanup runtime
* inliner?? graphs is slow without

* self-compile
* outpout/input buffering
* gc fd's.
* gset check
* bignums, compnums, ratnums

* recheck GC get stack top
* gc generational-lazy marking

* call/cc: figure out valgrind or address santizer workings?
