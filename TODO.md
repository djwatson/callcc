# known bugs:

# plan
  * records need record check
    * can remove port? check in places
	* needs to be fast-ish somehow.
	* record? bits should be in type header?

# full r7rs / safety / tests
  * port tests
  * copyish tests
  
# GC stuff:

* Cleanup the GC roots, some should be in types, add_root unused currently.
* Remove the radix tree: just use pre-allocated virtual space.

* cleanup vector sizing: just use another header for large similar to small logbits?
  * large could always use markbits, but it doesn't show up in any tests currently.
* And do same thing for static symbols?

# benchmarks / perf improvements:
  * faster with single-shot continuations: ctak fibc
  * faster with float type: fft fibfp mbrot pnpoly simplex sumfp. some might need inlining?
  * faster with list/int typecheck removal: quicksort primes puzzle array1

# CLEANUP

* lint all C
* get a scm formatter
* scm lint as much as possible

----------------------------------------


# Long term:	 

# CLEANUP

* cleanup library paths (instead of usage of -I or -A, use full path)
  * can remove junk extra paths from bc.scm and eval.scm
* gc fd's.
* figure out strategy for intrinsics????
  * we can auto-gen the header for llvm (excepting like SCM_CALL_CC that need extra flags)
  * what we've got is fine, gen-libraries can remove the old intrinsics
* cleanup runtime - move as much to scm as possible?
  * especially moving the number routine slowpaths would be cleaner?
* environments/eval
  * check if necessary to rename intrinsics? Works in other eval somehow
     * I think this is caused by needing to 'expand' symbols inserted by 
	   syntax-case, and by the macro serializer.
  * The whole expander needs to be re-written a-la chez with wrappers instead.

# PERF
* Needs inliner to remove alloc: graphs
  * simple called-once: Do it based on libraries?
* auto-listify globals: consargs stub in compiler called a lot: 
  * vector. Hand-coded in chez
* better control of inlining, read-char/peek-char should be inlined fastpath.
* S_error should have noreturn
* fixup writing w-out going through write-char if possible?: doesn't seem to affect perf much.
* gcbench: ratnums are too slow.  Doh.  Hand-roll ratnums?

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
