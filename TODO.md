# CLEANUP

* website? package? manpage?

----------------------------------------
# POST-first-release

* get a scm formatter - s7? chez?

# known bugs

* We don't check for modification of constant values
  (Well, they're in the .rodata section, so you get a SEGV. Install a segv handler).

# library
* Some of the math routines don't support complex numbers correctly 
* Ports need more cleanup: support input from binary ports, more error checking,


# global analysis perf improvements :
  * faster with single-shot continuations: ctak fibc
  * faster with float type: fft fibfp mbrot pnpoly simplex sumfp. some might need inlining?
  * faster with list/int typecheck removal: quicksort primes puzzle array1
  * faster with custom ratnum (non libgmp): gcbench
  * faster with buffer management directly: tail 
     * read-line (vs. repeated read-char)
     * Some sort of simd looking for newline?
  * faster with custom inliner: graphs (and probably others due to fewer closure allocs),
      * read-char write-char won't inline without PGO
	  * We also have to rebuild all of the r7rs lib because we don't have cross-lib inlining.
  * faster with chains of recurances: sum / sumfp.  Can be O(1)
  * ports can combine input/textual field
    * or even better: port type can just have bits for input/textual for single-type check.

# CLEANUP

* cleanup library paths (instead of usage of -I or -A, use full path)
  * can remove junk extra paths from bc.scm and eval.scm
* figure out strategy for cleaner intrinsics????
  * we can auto-gen the header for llvm (excepting like SCM_CALL_CC that need extra flags)
  * what we've got is fine, gen-libraries can remove the old intrinsics
* cleanup runtime - move as much to scm as possible?
  * especially moving the number routine slowpaths would be cleaner?
* environments/eval
  * check if necessary to rename intrinsics? Works in other eval somehow
     * I think this is caused by needing to 'expand' symbols inserted by 
	   syntax-case, and by the macro serializer.

# PERF
* Needs inliner to remove alloc: graphs
  * simple called-once: Do it based on libraries?
* auto-listify globals: consargs stub in compiler called a lot: 
  * vector. Hand-coded in chez
* better control of inlining, read-char/peek-char should be inlined fastpath.
* S_error should have noreturn
* fixup writing w-out going through write-char if possible?: doesn't seem to affect perf much.

* fast globals, a.la chez

# gc
  * cleanup vector sizing: just use another header for large similar to small logbits?
    * large could always use markbits, but it doesn't show up in any tests currently.
  * And do same thing for static symbols?

-------------
# Super long term projects:
	 
* eval() could go through all the passes, and only run an interpreter after - would probably be 
  a 2-4x speedboost.

* gc close fd's, guardians or something

* Could do precise GC, using LLVM's stackmaps + non-integral pointer types. 
     This will probably break some optimizations based on type: Would need
	 a typecheck-removal pass
	 
* The whole expander needs to be re-written a-la chez with wrappers instead.
  * syntax-case is halfassed and doesn't work sometimes (especially library paths)

* call/cc
  * call/cc: figure out valgrind or address santizer workings?

* Object code in the heap / jit
 * heap dump/reload.
 * Probably significantly improves bootup time, but needs custom linker.
 * Probably also requires precise GC GC
