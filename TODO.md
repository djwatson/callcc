# known bugs

* make-string with negative numbers gives 'const data' errors. (it should really check if alloc succeeded?)
  and not allow neg numbers.
* writing to open-output-string with write-char is super slow if there is unicode
* nan.0 results in non-eqv values if add, sub, etc.

* call/cc multiple value return is broken:
  (call-with-values (lambda ()  (call-with-current-continuation (lambda (arg) (arg '() '())))) (lambda (a b) (display a) (display b) (newline)))
* We don't check for modification of constant values in the interpreter (only the compiler)
* (load) paths seem incorrect

## library
* Some of the math routines don't support complex numbers correctly 
* Ports need more cleanup: support input from binary ports, more error checking,

# PERF
  * faster with full inliner: 
      * read-char write-char won't inline without PGO
	  * We also have to rebuild all of the r7rs lib because we don't have cross-lib inlining.
  * faster with float type: fft fibfp mbrot pnpoly simplex sumfp. some might need inlining?
  * faster with list/int typecheck removal: quicksort primes puzzle array1
  * ports can combine input/textual field
    * or even better: port type can just have bits for input/textual for single-type check.
  * true multiple-return-values
    * deriv: call-with-values overhead is high
	* there is a hacky pass for this for now.
  * we're unable to remove record checks - a type-removal pass, and more work around records
    as intriniscs.
  * we don't currently inline constant global vars that are non-functions
    * causes a bunch of extra loads and undef-checks, especially for records (the record type
	  is a global var)
    * this affects cat,tail,wc (because ports used records), and gcbench.
  * auto-listify globals: consargs stub in compiler called a lot: 
    * vector. Hand-coded in chez
  * better control of inlining, read-char/peek-char should be inlined fastpath.
  * S_error should have noreturn
  * fixup writing w-out going through write-char if possible?: doesn't seem to affect perf much.

  * fast globals, a.la chez

# CLEANUP

* get a scm formatter - s7? chez?
* cleanup library paths (instead of usage of -I or -A, use full path)
  * can remove junk extra paths from bc.scm and eval.scm
* figure out strategy for cleaner intrinsics - cleanup gen-libraries
* cleanup runtime - move as much to scm as possible?
  * especially moving the number routine slowpaths would be cleaner?
* environments/eval
  * check if necessary to rename intrinsics? Works in other eval somehow
     * I think this is caused by needing to 'expand' symbols inserted by 
	   syntax-case, and by the macro serializer.

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
