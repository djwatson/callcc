# FAQ

* What's the goal? Why another scheme compiler?

  The goal of callcc is to see how fast we can run on the r7rs
  benchmarks, with a minimum of code.  Chez is great, but it is 10x
  the amount of code here
  
  We're targeting full r7rs-small compliance, with full safety
  (seatbelts-on).  There is no 'unsafe' mode (although there are
  probably lots of bugs).

* Why is it called 'callcc'?

  Supporting call/cc seems to be the hardest part of writing a
  performant scheme compiler - and it's the only thing written in ASM
  in the codebase.
  
* Why LLVM IR instead of C?

  The output is already in SSA form, so it's easier out output IR,
  instead of having to translate out.  Unlike gambit or other C
  compilers, we don't do any sort of 'register' allocation, leaving it
  all up to LLVM.
  
* What additional LLVM work could be done?

  A pass to find GC roots could be done, a la Julia.  An exact GC is
  totally possible using non-integral pointer types in LLVM: However
  we'd probably loose some type analysis that currently occurs with
  integral pointer types, that would have to be re-written in scheme
  (i.e. analyzing a fixnum is 000-tag can't happen if the ptr type
  isn't integral).
  
  Instead of a shadow stack, if we had a custom calling convention in
  LLVM, we could do more efficent args beyohnd the default amount
  passed in registers.
  
  I can't find a way to get rid of gc_alloc calls, even if they are
  unused - normally LLVM will remove malloc/free calls, but this
  doesn't seem to work on custom alloc functions.
  
  Some fields in heap-allocated values are immutable - the type, the
  length, the record type for records, etc.  Currently LLVM doesn't
  know these are immutable, and constantly re-checks them if there is
  an intervening call.

* What interesting facts have you discovered about individual
  benchmarks?
  
   array1: It allocs 8MB arrays and frees them - many GCs un-mmap
these back to the OS, which is slow.

   bv2string/string: This is much faster using utf8 strings
natively, it can be vectorized effectively.

   cpstak: LLVM tries to vectorize the closure creation, which for
some reason is slower on my machine.

   deriv: The test run is  too short, the (hide) overhead dominates.

   gcbench: Ironically not a great GC test, tests records more
effectively.

   graphs: There's tons of closures that can be inlined here, making
this both an inlining and GC test.

   sum/sumfp: Using chains of recurrances / scalar evolution pass,
these are both O(1) (LLVM has like 20k lines of code for this,
however it has to be done in scheme number types here, there is a
custom pass for this).

   mbrot: This and some of the other inexact benchmarks could be
  faster with a pass to lower to doubles.
  
   earley/paraffins: These are the only benchmarks that seem to show
  much GC fragmentation.
  
   earley: using posix_memalign doesn't work great on linux's libc,
  it wastes lots of memory.  A custom mmap() allocation function
  reduces memory usage 
  
   compiler: The only benchmark with a global that is set! to a new
  lambda function.  
  
   ctak/fibc: Only oneshot continuations are needed, a custom pass
  analyzes if we can change call/cc to call/cc-oneshot.
