# CALL/CC

call/cc implementation strategy is probably the most important for a
scheme implementation, especially for performance.  There is an
excellent overview of modern techniques:

From Folklore to Fact:
Comparing Implementations of Stacks and
Continuations
Kavon Farvardin, John Reppy

call/cc's strategy is most similar to the 'contiguous' strategy: We
always use the C stack.  When a continuation is captured, it is copied
to the heap: but then we *reset* the stack to empty and call the
thunk.  The heap stack segments therefore form a linked-list of
segments (one segment for each call/cc call). When the thunk returns
(or the continuation is called), the underflow handler is hit, and we
copy a single segment back to the stack.

This means if the stack is captured multiple times from the same
continuation, we only ever copy it once.  Or if call/cc is called from
several locations, we only copy each individual segment one time.

It also means we are only ever running with the stack in *the same
location* it was when it ran the first time: it never moves out from
under the compiler.  This means for example, frame pointers work just
fine without any additional compiler complexity. 

The downside is that there is always at least two copies, vs. chez's
single copy.  So some call/cc intensive benchmarks may run twice as
slow.

preserve_none is used to force spilling of callee-save registers
before saving the continuation.

# Single-shot CALL/CC

However, if the continuation never escapes, and it is single shot we
can do better: just treat it the same as setjmp/longjmp, and don't
copy the stack at all.

# Tail-calls, rest-arguments, apply

LLVM's 'musttail' is utiliezed to guarantee tail-calls.  To support
rest lists and apply, a lookaside stack is used:  Any arguments over 6
(or 8 on ARM: selected by default to be those args passed in
registers) are passed on the lookaside stack.  Code to interact with
apply & rest are therefore pretty straightforward. 

The downside is that LLVM doesn't natively know about the lookaside
stack, so calls with 6+ args may be shuffling code between the regular
stack and lookaside stack frequently. 

On the upside, we don't have to do any register allocation or stack
management in scheme.

# Dynamic type tagging

Pretty standard low-3-bit tagging is used.

Floats are tagged, as in the paper:

"Float Self-Tagging" Olivier Melançon, Manuel Serrano, Marc Feeley

This means most inexact operations are 3~4 times faster than chez, or
other boxed-by-default float implementations, while mantaining full
double accuracy.

# GC

The GC is conservative on the stack and heap, and non-moving.  This
vastly simplifies the C code, and works easily with an LLVM backend
without extra work. 

The GC *only* roots a tiny handful of things (vs. for example BDWGC,
that roots all of the static data area): The stack, the global
variables, the lookaside stack, and complex static data constants.

It has a sticky-mark-bit mode, for a simple generational GC.

By default the nursury is 50MB, and we do a full GC every 8
collections. There are some heuristics to detect if we need a full GC
more often.

For small size classes, we allocate in slabs: i.e. each slab holds
only a single size: we can easily detect interior pointers this way.

preserve_none is used to dump callee-save registers before
conservatively walking the stack, so no ASM is necessary in the GC at all.

# C-scheme integration

Currently calls to runtime functions just pass gc_obj tagged-types
directly. using LTO (-flto), LLVM is able to inline this all as much
as it decides. 

We force inlining and slow/fastpaths by utilizing noinline, musttail,
and preserve_all in places (and looking at the generated code).

Even then, LLVM isn't as willing to inline as much as some schemes
are.  PGO is quite effective at inlining the remaining paths:
-fprofile-generate and -fprofile-use.

# Unicode

Strings are all represented as UTF8.  Strings store both the codepoint
count, and byte count.  If they are equal, most routines have
fastpaths that operate only on ASCII-chars. 

Therefore string-ref/string-set! are O(1) for ascii, and O(n) for full
unicode.

string->utf8 and utf8->string are pretty trivial then: The first is
just a byte copy, and the second needs to verify the bytevector is
utf8 and count codepoints, but is then just a copy also.

Strings *do* countain an additional indirection, in case the allocated
size needs to change (we string-set! an ascii char with a larger
codepoint, for example).

An optimization here (not yet implemented) could be to refcount and
COW the backing store, making string->utf8, substring, ... O(1)
operations.

libutf8proc currently provides the database for upper/lowercase/etc
codepoint information.

# Bignums

libgmp is used for bignums and ratnums currently. 

Ratnums are a bit slow, and in fact most schemes use custom bignum
implementations: most code is not limited by the speed of the bignum
implementation itself, but maybe only that expt/exact-integer-sqrt are
fast on bignums also. 

# Compiler

The compiler consists of several passes:

* generic expander, in expand.scm and library-manager.scm.
  The gen-libraries.scm generates the normal scheme headers, and
  default macros.
  
  The expander emits directly-executable scheme code for easy testing.
  
* passes.scm: A poor man's nanopass compiler implementing many
  standard passes:
  
  * lifing of complex constants
  * inlining simple builtins
  * fixing letrec (reloaded)
  * assignment conversion
  * recovering let
  * loop recovery
  * naming of lambdas
  * closure conversion 
  * 'programify': assume unset-globals will not be modified, and can
    be directly referenced.
	
Finally, LLVM code is emitted in bc.scm, which is linked with the C
runtime.

Currently the whole scheme standard library must be built along with
the user program.

# REPL

A simple repl is implemented by expanding the user code, and running
the expanded code directly.  No closure conversion or other passes are
done, so it is quite slow.

macros are run via repl, so macro usage can also be slow.
