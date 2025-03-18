# CALLCC 

copyright Dave Watson 2025

A r7rs scheme compiler with an LLVM backend.

# Requirements

* libgmp
* libutf8proc
* modern clang, tested on version 19.
* A bootstrap scheme: Currently, guache is recommended
* Normal build machinery, like make, install, etc.

# Build

Assuming you have the build requirements, simply:

```
$ make 
```

Will produce a working executable.

# Install

```
$ PREFIX=/usr sudo make install
```

# Test

```
$ make test -j12 -k
```

# Usage

Basic usage for the interpreter is to use -s or --script to run a scm file:

```
$ ./callcc -s hello_world.scm
$ cat hello_world.scm
(display "Hello World!\n")
```

Usage for the compiler is via --exe, and optionally -o:

```
$ ./callcc --exe hello_world.scm
$ ./hello_world
```

# Performance

Currently, the standard scm library is rebuilt for every exe.

For quicker builds, if you do not use eval, use -fno-eval.
This trims the executable size and build time substantially.

For maximum performance, the following C flags are recommended:
```
$ ./callcc -fno-eval --cc "-O3 -flto" --exe whatever.scm
```

For even greater performance, PGO (profile guided optimization) can be
used.  For example:

```
$ ./callcc -fno-eval --cc "-O3 -flto -fprofile-generate" --exe wc.scm
$ LLVM_PROFILE_FILE=a.profraw ./wc
$ llvm-profdata merge -output=a.profdata a.profraw
$ ./callcc -fno-eval --cc "-O3 -flto -fprofile-use=a.profdata" --exe wc.scm
$ time ./wc
```

# Platforms

* aarch64/linux, aarch64/termux
* x86_64/linux

There are ~50 lines of asm required for each platform, entirely for
fast call/cc support.  Everything else is relatively portable C/posix
or r7rs scheme.

The LLVM backend uses a few semi-portable flags, including 'musttail',
'preserve_none', 'preserve_all'.

Currently only x86_64 has been benchmarked for performance.

# Notes:

Output is only flushed after newlines, so be sure to put a newline
after the last display.

Currently FD's are closed automatically, close-input/output-port must
be called.


