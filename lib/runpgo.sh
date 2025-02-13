#!/bin/bash

FILE=$(basename $1 .scm)
gosh -I. bc.gauche.scm $1 > test.ll 2>/dev/null && clang   -O3 -DNDEBUG -flto  -g  -std=gnu23 -o $FILE test.ll ../types.c ../gc.c ../list.c ../alloc_table.c ../callcc.S -lm -march=native -fno-plt -ffast-math -funroll-loops -fvectorize -mtune=native -fprofile-generate || echo "FAIL $FILE"
LLVM_PROFILE_FILE=a.profraw ./$FILE
llvm-profdata merge -output=a.profdata a.profraw
clang -Wl,-q -DNDEBUG -flto -O3 -g  -o $FILE  -std=gnu23  test.ll ../types.c ../gc.c ../list.c ../alloc_table.c ../callcc.S   -lm  -march=native -fprofile-use=a.profdata -fno-plt -ffast-math -funroll-loops -fvectorize -mtune=native

/usr/bin/time -v ./$FILE
