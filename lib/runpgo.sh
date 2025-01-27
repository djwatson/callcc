#!/bin/bash

FILE=$(basename $1 .scm)
gosh -I. bc.gauche.scm $1 > test.ll 2>/dev/null && clang   -O3 -DNDEBUG -flto  -g  -std=gnu23 -o $FILE test.ll ../types.c ../gc.c ../list.c ../alloc_table.c -lm -mcrc32 -msse4.2 -march=native -lxxhash -fprofile-generate || echo "FAIL $FILE"
LLVM_PROFILE_FILE=a.profraw ./$FILE
llvm-profdata merge -output=a.profdata a.profraw
clang -DNDEBUG -flto -O3 -g  -o $FILE  -std=gnu23  test.ll ../types.c ../gc.c ../list.c ../alloc_table.c   -lm  -mcrc32 -msse4.2 -march=native -lxxhash -fprofile-use=a.profdata

/usr/bin/time -v ./$FILE
