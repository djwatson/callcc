#!/bin/bash

FILE=$(basename $1 .scm)
gosh -I. bc.gauche.scm $1 > test.ll 2>/dev/null && clang   -O3  -flto  -g  -std=gnu23 -o $FILE test.ll ../types.c ../gc.c ../list.c ../alloc_table.c -lm -mcrc32 -msse4.2 -march=native -lxxhash || echo "FAIL $FILE" && /usr/bin/time -v ./$FILE 
