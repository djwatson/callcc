#!/bin/bash

FILE=$(basename $1 .scm)
gosh -I. bc.gauche.scm $1 > test.ll 2>/dev/null

clang -DNDEBUG -flto -O3 -g  -std=gnu23 -o $FILE test.ll ../types.c ../gc.c ../list.c ../alloc_table.c -lm
