#!/bin/bash

FILE=$(basename $1 .scm)
gosh -I. bc.gauche.scm $1 > test.ll 2>/dev/null

clang   -O3 -DNDEBUG -flto  -g  -std=gnu23 -o $FILE test.ll ../types.c ../gc.c ../list.c ../alloc_table.c -lm
time ./$FILE
