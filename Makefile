# callcc requires clang/llvm.
CC = clang
CFLAGS = -g  -std=gnu23 -Wall
#CFLAGS = -flto -O3 -std=gnu23 -Wall
LIBS = -lm -lgmp -lutf8proc
SRCS = alloc_table.c gc.c types.c list.c callcc.S
OBJECTS = $(patsubst %, %.o, $(basename $(SRCS)))
SCM_SRCS = lib/runtime2.scm lib/eval.scm lib/read.scm lib/equal.scm lib/hashtable.scm lib/str2num.scm lib/bc.callcc.scm lib/bc.scm lib/expand.scm lib/fix-letrec.scm lib/library-manager.scm lib/match.scm lib/memory_layout.scm lib/passes.scm lib/qq.scm lib/sua.scm lib/util.scm lib/gen-libraries.scm
SRFI_SRCS = lib/srfi2/srfi/*.scm

all: callcc

callcc: lib/bc.callcc.ll libcallcc.a
	${CC} ${CFLAGS} -o $@ lib/bc.callcc.ll libcallcc.a ${LIBS}

libcallcc.a: ${OBJECTS}
	ar rcs $@ $^

lib/headers:
	cd lib; mkdir -p headers/flow; mkdir -p headers/scheme; gosh -I. gen-libraries.scm

lib/bc.callcc.ll: ${SCM_SRCS} lib/headers ${SRFI_SRCS}
	cd lib; gosh -I. bc.gauche.scm bc.callcc.scm > bc.callcc.ll

clean:
	rm -rf callcc lib/headers lib/bc.callcc.cc *.o libcallcc.a

cloc:
	cloc --by-file-by-lang ${SRCS} ${SCM_SRCS}

TESTS = $(addsuffix .test, $(basename $(wildcard test/*.scm))) $(addsuffix .test, $(basename $(wildcard test/bench/*.scm)))

.PHONY: test %.test

%.test : %.scm callcc
	cd test; bash cmp.sh ../$<

test: $(TESTS)

