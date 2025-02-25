# callcc requires clang/llvm.
CC = clang
CFLAGS = -flto -O3 -g  -std=gnu23 -Wall -DNDEBUG -march=native -mtune=native -ffat-lto-objects -Wextra -Wnull-dereference  -Wshadow -Wno-unused-parameter
#CFLAGS = -flto -O3 -std=gnu23 -Wall
LIBS = -lm -lgmp -lutf8proc
SRCS = alloc_table.c gc.c types.c list.c callcc.S
OBJECTS = $(patsubst %, %.o, $(basename $(SRCS)))
SCM_SRCS = lib/runtime2.scm lib/eval.scm lib/read.scm lib/equal.scm lib/hashtable.scm lib/str2num.scm lib/bc.callcc.scm lib/bc.scm lib/expand.scm lib/fix-letrec.scm lib/library-manager.scm lib/match.scm lib/memory_layout.scm lib/passes.scm lib/qq.scm lib/sua.scm lib/util.scm lib/gen-libraries.scm callcc.scm
SRFI_SRCS = lib/srfi2/srfi/*.scm
PREFIX ?= /usr

all: callcc

callcc: callcc.scm.ll libcallcc.a
	${CC} ${CFLAGS} -o $@ callcc.scm.ll libcallcc.a ${LIBS}

libcallcc.a: ${OBJECTS}
	ar rcs $@ $^

lib/headers:
	cd lib; mkdir -p headers/flow; mkdir -p headers/scheme; gosh -I. gen-libraries.scm

callcc.scm.ll: ${SCM_SRCS} lib/headers ${SRFI_SRCS}
	gosh -Ilib callcc.scm --exe callcc.scm -I lib/headers -Ilib

clean:
	rm -rf callcc lib/headers lib/bc.callcc.cc *.o libcallcc.a

cloc:
	cloc --by-file-by-lang ${SRCS} ${SCM_SRCS}

TESTS = $(addsuffix .test, $(basename $(wildcard test/*.scm))) $(addsuffix .test, $(basename $(wildcard test/bench/*.scm)))

.PHONY: test %.test

%.test : %.scm callcc
	cd test; bash cmp.sh ../$<

test: $(TESTS)

format:
	clang-format -i *.c

install: callcc
	install -d ${PREFIX}/bin
	install callcc ${PREFIX}/bin/
	install -d ${PREFIX}/bin/lib
	install libcallcc.a ${PREFIX}/bin/lib
	install lib/runtime2.scm ${PREFIX}/bin/lib
	install lib/eval.scm ${PREFIX}/bin/lib
	install lib/memory_layout.scm ${PREFIX}/bin/lib
	install lib/str2num.scm ${PREFIX}/bin/lib
	install lib/read.scm ${PREFIX}/bin/lib
	install lib/hashtable.scm ${PREFIX}/bin/lib
	install lib/equal.scm ${PREFIX}/bin/lib
	install lib/expand.* ${PREFIX}/bin/lib
	install lib/library-manager.* ${PREFIX}/bin/lib
	install lib/format.* ${PREFIX}/bin/lib
	install lib/util.* ${PREFIX}/bin/lib
	install lib/match.* ${PREFIX}/bin/lib
	install lib/stdlib.scm ${PREFIX}/bin/lib
	install libcallcc.a ${PREFIX}/bin/
	install -d ${PREFIX}/bin/lib/headers/flow
	install lib/headers/flow/* ${PREFIX}/bin/lib/headers/flow
	install -d ${PREFIX}/bin/lib/headers/scheme
	install lib/headers/scheme/* ${PREFIX}/bin/lib/headers/scheme
	install -d ${PREFIX}/bin/lib/srfi
	install lib/srfi2/srfi/* ${PREFIX}/bin/lib/srfi
	install -d ${PREFIX}/bin/lib/third-party
	install lib/third-party/* ${PREFIX}/bin/lib/third-party
