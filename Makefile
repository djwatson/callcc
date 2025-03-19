# callcc requires clang/llvm.
CC = clang
CFLAGS = -O3 -flto=full -DNDEBUG -g  -std=gnu23 -Wall  -march=native -mtune=native -ffat-lto-objects -Wextra -Wnull-dereference  -Wshadow -Wno-unused-parameter -I./c
LIBS = -lm -lgmp -lutf8proc
SRCS = c/gc/alloc_table.c c/gc/gc.c c/runtime.c c/util/list.c c/callcc.S
OBJECTS = $(patsubst %, %.o, $(basename $(SRCS)))
SCM_LIB_SRCS = lib/runtime2.scm lib/eval.scm lib/read.scm lib/equal.scm lib/hashtable.scm lib/str2num.scm  
SCM_COMPILER_SRCS = compiler/bc.scm compiler/expand.scm compiler/fix-letrec.scm compiler/library-manager.scm compiler/match.scm compiler/memory_layout.scm compiler/passes.scm compiler/qq.scm compiler/sua.scm compiler/util.scm compiler/gen-libraries.scm callcc.scm
SCM_SRCS = ${SCM_LIB_SRCS} ${SCM_COMPILER_SRCS}
SRFI_SRCS = lib/srfi2/srfi/*.scm
PREFIX ?= /usr

all: callcc

callcc: callcc.scm.ll libcallcc.a
	${CC} ${CFLAGS} -o $@ callcc.scm.ll libcallcc.a ${LIBS}

libcallcc.a: ${OBJECTS}
	ar rcs $@ $^

compiler/headers:
	cd compiler; mkdir -p headers/flow; mkdir -p headers/scheme; gosh -I. gen-libraries.scm

callcc.scm.ll: ${SCM_SRCS} compiler/headers ${SRFI_SRCS}
	gosh -Ilib -Icompiler callcc.scm --exe callcc.scm -I compiler/headers -Ilib -Icompiler

clean:
	rm -rf callcc compiler/headers  c/*.o c/gc/*.o libcallcc.a

cloc:
	cloc --by-file ${SRCS} 
	cloc --by-file ${SCM_COMPILER_SRCS} 
	cloc --by-file ${SCM_LIB_SRCS} 
	cloc --by-file-by-lang ${SRCS} ${SCM_SRCS} 

TESTS = $(addsuffix .test, $(basename $(wildcard test/*.scm))) \
	$(addsuffix .test, $(basename $(wildcard test/bench/*.scm))) \
	$(addsuffix .test, $(basename $(wildcard test/macros/*.scm)))

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
	install -d ${PREFIX}/bin/compiler
	install libcallcc.a ${PREFIX}/bin/lib
	install compiler/bc.* ${PREFIX}/bin/compiler
	install compiler/passes.* ${PREFIX}/bin/compiler
	install compiler/fix-letrec.scm ${PREFIX}/bin/compiler
	install compiler/qq.scm ${PREFIX}/bin/compiler
	install compiler/sua.scm ${PREFIX}/bin/compiler
	install lib/runtime2.scm ${PREFIX}/bin/lib
	install lib/eval.scm ${PREFIX}/bin/lib
	install compiler/memory_layout.scm ${PREFIX}/bin/compiler
	install lib/str2num.scm ${PREFIX}/bin/lib
	install lib/read.scm ${PREFIX}/bin/lib
	install lib/hashtable.scm ${PREFIX}/bin/lib
	install lib/equal.scm ${PREFIX}/bin/lib
	install compiler/expand.* ${PREFIX}/bin/compiler
	install compiler/library-manager.* ${PREFIX}/bin/compiler
	install compiler/format.* ${PREFIX}/bin/compiler
	install compiler/util.* ${PREFIX}/bin/compiler
	install compiler/match.* ${PREFIX}/bin/compiler
	install compiler/stdlib.scm ${PREFIX}/bin/compiler
	install libcallcc.a ${PREFIX}/bin/
	install -d ${PREFIX}/bin/compiler/headers/flow
	install compiler/headers/flow/* ${PREFIX}/bin/compiler/headers/flow
	install -d ${PREFIX}/bin/compiler/headers/scheme
	install compiler/headers/scheme/* ${PREFIX}/bin/compiler/headers/scheme
	install -d ${PREFIX}/bin/lib/srfi
	install lib/srfi2/srfi/* ${PREFIX}/bin/lib/srfi
