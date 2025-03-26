# callcc requires clang/llvm.
CC = clang
CFLAGS = -O3 -flto=full -DNDEBUG -g  -std=gnu23 -Wall  -march=native -mtune=native -ffat-lto-objects -Wextra -Wnull-dereference  -Wshadow -Wno-unused-parameter
LIBS = -lm -lgmp -lutf8proc
SRCS = c/alloc_table.c c/gc.c c/runtime.c c/util/list.c c/util/bitset.c c/callcc.S
OBJECTS = $(patsubst %, %.o, $(basename $(SRCS)))
SCM_LIB_SRCS = lib/runtime.scm lib/eval.scm lib/read.scm lib/equal.scm lib/hashtable.scm lib/str2num.scm  
SCM_COMPILER_SRCS = compiler/bc.scm compiler/expand.scm compiler/fix-letrec.scm compiler/library-manager.scm compiler/match.scm compiler/memory_layout.scm compiler/passes.scm compiler/qq.scm compiler/sua.scm compiler/util.scm compiler/gen-libraries.scm compiler/callcc.scm
SCM_SRCS = ${SCM_LIB_SRCS} ${SCM_COMPILER_SRCS}
SRFI_SRCS = lib/srfi2/srfi/*.scm
PREFIX ?= /usr

all: bin/callcc

bin/callcc: compiler/callcc.scm.ll libcallcc.a
	mkdir -p bin
	${CC} ${CFLAGS} -o $@ compiler/callcc.scm.ll libcallcc.a ${LIBS}

libcallcc.a: ${OBJECTS}
	ar rcs $@ $^

compiler/headers:
	cd compiler; mkdir -p headers/flow; mkdir -p headers/scheme; gosh -I. gen-libraries.scm

compiler/callcc.scm.ll: ${SCM_SRCS} compiler/headers ${SRFI_SRCS}
# Fake install dir location, only used for bootstrap.
	rm -f lib/callcc
	ln -s .. lib/callcc 
	cd compiler; gosh -I. callcc.scm --exe callcc.scm 

clean:
	rm -rf bin/callcc compiler/headers  c/*.o libcallcc.a

cloc:
	cloc --by-file ${SRCS} 
	cloc --by-file ${SCM_COMPILER_SRCS} 
	cloc --by-file ${SCM_LIB_SRCS} 
	cloc --by-file-by-lang ${SRCS} ${SCM_SRCS} 

TESTS = $(addsuffix .test, $(basename $(wildcard test/*.scm))) \
	$(addsuffix .test, $(basename $(wildcard test/bench/*.scm))) \
	$(addsuffix .test, $(basename $(wildcard test/macros/*.scm)))

.PHONY: test %.test

%.test : %.scm bin/callcc
	cd test; bash cmp.sh ../$<

test: $(TESTS)

format:
	clang-format -i c/*.c c/*.h

tidy:
	clang-tidy --extra-arg="-std=gnu23" c/*.c c/util/*.c

install: bin/callcc
	install -d ${PREFIX}/bin
	install bin/callcc ${PREFIX}/bin/
	install -d ${PREFIX}/lib/callcc/lib
	install -d ${PREFIX}/lib/callcc/compiler
	install libcallcc.a ${PREFIX}/lib/callcc/
	install -m644 compiler/bc.* ${PREFIX}/lib/callcc/compiler
	install -m644 compiler/passes.* ${PREFIX}/lib/callcc/compiler
	install -m644 compiler/fix-letrec.scm ${PREFIX}/lib/callcc/compiler
	install -m644 compiler/qq.scm ${PREFIX}/lib/callcc/compiler
	install -m644 compiler/sua.scm ${PREFIX}/lib/callcc/compiler
	install -m644 lib/runtime2.scm ${PREFIX}/lib/callcc/lib
	install -m644 lib/eval.scm ${PREFIX}/lib/callcc/lib
	install -m644 compiler/memory_layout.scm ${PREFIX}/lib/callcc/compiler
	install -m644 lib/str2num.scm ${PREFIX}/lib/callcc/lib
	install -m644 lib/read.scm ${PREFIX}/lib/callcc/lib
	install -m644 lib/hashtable.scm ${PREFIX}/lib/callcc/lib
	install -m644 lib/equal.scm ${PREFIX}/lib/callcc/lib
	install -m644 compiler/expand.* ${PREFIX}/lib/callcc/compiler
	install -m644 compiler/library-manager.* ${PREFIX}/lib/callcc/compiler
	install -m644 compiler/format.* ${PREFIX}/lib/callcc/compiler
	install -m644 compiler/util.* ${PREFIX}/lib/callcc/compiler
	install -m644 compiler/match.* ${PREFIX}/lib/callcc/compiler
	install -m644 compiler/stdlib.scm ${PREFIX}/lib/callcc/compiler
	install -d ${PREFIX}/lib/callcc/compiler/headers/flow
	install -m644 compiler/headers/flow/* ${PREFIX}/lib/callcc/compiler/headers/flow
	install -d ${PREFIX}/lib/callcc/compiler/headers/scheme
	install -m644 compiler/headers/scheme/* ${PREFIX}/lib/callcc/compiler/headers/scheme
	install -d ${PREFIX}/lib/callcc/lib/srfi
	install -m644 lib/srfi2/srfi/* ${PREFIX}/lib/callcc/lib/srfi

uninstall:
	rm -f ${PREFIX}/bin/callcc
	rm -rf ${PREFIX}/lib/callcc
